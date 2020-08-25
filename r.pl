% R on demand
:- module(r, [ r_init/0, rod/2, op(150, xfx, ...) ]).

:- set_prolog_flag(float_overflow, infinity).

:- use_module(library(r/r_call)).
r_call:r_console_property(size(25, 80)).

% Use prolog for evaluation
%
% Predefined hooks and custom functions
:- multifile pl_hook/2.

:- discontiguous pl/2.
pl(Expr, Res) :-
    pl_hook(Expr, Hook),
    !,
    pl(Hook, Res).

% Handle lists
pl([], Res) :-
    !,
    Res = [].

pl([H | T], Res) :-
    maplist(pl, [H | T], Res).

% Switch to R
pl(rr(Expr), Res) :-
    !,
    rr(Expr, Res).

pl(r(Expr), Res) :-
    !,
    r(Expr, Res).

%
% Interval type
%
interval(_ ... _).

%
% Hickey Figure 1
%
mixed(L ... U) :-
    L < 0,
    U > 0.

zero(0.0 ... 0.0).

positive(L ... U) :-
    L >= 0,
    U > 0.

zeropos(0.0 ... U) :-
    U > 0.

strictpos(L ... _) :-
    L > 0.

negative(L ... U) :-
    L < 0,
    U =< 0.

zeroneg(L ... 0.0) :-
    L < 0.

strictneg(_ ... U) :-
    U < 0.

% Things such as pi
pl(Expr, Res) :-
    atom(Expr),
    !,
    Res is Expr.

pl(Expr, Res) :-
    string(Expr),
    !,
    Res = Expr.

pl(Expr, Res) :-
    number(Expr),
    !,
    Res is Expr.

pl(X, Res) :-
    interval(X),
    !,
    Res = X.

pl(interval(A ... B), Res) :-
    !,
    interval(A ... B, Res).

%
% Convert to interval
%
:- discontiguous interval/2.
interval(X, B ... B) :-
    atomic(X),
    pl(X, B).

% compatible with atoms like pi
interval(A ... B, L ... U) :-
    pl(A, L),
    pl(B, U).

:- discontiguous example/0.
example :-
    writeln(type-1),
    interval(1.0, X),
    writeln(X).

example :-
    writeln(type-2),
    interval(0.99 ... 1.01, X),
    writeln(X).

%
% Equality
%
pl(X == Y, Res) :-
    pl(X, ResX),
    number(ResX),
    pl(Y, ResY),
    number(ResY),
    !,
    ResX =@= ResY,
    Res = ResX.

pl(X == Y, L ... U) :-
    pl(X, ResX),
    pl(Y, ResY),
    (   interval(ResX) ;
        interval(ResY)
    ),
    !,
    interval(ResX, A ... B),
    interval(ResY, C ... D),
    eval_interval(A ... B == C ... D, L ... U).

eval_interval(A ... B == C ... D, L ... U) :-
    L is max(A, C),
    U is min(B, D),
    L =< U.

example :-
    writeln(equality-1),
    interval(1.00 ... 1.02, X),
    interval(1.01 ... 1.0Inf, Y),
    ( pl(X == Y, Z)
        -> writeln(X == Y --> Z)
         ; writeln(X == Y --> fails)
    ).

example :-
    writeln(equality-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.04, Y),
    ( pl(X == Y, Z)
        -> writeln(X == Y --> Z)
         ; writeln(X == Y --> fails)
    ).

%
% Simple arithmetics
%
pl(A + B, Res) :-
    pl(A, ResA),
    number(ResA),
    pl(B, ResB),
    number(ResB),
    !,
    Res is ResA + ResB.

pl(A + B, Res) :-
    !,
    pl(A, ResA),
    pl(B, ResB),
    interval(ResA, IntA),
    interval(ResB, IntB),
    eval_interval(IntA + IntB, Res).

%
% Hickey, Theorem 4
%
:- discontiguous eval_interval/2.
eval_interval(A ... B + C ... D, L ... U) :-
    L is A + C,
    U is B + D.

example :-
    writeln(sum-1),
    interval(1.00 ... 1.02, X),
    interval(1.01 ... 1.0Inf, Y),
    pl(X + Y, Z),
    writeln(X + Y --> Z).

example :-
    writeln(sum-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.04, Y),
    pl(X + Y, Z),
    writeln(X + Y --> Z).

%
% Difference
%
pl(A - B, Res) :-
    pl(A, ResA),
    number(ResA),
    pl(B, ResB),
    number(ResB),
    !,
    Res is ResA - ResB.

pl(A - B, Res) :-
    !,
    pl(A, ResA),
    pl(B, ResB),
    interval(ResA, IntA),
    interval(ResB, IntB),
    eval_interval(IntA - IntB, Res).

%
% Hickey, Theorem 4
%
eval_interval(A ... B - C ... D, L ... U) :-
    L is A - D,
    U is B - C.

example :-
    writeln(diff-1),
    interval(0.99 ... 1.01, X),
    interval(0.99 ... 1.01, Y),
    pl(X - Y, Z),
    writeln(X - Y --> Z).

example :-
    writeln(diff-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.0Inf, Y),
    pl(X - Y, Z),
    writeln(X - Y --> Z).

%
% Hickey Theorem 6 and Figure 3
%
pl(A * B, Res) :-
    pl(A, ResA),
    number(ResA),
    pl(B, ResB),
    number(ResB),
    !,
    Res is ResA * ResB.

pl(A * B, Res) :-
    !,
    pl(A, ResA),
    pl(B, ResB),
    interval(ResA, IntA),
    interval(ResB, IntB),
    eval_interval(IntA * IntB, Res).

%
% Hickey, Figure 3 (last rows first)
%
eval_interval(A ... B * C ... D, L ... U) :-
    once(zero(A ... B); zero(C ... D)),
    !,
    L is 0.0,
    U is 0.0.

% P * P
eval_interval(A ... B * C ... D, L ... U) :-
    positive(A ... B),
    positive(C ... D),
    !,
    L is A * C,
    U is B * D.

% P * M
eval_interval(A ... B * C ... D, L ... U) :-
    positive(A ... B),
    mixed(C ... D),
    !,
    L is B * C,
    U is B * D.

% P * N
eval_interval(A ... B * C ... D, L ... U) :-
    positive(A ... B),
    negative(C ... D),
    !,
    L is B * C,
    U is A * D.

% M * P
eval_interval(A ... B * C ... D, L ... U) :-
    mixed(A ... B),
    positive(C ... D),
    !,
    L is A * D,
    U is B * D.

% M * M
eval_interval(A ... B * C ... D, L ... U) :-
    mixed(A ... B),
    mixed(C ... D),
    !,
    L is min(A * D, B * C),
    U is max(A * C, B * D).

% M * N
eval_interval(A ... B * C ... D, L ... U) :-
    mixed(A ... B),
    negative(C ... D),
    !,
    L is B * C,
    U is A * C.

% N * P
eval_interval(A ... B * C ... D, L ... U) :-
    negative(A ... B),
    positive(C ... D),
    !,
    L is A * D,
    U is B * C.

% N * M
eval_interval(A ... B * C ... D, L ... U) :-
    negative(A ... B),
    mixed(C ... D),
    !,
    L is A * D,
    U is A * C.

% N * N
eval_interval(A ... B * C ... D, L ... U) :-
    negative(A ... B),
    negative(C ... D),
    !,
    L is B * D,
    U is A * C.

example :-
    writeln(example-4.2-1),
    interval(pi ... pi, Y),
    pl(2 * Y, Z),
    writeln(2 * Y --> Z).

example :-
    writeln(example-4.2-2),
    interval(0.0 ... 0.0, X),
    interval(-1.0Inf ... 1.0Inf, Y),
    pl(X * Y, Z),
    writeln(X * Y --> Z).

%
% Hickey Theorem 8 and Figure 4
%
pl(A / B, Res) :-
    pl(A, ResA),
    number(ResA),
    pl(B, ResB),
    number(ResB),
    !,
    Res is ResA / ResB.

pl(A / B, Res) :-
    !,
    pl(A, ResA),
    pl(B, ResB),
    interval(ResA, IntA),
    interval(ResB, IntB),
    eval_interval(IntA / IntB, Res).

% P1 / P
eval_interval(A ... B / 0.0 ... D, L ... U) :-
    strictpos(A ... B),
    positive(0.0 ... D),
    !,
    L is A / D,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    strictpos(A ... B),
    positive(C ... D),
    !,
    L is A / D,
    U is B / C.

% P0 / P
eval_interval(A ... B / 0.0 ... D, L ... U) :-
    zeropos(A ... B),
    positive(0.0 ... D),
    !,
    L is 0.0,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    zeropos(A ... B),
    positive(C ... D),
    !,
    L is 0.0,
    U is B / C.

% M / P
eval_interval(A ... B / 0.0 ... D, L ... U) :-
    mixed(A ... B),
    positive(0.0 ... D),
    !,
    L is -1.0Inf,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    mixed(A ... B),
    positive(C ... D),
    !,
    L is A / C,
    U is B / C.

% N0 / P
eval_interval(A ... B / 0.0 ... D, L ... U) :-
    zeroneg(A ... B),
    positive(0.0 ... D),
    !,
    L is -1.0Inf,
    U is 0.0.

eval_interval(A ... B / C ... D, L ... U) :-
    zeroneg(A ... B),
    positive(C ... D),
    !,
    L is A / C,
    U is 0.0.

% N1 / P
eval_interval(A ... B / 0.0 ... D, L ... U) :-
    strictneg(A ... B),
    positive(0.0 ... D),
    !,
    L is -1.0Inf,
    U is B / D.

eval_interval(A ... B / C ... D, L ... U) :-
    strictneg(A ... B),
    positive(C ... D),
    !,
    L is A / C,
    U is B / D.

% P1 / M
eval_interval(A ... B / C ... D, L ... U) :-
    strictpos(A ... B),
    mixed(C ... D),
    L is -1.0Inf,
    U is A / C.

eval_interval(A ... B / C ... D, L ... U) :-
    strictpos(A ... B),
    mixed(C ... D),
    !,
    L is A / D,
    U is 1.0Inf.

% P0 / M
eval_interval(A ... B / C ... D, L ... U) :-
    zeropos(A ... B),
    mixed(C ... D),
    !,
    L is -1.0Inf,
    U is 1.0Inf.

% M / M
eval_interval(A ... B / C ... D, L ... U) :-
    mixed(A ... B),
    mixed(C ... D),
    !,
    L is -1.0Inf,
    U is 1.0Inf.

% N0 / M
eval_interval(A ... B / C ... D, L ... U) :-
    zeroneg(A ... B),
    mixed(C ... D),
    !,
    L is -1.0Inf,
    U is 1.0Inf.

% N1 / M
eval_interval(A ... B / C ... D, L ... U) :-
    strictneg(A ... B),
    mixed(C ... D),
    L is -1.0Inf,
    U is B / D.

eval_interval(A ... B / C ... D, L ... U) :-
    strictneg(A ... B),
    mixed(C ... D),
    !,
    L is B / C,
    U is 1.0Inf.

% P1 / N
eval_interval(A ... B / C ... 0.0, L ... U) :-
    strictpos(A ... B),
    negative(C ... 0.0),
    !,
    L is -1.0Inf,
    U is A / C.

eval_interval(A ... B / C ... D, L ... U) :-
    strictpos(A ... B),
    negative(C ... D),
    !,
    L is B / D,
    U is A / C.

% P0 / N
eval_interval(A ... B / C ... 0.0, L ... U) :-
    zeropos(A ... B),
    negative(C ... 0.0),
    !,
    L is -1.0Inf,
    U is 0.0.

eval_interval(A ... B / C ... D, L ... U) :-
    zeropos(A ... B),
    negative(C ... D),
    !,
    L is B / D,
    U is 0.0.

% M / N
eval_interval(A ... B / C ... 0.0, L ... U) :-
    mixed(A ... B),
    negative(C ... 0.0),
    !,
    L is -1.0Inf,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    mixed(A ... B),
    negative(C ... D),
    !,
    L is B / D,
    U is A / D.

% N0 / N
eval_interval(A ... B / C ... 0.0, L ... U) :-
    zeroneg(A ... B),
    negative(C ... 0.0),
    !,
    L is 0,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    zeroneg(A ... B),
    negative(C ... D),
    !,
    L is 0.0,
    U is A / D.

% N1 / N
eval_interval(A ... B / C ... 0.0, L ... U) :-
    strictneg(A ... B),
    negative(C ... 0.0),
    !,
    L is B / C,
    U is 1.0Inf.

eval_interval(A ... B / C ... D, L ... U) :-
    strictneg(A ... B),
    negative(C ... D),
    !,
    L is B / C,
    U is A / D.

example :-
    writeln(page-3-(-inf ... 0, 1...inf)),
    interval(1.00 ... 1.00, X),
    interval(-1.0Inf ... 1.0, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-3-(0 ... inf)),
    interval(0.00 ... 1.00, X),
    interval(0.00 ... 1.00, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-4-(r-without-0)),
    interval(1.00 ... 1.00, X),
    interval(-1.0Inf ... 1.0Inf, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-5-(-inf ... -1, 1 ... inf)),
    interval(1.0 ... 1.0, X),
    interval(-1.0 ... 1.0, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-6-(-inf ... -1, +0 ... inf)),
    interval(1.0 ... 1.0, X),
    interval(-1.0 ... 1.0Inf, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-8-(-inf ... -0, 1 ... inf)),
    interval(1.0 ... 1.0, X),
    interval(-1.0Inf ... 1.0, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-a-(0 ... inf)),
    interval(0.0 ... 1.0, X),
    interval(0.0 ... 2.0, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-b-(0 ... 1)),
    interval(1.0 ... 1.0, X),
    interval(1.0 ... 1.0Inf, Y),
    pl(X / Y, Z),
    writeln(X / Y --> Z).

%
% Fraction
%
pl(frac(X, Y), Z) :-
    !,
    pl(X / Y, Z).

example :-
    writeln(frac),
    interval(2.0 ... 2.0, X),
    pl(1 + frac(1, X), Z),
    writeln(1 + frac(1, X) --> Z).

%
% Square root. This declaration should be dropped because it
% only applies the sqrt function to the two bounds of the
% interval, which is the default.
%
pl(sqrt(X), Res) :-
    pl(X, R),
    number(R),
    !,
    Res is sqrt(R).

pl(sqrt(X), Res) :-
    pl(X, R),
    !,
    eval_interval(sqrt(R), Res).

eval_interval(sqrt(A ... B), L ... U) :-
    once(zero(A ... B) ; positive(A ... B)),
    !,
    L is sqrt(A),
    U is sqrt(B).

example :-
    writeln(sqrt-1),
    interval(2.0 ... 2.0, X),
    pl(1 + sqrt(X), Z),
    writeln(1 + sqrt(X) --> Z).

%
% Round to D decimals
%
pl(dec(X, D), Res) :-
    pl(X, R),
    number(R),
    !,
    interval(R, Int),
    pl(dec(Int, D), Res).

pl(dec(X, D), Res) :-
    pl(X, A ... B),
    !,
    L is floor(10^D * A) / 10^D,
    U is ceiling(10^D * B) / 10^D,
    Res = L ... U.

example :-
    writeln(dec2-1),
    pl(dec(sqrt(2), 2), Z),
    writeln(dec(sqrt(2), 2) --> Z).

example :-
    writeln(dec2-2),
    pl(dec(sqrt(2.1 ... 2.2), 2), Z),
    writeln(dec(sqrt(2.1 ... 2.2), 2) --> Z).

%
% General functions that do not account for intervals.
%
% Examples
% * interval(sin(0.1 ... 0.2), X)
% * interval(sin(0.1), X)
%
% Evaluate interval for the arguments first, and then (blindly) apply
% the function to the lower and upper bound. Obviously, this only
% works for functions that are monotonous in all their arguments.
%
bound(A ... _, A).

bound(_ ... B, B) :-
    !.

bound(X, X).

pl(X, Res) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    maplist(pl, Arguments, Results),
    findall(R, ( maplist(bound, Results, Bounds),
                 compound_name_arguments(New, Name, Bounds),
                 R is New
               ), List),
    min_member(L, List),
    max_member(U, List),
    ( length(List, 1)
       -> [Res] = List
       ;  Res = L ... U
    ).

example :-
    writeln(sin-1),
    pl(sin(0.1), Z),
    writeln(sin(0.1) --> Z).

example :-
    writeln(sin-1),
    pl(sin(0.1 ... 0.2), Z),
    writeln(sin(0.1 ... 0.2) --> Z).

example :-
    writeln(power-generic),
    pl(2 ... 3 ^ (-(2 ... 3)), Z),
    writeln("2 ... 3 ^ (-(2 ... 3))" --> Z).
    
%
% t-test example
%
example :-
    writeln(ttest),
    interval(5.7 ... 5.8, D),
    interval(4.0 ... 4.0, Mu),
    interval(3.8 ... 3.8, S),
    interval(24.0 ... 24.0, N),
    pl(dec(frac(D - Mu, dec(S / dec(sqrt(N), 2), 2)), 2), T),
    writeln(t --> T).

% Custom functions
pl_hook(dfrac(Num, Den), Res) :-
    pl(Num / Den, Res).

pl_hook(left_landed(_, Expr), Res) :-
    pl(Expr, Res).

pl_hook(right_landed(_, Expr), Res) :-
    pl(Expr, Res).

pl_hook(left_elsewhere(_, Expr), Res) :-
    Expr =.. [_, _, R],
    pl(R, Res).

pl_hook(right_elsewhere(_, Expr), Res) :-
    Expr =.. [_, L, _],
    pl(L, Res).

pl_hook(instead_of(_, Expr, _), Res) :-
    pl(Expr, Res).

pl_hook(instead_of(_, Expr, _, _, _), Res) :-
    pl(Expr, Res).

pl_hook(omit_left(_, Expr), Res) :-
    Expr =.. [_, _, R],
    pl(R, Res).

pl_hook(omit_right(_, Expr), Res) :-
    Expr =.. [_, L, _],
    pl(L, Res).

pl_hook(skip(_, _, Expr), Res) :-
    pl(Expr, Res).

pl_hook(denoting(_, Expr, _), Res) :-
    pl(Expr, Res).

pl_hook(abbrev(_, Expr), Res) :-
    pl(Expr, Res).

pl_hook(protect(Expr), Res) :-
    pl(Expr, Res).

pl_hook(var_pool2(Var_A, N_A, Var_B, N_B), Res) :-
    pl(frac((N_A - 1) * Var_A + (N_B - 1) * Var_B, N_A + N_B - 2), Res).

pl_hook(zratio(Z), Res) :-
    pl(Z, ResZ),
    number(ResZ),
    ( abs(ResZ) >= 10
      -> format(string(Res), "z = ~1f", [ResZ])
       ; format(string(Res), "z = ~2f", [ResZ])
    ).

pl_hook(zratio(Z), Res) :-
    pl(Z, ResZ),
    interval(ResZ),
    interval(ResZ, L ... U),
    ( min(abs(L), abs(U)) >= 10
      -> format(string(Res), "z = ~1f ... ~1f", [L, U])
       ; format(string(Res), "z = ~2f ... ~2f", [L, U])
    ).

pl_hook(tratio(T, DF), Res) :-
    pl(T, ResT),
    number(ResT),
    pl(DF, ResDF),
    ( abs(ResT) >= 10 
      -> format(string(Res), "t(~0f) = ~1f", [ResDF, ResT])
       ; format(string(Res), "t(~0f) = ~2f", [ResDF, ResT])
    ). 

pl_hook(tratio(T, DF), Res) :-
    pl(T, ResT),
    interval(ResT),
    interval(ResT, L ... U),
    pl(DF, ResDF),
    ( min(abs(L), abs(U)) >= 10
      -> format(string(Res), "t(~0f) = ~1f ... ~1f", [ResDF, L, U])
       ; format(string(Res), "t(~0f) = ~2f ... ~2f", [ResDF, L, U])
    ).

pl_hook(chi2ratio(X2, DF), Res) :-
    pl(X2, ResX2),
    number(ResX2),
    pl(DF, ResDF),
    ( abs(ResX2) >= 10
      -> format(string(Res), "chi2(~0f) = ~1f", [ResDF, ResX2])
       ; format(string(Res), "chi2(~0f) = ~2f", [ResDF, ResX2])
    ).

pl_hook(chi2ratio(X2, DF), Res) :-
    pl(X2, ResX2),
    interval(ResX2),
    interval(ResX2, L ... U),
    pl(DF, ResDF),
    ( min(abs(L), abs(U)) >= 10
      -> format(string(Res), "chi2(~0f) = ~1f ... ~1f", [ResDF, L, U])
       ; format(string(Res), "chi2(~0f) = ~2f ... ~2f", [ResDF, L, U])
    ).

pl_hook(perc(P), Res) :-
    pl(perc(P, 3), Res).

pl_hook(perc(P, Digits), Res) :-
    pl(P, ResP),
    number(ResP),
    format(string(Mask), "~~~0ff%", max(0, Digits-2)),
    format(string(Res), Mask, ResP*100).

pl_hook(perc(P, Dig), Res) :-
    pl(P, ResP),
    interval(ResP),
    interval(ResP, L ... U),
    format(string(Mask), "~~~0ff% ... ~~~0ff%", [max(0, Dig-2), max(0, Dig-2)]),
    format(string(Res), Mask, [L*100, U*100]).
    
pl_hook(fratio(F), Res) :-
    r(fratio(pl(F)), Res).

pl_hook(prob(P), Res) :-
    r(prob(pl(P)), Res).

pl_hook(pvalue(P), Res) :-
    r(pvalue(pl(P)), Res).

pl_hook(natural(P), Res) :-
    r(natural(pl(P)), Res).

pl_hook(qt(P, DF), Res) :-
    r(qt(pl(P), pl(DF)), Res).

pl_hook(dbinom(K, Size, Prob), Res) :-
    r(dbinom(pl(K), pl(Size), pl(Prob)), Res).

pl_hook(pwbinom(C, Tail, Size, Prob), Res) :-
    r(pwbinom(pl(C), pl(Tail), pl(Size), pl(Prob)), Res).

pl_hook(uqbinom(Tail, Dist, Alpha, Size, Prob), Res) :-
    r(uqbinom(pl(Tail), pl(Dist), pl(Alpha), pl(Size), pl(Prob)), Res).

pl_hook(tail(Tail), Res) :-
    r(tail(pl(Tail)), Res).

pl_hook(dist(Dist), Res) :-
    r(dist(pl(Dist)), Res).

pl_hook(dist(Dist, K), Res) :-
    r(dist(pl(Dist), pl(K)), Res).

pl_hook(pnorm(Z), Res) :-
    r(pnorm(pl(Z)), Res).

pl_hook(unorm(Z), Res) :-
    r(unorm(pl(Z)), Res).

pl_hook(pm(X, PM), [P, M]) :-
    pl(X - PM, P),
    pl(X + PM, M).

pl_hook(confint(CI, D), Res) :-
    r(confint(pl(CI), D), Res).

pl_hook(anova_f(Model, Effect), Res) :-
    r('`[`'(anova(pl(Model)), Effect, "F value"), Res).

pl_hook(ancova_ffffff(D, Prim, Cov, Strata, Other, Int, Ex, Main), Res) :-
    rr(ancova_ffffff(D, Prim, Cov, Strata, Other, Int, Ex, Main), Res).

pl_hook(choose(N, K), Res) :-
    r(choose(N, K), Res).

pl_hook(square(X), Res) :-
    pl(X * X, Res).

% Use R for evaluation
%
% Use R for full expression
rr(Expr, Res) :-
    !,
    pl2r(Expr, Fix),
    R <- Fix,
    [Res] = R.

% Switch to Prolog
r(pl(Expr), Res) :-
    !,
    pl(Expr, Res).

% Handle intervals
r(A ... B, Res) :-
    !,
    r(A, ResA),
    r(B, ResB),
    L is min(ResA, ResB),
    U is max(ResA, ResB),
    Res = L ... U.

% Handle compounds
r(X, Res) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    maplist(r, Arguments, Results),
    findall(R, ( maplist(bound, Results, Bounds),
                 compound_name_arguments(New, Name, Bounds),
                 rr(New, R)
               ), List),
    min_member(L, List),
    max_member(U, List),
    ( length(List, 1)
       -> [Res] = List
       ;  Res = L ... U
    ).

r(Expr, Res) :-
    atomic(Expr),
    !,
    rr(Expr, Res).

% R on demand
rod(Expr, Res) :-
    pl(Expr, Res).

% Translate problematic prolog compounds to R
% Multifile hook for extensions
:- multifile pl2r_hook/2.
pl2r(P, R) :-
    pl2r_hook(P, R1),
    !,
    R = R1.

% Prolog lists <-> (flat) R vectors
pl2r([], 'NULL') :-
    !.

pl2r([H | T], R) :-
    !,
    maplist(pl2r, [H | T], List),
    compound_name_arguments(R, c, List).

pl2r(P, R) :-
    atomic(P),
    !,
    R = P.

pl2r(P, R) :-
    compound(P),
    compound_name_arguments(P, N, PArgs),
    maplist(pl2r, PArgs, RArgs),
    compound_name_arguments(R, N, RArgs).

% Some common functions
r_init :-
    {|r||
        left_landed <- function(err, expr)
        {
            return(expr)
        }

        right_landed <- function(err, expr)
        {
            return(expr)
        }

        left_elsewhere <- omit_left <- function(err, expr)
        {
            eval(substitute(expr)[[3]])
        }

        right_elsewhere <- omit_right <- function(err, expr)
        {
            eval(substitute(expr)[[2]])
        }

        instead_of1 <- function(err, wrong, error, correct, noerror)
        {
            return(wrong)
        }

        skip <- function(err, fn, expr)
        {
            return(expr)
        }

        denoting <- function(sym, expr, label)
        {
            return(expr)
        }

        abbrev <- function(sym, expr)
        {
            return(expr)
        }

        fratio <- function(f)
        {
            sprintf(ifelse(abs(f) >= 10, "%.1f", "%.2f"), f)
        }

        confint <- function(ci, digits=2)
        {
            mask = sprintf("%%.%if to %%.%if", digits, digits)
            sprintf(mask, ci[1], ci[2])
        }

        prob <- function(p, digits=3)
        {
            mask = sprintf("%%.%if", digits)
            sprintf(mask, p)
        }

        pvalue <- function(p, letter="p")
        {
            if(p < .001)
                return(sprintf("%s < .001", letter))

            if(p < .10)
                return(sprintf("%s = %.3f", letter, p))

            sprintf("%s = %.2f", letter, p)
        }
            
        natural <- function(n)
        {
            sprintf("%i", n)
        }

        pm <- function(a, b)
        {
            c(a - b, a + b)
        }

        protect <- identity

        unorm <- function(...)
        {
            pnorm(..., lower.tail=FALSE)
        }
    |}.

% Tests
example :-
    pl(1 + 1, Res),
    writeln(Res).

example :-
    pl(r(2 + 2), Res),
    writeln(Res).

example :-
    pl(rr(3 + 3), Res),
    writeln(Res).

example :-
    pl(4 + r(4 + 4), Res),
    writeln(Res).

example :-
    pl(r(5 + p(5 + 5)), Res),
    writeln(Res).

example :-
    pl(rr($('t.test'(c(1, 2, 3, 4, 5), mu=0), "p.value")), Res),
    writeln(Res).

example :-
    writeln("2.1...2.2 in Prolog"),
    interval(2.1 ... 2.2, X),
    pl(X + 1, Res),
    writeln(Res).

example :-
    writeln("2.1...2.2 in R"),
    interval(2.1 ... 2.2, X),
    pl(1 + r(X + 1), Res),
    writeln(Res).

example :-
    writeln(r-power-generic),
    pl(r(2 ... 3 ^ (-(2 ... 3))), Z),
    writeln("r(2 ... 3 ^ (-(2 ... 3)))" --> Z).

