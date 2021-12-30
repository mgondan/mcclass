% Interval arithmetic in Prolog
:- module(interval, [ interval/3, op(150, xfx, ...) ]).

:- use_module(r).
:- use_module(session).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- discontiguous int/3.
:- discontiguous interval/2.
:- discontiguous example/0.

:- multifile r_hook/1, hook/4.

% Determine result using integer arithmetic
interval(Flags, Expr, Res) :-
    int([engine(pl) | Flags], Expr, Res).

% Allows for external definitions
int(Flags, Expr, Res),
    option(engine(pl), Flags),
    r_hook(Expr)
 => int([engine(r) | Flags], Expr, Res).

int(Flags, Expr, Res),
    hook(Flags, Expr, New, Hook)
 => int(New, Hook, Res).

% Some standard R functions and own definitions. These will be moved elsewhere.
hook(Flags, choose(N, K), Flags, r(choose(N, K))) :-
    option(engine(pl), Flags).

hook(Flags, instead(_Bug, Wrong, _Correct), Flags, Wrong).

hook(Flags, instead(_Bug, Wrong, _Correct0, _Correct), Flags, Wrong).

hook(Flags, omit_right(_Bug, Expr), Flags, Left) :-
    Expr =.. [_Op, Left, _Right].

hook(Flags, omit_left(_Bug, Expr), Flags, Right) :-
    Expr =.. [_Op, _Left, Right].

hook(Flags, drop_right(_Bug, Expr), Flags, Left) :-
    Expr =.. [_Op, Left, _Right].

hook(Flags, drop_left(_Bug, Expr), Flags, Right) :-
    Expr =.. [_Op, _Left, Right].

hook(Flags, invent_left(_Bug, Expr), Flags, Expr).

hook(Flags, invent_right(_Bug, Expr), Flags, Expr).

hook(Flags, abbrev(_Sym, Expr, _Text), Flags, Expr).

hook(Flags, color(_Col, Expr), Flags, Expr).

hook(Flags, '<-'(Var, Expr), Flags, r('<-'(Var, pl(Expr)))) :-
    option(engine(pl), Flags).

hook(Flags, ';'(Expr1, Expr2), Flags, Res) :-
    int(Flags, Expr1, _),
    int(Flags, Expr2, Res).

hook(Flags, '{}'(Expr), Flags, Res) :-
    int(Flags, Expr, Res).

hook(Flags, @(Expr, Options), New, Expr) :-
    append(Options, Flags, New).

example :-
    writeln(quantity-1),
    interval([], @(1.0, [digits(2)]), X),
    writeln(X).

example :-
    writeln(quantity-2),
    interval([], @(2 ... 3, [digits(2)]), X),
    writeln(X).

%
% Hickey Figure 1
%
mixed(L, U) :-
    L < 0,
    U > 0.

zero(0.0, 0.0).

positive(L, U) :-
    L >= 0,
    U > 0.

zeropos(0.0, U) :-
    U > 0.

strictpos(L, _) :-
    L > 0.

negative(L, U) :-
    L < 0,
    U =< 0.

zeroneg(L, 0.0) :-
    L < 0.

strictneg(_, U) :-
    U < 0.

%
% Convert to interval
%
int(Flags, X, Res),
    atomic(X),
    option(engine(pl), Flags)
 => L is X,
    U is X,
    Res = L ... U.

int(Flags, X ... Y, Res),
    option(engine(pl), Flags)
 => interval(X ... Y, Res).

% compatible with atoms like pi
interval(A ... B, Res) :-
    !,
    L is A,
    U is B,
    Res = L ... U.

example :-
    writeln(type-1),
    interval([], 1.0, X),
    writeln(X).

example :-
    writeln(type-2),
    interval([], 0.99 ... 1.01, X),
    writeln(X).

%
% Equality = overlapping
%
int(Flags, X =@= Y, Res)
 => int(Flags, X, A ... B),
    int(Flags, Y, C ... D),
    L is max(A, C),
    U is min(B, D),
    L =< U,
    Res = L ... U.

example :-
    writeln(equality-1),
    X = 1.00 ... 1.02,
    Y = 1.01 ... 1.0Inf,
    interval([], X =@= Y, Z),
    writeln(X =@= Y --> Z).

example :-
    writeln(equality-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.04, Y),
    \+ interval([], X =@= Y, _),
    writeln(X =@= Y --> fails).

%
% Hickey, Theorem 4
%
int(Flags, X + Y, Res)
 => int(Flags, X, ResX),
    int(Flags, Y, ResY),
    interval(ResX + ResY, Res).

interval(A...B + C...D, Res) :-
    L is A + C,
    U is B + D,
    Res = L ... U.

example :-
    writeln(sum-1),
    X = 1.00 ... 1.02,
    Y = 1.01 ... 1.0Inf,
    interval([], X + Y, Z),
    writeln(X + Y --> Z).

example :-
    writeln(sum-2),
    X = 1.00 ... 1.02,
    Y = 1.03 ... 1.04,
    interval([], X + Y, Z),
    writeln(X + Y --> Z).

%
% Hickey, Theorem 4
%
int(Flags, X - Y, Res)
 => int(Flags, X, ResX),
    int(Flags, Y, ResY),
    interval(ResX - ResY, Res).

interval(A...B - C...D, Res) :-
    L is A - D,
    U is B - C,
    Res = L ... U.

example :-
    writeln(diff-1),
    X = 0.99 ... 1.01,
    Y = 0.99 ... 1.01,
    interval([], X - Y, Z),
    writeln(X - Y --> Z).

example :-
    writeln(diff-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.0Inf, Y),
    interval([], X - Y, Z),
    writeln(X - Y --> Z).

%
% Hickey Theorem 6 and Figure 3
%
int(Flags, X * Y, Res)
 => int(Flags, X, ResX),
    int(Flags, Y, ResY),
    interval(ResX * ResY, Res).

%
% Hickey, Figure 3 (last rows first)
%
interval(A ... B * C ... D, Res) :-
    once(zero(A, B); zero(C, D)),
    !,
    Res = 0.0 ... 0.0.

% P * P
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    positive(C, D),
    !,
    L is A * C,
    U is B * D,
    Res = L ... U.

% P * M
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    mixed(C, D),
    !,
    L is B * C,
    U is B * D,
    Res = L ... U.

% P * N
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    negative(C, D),
    !,
    L is B * C,
    U is A * D,
    Res = L ... U.

% M * P
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    positive(C, D),
    !,
    L is A * D,
    U is B * D,
    Res = L ... U.

% M * M
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    mixed(C, D),
    !,
    L is min(A * D, B * C),
    U is max(A * C, B * D),
    Res = L ... U.

% M * N
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    negative(C, D),
    !,
    L is B * C,
    U is A * C,
    Res = L ... U.

% N * P
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    positive(C, D),
    !,
    L is A * D,
    U is B * C,
    Res = L ... U.

% N * M
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    mixed(C, D),
    !,
    L is A * D,
    U is A * C,
    Res = L ... U.

% N * N
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    negative(C, D),
    !,
    L is B * D,
    U is A * C,
    Res = L ... U.

example :-
    writeln(example-4.2-1),
    interval([], pi ... pi, Y),
    interval([], 2 * Y, Z),
    writeln(2 * Y --> Z).

example :-
    writeln(example-4.2-2),
    interval([], 0.0 ... 0.0, X),
    interval([], -1.0Inf ... 1.0Inf, Y),
    interval([], X * Y, Z),
    writeln(X * Y --> Z).

%
% Hickey Theorem 8 and Figure 4
%
int(Flags, X / Y, Res)
 => int(Flags, X, ResX),
    int(Flags, Y, ResY),
    interval(ResX / ResY, Res).

% P1 / P (special case, then general case)
interval(A ... B / 0.0 ... D, Res) :-
    strictpos(A, B),
    positive(0.0, D),
    !,
    L is A / D,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    positive(C, D),
    !,
    L is A / D,
    U is B / C,
    Res = L ... U.

% P0 / P
interval(A ... B / 0.0 ... D, Res) :-
    zeropos(A, B),
    positive(0.0, D),
    !,
    L is 0.0,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    positive(C, D),
    !,
    L is 0.0,
    U is B / C,
    Res = L ... U.

% M / P
interval(A ... B / 0.0 ... D, Res) :-
    mixed(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is B / C,
    Res = L ... U.

% N0 / P
interval(A ... B / 0.0 ... D, Res) :-
    zeroneg(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is 0.0,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is 0.0,
    Res = L ... U.

% N1 / P
interval(A ... B / 0.0 ... D, Res) :-
    strictneg(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is B / D,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is B / D,
    Res = L ... U.

% P1 / M (2 solutions)
interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    mixed(C, D),
    L is -1.0Inf,
    U is A / C,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    mixed(C, D),
    !,
    L is A / D,
    U is 1.0Inf,
    Res = L ... U.

% P0 / M
interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% M / M
interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% N0 / M
interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% N1 / M (2 solutions)
interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    mixed(C, D),
    L is -1.0Inf,
    U is B / D,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    mixed(C, D),
    !,
    L is B / C,
    U is 1.0Inf,
    Res = L ... U.

% P1 / N
interval(A ... B / C ... 0.0, Res) :-
    strictpos(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is A / C,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is A / C,
    Res = L ... U.

% P0 / N
interval(A ... B / C ... 0.0, Res) :-
    zeropos(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is 0.0,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is 0.0,
    Res = L ... U.

% M / N
interval(A ... B / C ... 0.0, Res) :-
    mixed(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is A / D,
    Res = L ... U.

% N0 / N
interval(A ... B / C ... 0.0, Res) :-
    zeroneg(A, B),
    negative(C, 0.0),
    !,
    L is 0.0,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    negative(C, D),
    !,
    L is 0.0,
    U is A / D,
    Res = L ... U.

% N1 / N
interval(A ... B / C ... 0.0, Res) :-
    strictneg(A, B),
    negative(C, 0.0),
    !,
    L is B / C,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    negative(C, D),
    !,
    L is B / C,
    U is A / D,
    Res = L ... U.

example :-
    writeln(page-3-(-inf ... 0, 1...inf)),
    X = 1.00 ... 1.00,
    Y = -1.0Inf ... 1.0,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-3-(0 ... inf)),
    X = 0.00 ... 1.00,
    Y = 0.00 ... 1.00,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-4-(r-without-0)),
    X = 1.00 ... 1.00,
    Y = -1.0Inf ... 1.0Inf,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-5-(-inf ... -1, 1 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0 ... 1.0,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-6-(-inf ... -1, +0 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0 ... 1.0Inf,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-8-(-inf ... -0, 1 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0Inf ... 1.0,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-a-(0 ... inf)),
    X = 0.0 ... 1.0,
    Y = 0.0 ... 2.0,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-b-(0 ... 1)),
    X = 1.0 ... 1.0,
    Y = 1.0 ... 1.0Inf,
    interval([], X / Y, Z),
    writeln(X / Y --> Z).

%
% Available: not NA
%
int(Flags, available(X), Res),
    option(engine(pl), Flags)
 => int(Flags, X, L ... U),
    (integer(L); (float_class(L, ClassL), dif(ClassL, nan))),
    (integer(U); (float_class(U, ClassU), dif(ClassU, nan))),
    Res = L ... U.  % todo: rewrite as a boolean function

%
% Absolute value
%
int(Flags, abs(X), Res),
    option(engine(pl), Flags)
 => int(Flags, X, L ... U),
    (   positive(L, U)
     -> RL is L,
        RU is U
      ; negative(L, U)
     -> RL is abs(U),
        RU is abs(L)
      ; % mixed
        RL is 0.0,
        RU is max(abs(L), U)
    ),
    Res = RL ... RU.

example :-
    writeln(abs-1),
    X = -0.2 ... -0.1,
    interval([], abs(X), Res),
    writeln(abs(X) --> Res).

example :-
    writeln(abs-2),
    X = 0.1 ... 0.2,
    interval([], abs(X), Res),
    writeln(abs(X) --> Res).
    
example :-
    writeln(abs-3),
    X = -0.2 ... 0.1,
    interval([], abs(X), Res),
    writeln(abs(X) --> Res).

example :-
    writeln(abs-4),
    X = -0.1 ... 0.2,
    interval([], abs(X), Res),
    writeln(abs(X) --> Res).

%
% Fraction
%
int(Flags, frac(X, Y), Res),
    option(engine(pl), Flags)
 => int(Flags, X / Y, Res).

int(Flags, dfrac(Num, Den), Res)
 => int(Flags, frac(Num, Den), Res).

example :-
    writeln(frac),
    X = 2.0 ... 2.0,
    interval([], 1 + frac(1, X), Z),
    writeln(1 + frac(1, X) --> Z).

%
% Format result as a t-ratio
%
int(Flags, tratio(X), Res),
    option(engine(pl), Flags)
 => int([digits(2) | Flags], format(X), Res).

int(Flags, chi2ratio(X), Res),
    option(engine(pl), Flags)
 => int([digits(2) | Flags], format(X), Res).

int(Flags, format(X), Res),
    option(engine(pl), Flags)
 => option(digits(D), Flags, 2),
    interval(Flags, X, L0 ... U0),
    L is floor(L0*10^D) / 10^D,
    U is ceiling(U0*10^D) / 10^D,
    Res = L ... U.

%
% Square root. This declaration should be dropped because it
% only applies the sqrt function to the two bounds of the
% interval, which is the default.
%
int(Flags, sqrt(X), Res),
    option(engine(pl), Flags)
 => int(Flags, X, ResX),
    interval(sqrt(ResX), Res).

interval(sqrt(A ... B), Res) :-
    (zero(A, B) ; positive(A, B)),
    !,
    L is sqrt(A),
    U is sqrt(B),
    Res = L ... U.

example :-
    writeln(sqrt-1),
    X = 2.0 ... 2.0,
    interval([], 1 + sqrt(X), Z),
    writeln(1 + sqrt(X) --> Z).

%
% Handle lists
%
int(_Flags, [], Res)
 => Res = [].

int(Flags, [H | T], Res)
 => maplist(int(Flags), [H | T], Res).

%
% Equation sign: named arguments in R functions (leave name unchanged)
%
int(Flags, Name=X, Res),
    option(engine(r), Flags)
 => int(Flags, X, ResA),
    Res = (Name = ResA).

%
% Monotonically increasing or decreasing functions handled by R
%
bound(A ... _, A).

bound(_ ... B, B) :-
    !.

bound(X, X).

%
% Evaluate function for all possible combinations of bounds
% e.g., rint(^(2 ... 3, 4 ... 5), Res) yields 2^3, 2^5, 3^4, and 3^5
%
rint(Flags, Function, Arguments, Res) :-
    maplist(bound, Arguments, Bounds),
    compound_name_arguments(Expr, Function, Bounds),
    option(task(Task), Flags),
    r_task(Task, Expr, Res).

int(Flags, r(Expr), Res)
 => int([engine(r) | Flags], Expr, Res).

int(Flags, pl(Expr), Res)
 => int([engine(pl) | Flags], Expr, Res).

int(Flags, Expr, Res),
    atomic(Expr),
    option(engine(r), Flags)
 => option(task(Task), Flags),
    r_task(Task, Expr, R),
    (   R = _ ... _
     -> Res = R
      ; Res = R ... R
    ).

int(Flags, X ... Y, Res),
    option(engine(r), Flags)
 => option(task(Task), Flags),
    r_task(Task, X, ResX),
    r_task(Task, Y, ResY),
    Res = ResX ... ResY.

int(Flags, '<-'(Var, Expr), Res),
    option(engine(r), Flags)
 => int(Flags, Expr, Res),
    option(task(Task), Flags),
    r_task(Task, '<-'(Var, Res)).

int(Flags, $(Env, Var), Res),
    option(engine(r), Flags)
 => option(task(Task), Flags),
    r_task(Task, $(Env, Var), R),
    int([engine(pl) | Flags], R, Res).

int(Flags, Expr, Res),
    option(engine(r), Flags),
    compound(Expr)
 => compound_name_arguments(Expr, Function, Arguments),
    maplist(int(Flags), Arguments, New),
    findall(R, rint(Flags, Function, New, R), Bounds),
    min_member(L, Bounds),
    max_member(U, Bounds),
    Res = L ... U.

int(Flags, Expr, Res),
    option(engine(r), Flags),
    atomic(Expr)
 => option(task(Task), Flags),
    r_task(Task, Expr, R),
    Res = R...R.

example :-
    writeln(sin-1),
    r_initialize,
    r_session('<-'(r, 'new.env'())),
    interval([], r(sin(0.1)), Z),
    writeln(sin(0.1) --> Z).

example :-
    writeln(sin-2),
    r_session('<-'(r, 'new.env'())),
    interval([], r(sin(0.1 ... 0.2)), Z),
    writeln(sin(0.1 ... 0.2) --> Z).

example(1) :-
    writeln(pbinom),
    r_initialize,
    X = 3,
    Size = 10,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    r_session_source(tpaired),
    interval([task(tpaired)], r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 11,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval([], r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 10 ... 11,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval([], r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 10.0 ... 11.0,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval([], r(pbinom(X, Size, Prob, 'log.p'=true)), P),
    writeln(pbinom(X, Size, Prob, 'log.p'=true) --> P).

example :-
    writeln($(x, y)),
    r_initialize,
    r_session_source(tpaired),
    interval([task(tpaired)], r(mu), X),
    writeln($(tpaired, mu) = X).

%
% (Non-R) general functions that do not account for intervals.
%
% Examples
% * int(pl, sin(0.1 ... 0.2), X)
% * int(pl, sin(0.1), X)
%
% Evaluate interval for the arguments first, and then (blindly) apply
% the function to the lower and upper bound. Obviously, this only
% works for functions that behave monotonically in all their
% arguments.
%
int(Flags, X, Res),
    compound(X),
    option(engine(pl), Flags)
 => compound_name_arguments(X, Name, Arguments),
    maplist(int(Flags), Arguments, Results),
    findall(R,
        (   maplist(bound, Results, Bounds),
            compound_name_arguments(New, Name, Bounds),
            R is New
        ), List),
    min_list(List, L),
    max_list(List, U),
    (   length(List, 1)
     -> [Res] = List
     ;  Res = L ... U
    ).

example :-
    writeln(power-generic),
    interval([], 2 ... 3 ^ (-(2 ... 3)), Z),
    writeln("2 ... 3 ^ (-(2 ... 3))" --> Z).

example :-
    writeln(sin-1),
    interval([], sin(0.1), Z),
    writeln(sin(0.1) --> Z).

example :-
    writeln(sin-1),
    interval([], sin(0.1 ... 0.2), Z),
    writeln(sin(0.1 ... 0.2) --> Z).

%
% t-test example
%
example :-
    writeln(ttest),
    D = 5.7 ... 5.8,
    Mu = 4.0 ... 4.0,
    S = 3.8 ... 3.8,
    N = 24,
    interval([], frac(D - Mu, S / sqrt(N)), T),
    writeln(t --> T).

init :-
    session_data(interval),
    !.

init :-
    r(source("interval.R")),
    session_assert(interval).

:- init.
