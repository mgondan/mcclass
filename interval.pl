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

:- multifile hook/3.

interval(Task, Expr, Res) :-
    b_setval(task, Task),
    int(pl, Expr, Res).
    
% Allows for external definitions
int(Engine, Expr, Res),
    hook(Engine, Expr, Hook)
 => int(Engine, Hook, Res).

% Some standard R functions and own definitions. This will be moved elsewhere.
hook(pl, choose(N, K), r(choose(N, K))).

hook(_, instead(_Bug, Wrong, _Correct), Wrong).

hook(_, omit_right(_Bug, Expr), Left) :-
    Expr =.. [_Op, Left, _Right].

hook(_, omit_left(_Bug, Expr), Right) :-
    Expr =.. [_Op, _Left, Right].

hook(_, drop_right(_Bug, Expr), Left) :-
    Expr =.. [_Op, Left, _Right].

hook(_, drop_left(_Bug, Expr), Right) :-
    Expr =.. [_Op, _Left, Right].

hook(_, invent_left(_Bug, Expr), Expr).

hook(_, invent_right(_Bug, Expr), Expr).

hook(_, abbrev(_Sym, Expr, _Text), Expr).

hook(_, color(_Col, Expr), Expr).

hook(pl, '<-'(Var, Expr), r('<-'(Var, pl(Expr)))).

hook(E, ';'(Expr1, Expr2), Res) :-
    int(E, Expr1, _),
    int(E, Expr2, Res).

hook(E, '{}'(Expr), Res) :-
    int(E, Expr, Res).

hook(E, @(N, Options), Res) :-
    member(digits(D), Options),
    interval(E, N, L0 ... U0),
    L is L0 - 0.5/10^D,
    U is U0 + 0.5/10^D,
    Res = L ... U.

example :-
    writeln(quantity-1),
    interval(pl, @(1.0, [digits(2)]), X),
    writeln(X).

example :-
    writeln(quantity-2),
    interval(pl, @(2 ... 3, [digits(2)]), X),
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
int(pl, X, Res),
    atomic(X)
 => L is X,
    U is X,
    Res = L ... U.

int(pl, X ... Y, Res)
 => interval(X ... Y, Res).

% compatible with atoms like pi
interval(A ... B, Res) :-
    !,
    L is A,
    U is B,
    Res = L ... U.

example :-
    writeln(type-1),
    interval(pl, 1.0, X),
    writeln(X).

example :-
    writeln(type-2),
    interval(pl, 0.99 ... 1.01, X),
    writeln(X).

%
% Equality = overlapping
%
int(E, X =@= Y, Res)
 => int(E, X, A ... B),
    int(E, Y, C ... D),
    L is max(A, C),
    U is min(B, D),
    L =< U,
    Res = L ... U.

example :-
    writeln(equality-1),
    X = 1.00 ... 1.02,
    Y = 1.01 ... 1.0Inf,
    interval(pl, X =@= Y, Z),
    writeln(X =@= Y --> Z).

example :-
    writeln(equality-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.04, Y),
    \+ interval(pl, X =@= Y, _),
    writeln(X =@= Y --> fails).

%
% Hickey, Theorem 4
%
int(E, X + Y, Res)
 => int(E, X, ResX),
    int(E, Y, ResY),
    interval(ResX + ResY, Res).

interval(A...B + C...D, Res) :-
    L is A + C,
    U is B + D,
    Res = L ... U.

example :-
    writeln(sum-1),
    X = 1.00 ... 1.02,
    Y = 1.01 ... 1.0Inf,
    interval(pl, X + Y, Z),
    writeln(X + Y --> Z).

example :-
    writeln(sum-2),
    X = 1.00 ... 1.02,
    Y = 1.03 ... 1.04,
    interval(pl, X + Y, Z),
    writeln(X + Y --> Z).

%
% Hickey, Theorem 4
%
int(E, X - Y, Res)
 => int(E, X, ResX),
    int(E, Y, ResY),
    interval(ResX - ResY, Res).

interval(A...B - C...D, Res) :-
    L is A - D,
    U is B - C,
    Res = L ... U.

example :-
    writeln(diff-1),
    X = 0.99 ... 1.01,
    Y = 0.99 ... 1.01,
    interval(pl, X - Y, Z),
    writeln(X - Y --> Z).

example :-
    writeln(diff-2),
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.0Inf, Y),
    interval(pl, X - Y, Z),
    writeln(X - Y --> Z).

%
% Hickey Theorem 6 and Figure 3
%
int(E, X * Y, Res)
 => int(E, X, ResX),
    int(E, Y, ResY),
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
    interval(pl, pi ... pi, Y),
    interval(pl, 2 * Y, Z),
    writeln(2 * Y --> Z).

example :-
    writeln(example-4.2-2),
    interval(pl, 0.0 ... 0.0, X),
    interval(pl, -1.0Inf ... 1.0Inf, Y),
    interval(pl, X * Y, Z),
    writeln(X * Y --> Z).

%
% Hickey Theorem 8 and Figure 4
%
int(E, X / Y, Res)
 => int(E, X, ResX),
    int(E, Y, ResY),
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
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-3-(0 ... inf)),
    X = 0.00 ... 1.00,
    Y = 0.00 ... 1.00,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-4-(r-without-0)),
    X = 1.00 ... 1.00,
    Y = -1.0Inf ... 1.0Inf,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-5-(-inf ... -1, 1 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0 ... 1.0,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(example-4.2-6-(-inf ... -1, +0 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0 ... 1.0Inf,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-8-(-inf ... -0, 1 ... inf)),
    X = 1.0 ... 1.0,
    Y = -1.0Inf ... 1.0,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-a-(0 ... inf)),
    X = 0.0 ... 1.0,
    Y = 0.0 ... 2.0,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

example :-
    writeln(page-9-b-(0 ... 1)),
    X = 1.0 ... 1.0,
    Y = 1.0 ... 1.0Inf,
    interval(pl, X / Y, Z),
    writeln(X / Y --> Z).

%
% Available: not NA
%
int(E, available(X), Res)
 => int(E, X, L ... U),
    float_class(L, ClassL),
    dif(ClassL, nan),
    float_class(U, ClassU),
    dif(ClassU, nan),
    Res = L ... U .  % todo: boolean function

%
% Absolute value
%
int(E, abs(X), Res)
 => int(E, X, L ... U),
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
    interval(pl, abs(X), Res),
    writeln(abs(X) --> Res).

example :-
    writeln(abs-2),
    X = 0.1 ... 0.2,
    interval(pl, abs(X), Res),
    writeln(abs(X) --> Res).
    
example :-
    writeln(abs-3),
    X = -0.2 ... 0.1,
    interval(pl, abs(X), Res),
    writeln(abs(X) --> Res).

example :-
    writeln(abs-4),
    X = -0.1 ... 0.2,
    interval(pl, abs(X), Res),
    writeln(abs(X) --> Res).

%
% Fraction
%
int(E, frac(X, Y), Res)
 => int(E, X / Y, Res).

int(E, dfrac(Num, Den), Res)
 => int(E, frac(Num, Den), Res).

example :-
    writeln(frac),
    X = 2.0 ... 2.0,
    interval(pl, 1 + frac(1, X), Z),
    writeln(1 + frac(1, X) --> Z).

%
% Square root. This declaration should be dropped because it
% only applies the sqrt function to the two bounds of the
% interval, which is the default.
%
int(E, sqrt(X), Res)
 => int(E, X, ResX),
    interval(sqrt(ResX), Res).

interval(sqrt(A ... B), Res) :-
    (   zero(A, B) ;
        positive(A, B)
    ),
    !,
    L is sqrt(A),
    U is sqrt(B),
    Res = L ... U.

example :-
    writeln(sqrt-1),
    X = 2.0 ... 2.0,
    interval(pl, 1 + sqrt(X), Z),
    writeln(1 + sqrt(X) --> Z).

%
% Handle lists
%
int(E, X, Res),
    is_list(X)
 => maplist(int(E), X, Res).

%
% Equation sign: named arguments in R functions (leave unchanged)
%
int(r, Name = X, Res)
 => int(r, X, ResA),
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
rint(Function, Arguments, Res) :-
    maplist(bound, Arguments, Bounds),
    compound_name_arguments(Expr, Function, Bounds),
    b_getval(task, Task),
    r_task(Task, Expr, Res).

int(_, r(Expr), Res)
 => int(r, Expr, Res).

int(_, pl(Expr), Res)
 => int(pl, Expr, Res).

int(r, Expr, Res),
    atomic(Expr)
 => b_getval(task, Task),
    r_task(Task, Expr, R),
    (   R = _ ... _
     -> Res = R
      ; Res = R ... R
    ).

int(r, X ... Y, Res)
 => b_getval(task, Task),
    r_task(Task, X, ResX),
    r_task(Task, Y, ResY),
    Res = ResX ... ResY.

int(r, <-(Var, Expr), Res)
 => int(r, Expr, Res),
    b_getval(task, Task),
    r_task(Task, '<-'(Var, Res)).

int(r, $(Env, Var), Res)
 => b_getval(task, Task),
    r_task(Task, $(Env, Var), R),
    int(pl, R, Res).

int(r, Expr, Res),
    compound_name_arguments(Expr, Function, Arguments)
 => maplist(int(r), Arguments, New),
    findall(R, rint(Function, New, R), Bounds),
    min_member(L, Bounds),
    max_member(U, Bounds),
    Res = L ... U.

example :-
    writeln(sin-1),
    r_initialize,
    r_session('<-'(r, 'new.env'())),
    interval(r, r(sin(0.1)), Z),
    writeln(sin(0.1) --> Z).

example :-
    writeln(sin-2),
    r_session('<-'(r, 'new.env'())),
    interval(r, r(sin(0.1 ... 0.2)), Z),
    writeln(sin(0.1 ... 0.2) --> Z).

example :-
    writeln(pbinom),
    X = 3,
    Size = 10,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval(r, r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 11,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval(r, r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 10 ... 11,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval(r, r(pbinom(X, Size, Prob)), P),
    writeln(pbinom(X, Size, Prob) --> P).

example :-
    writeln(pbinom),
    X = 3,
    Size = 10.0 ... 11.0,
    Prob = 0.6 ... 0.7,
    r_session('<-'(r, 'new.env'())),
    interval(r, r(pbinom(X, Size, Prob, 'log.p'=true)), P),
    writeln(pbinom(X, Size, Prob, 'log.p'=true) --> P).

example :-
    writeln($(x, y)),
    r_initialize,
    r_session_source(tpaired),
    interval(tpaired, r(mu), X),
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
int(pl, X, Res),
    compound(X)
 => compound_name_arguments(X, Name, Arguments),
    maplist(int(pl), Arguments, Results),
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
    interval(pl, 2 ... 3 ^ (-(2 ... 3)), Z),
    writeln("2 ... 3 ^ (-(2 ... 3))" --> Z).

example :-
    writeln(sin-1),
    interval(pl, sin(0.1), Z),
    writeln(sin(0.1) --> Z).

example :-
    writeln(sin-1),
    interval(pl, sin(0.1 ... 0.2), Z),
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
    interval(pl, frac(D - Mu, S / sqrt(N)), T),
    writeln(t --> T).

init :-
    session_data(interval),
    !.

init :-
    r(source("interval.R")),
    session_assert(interval).

:- init.
