% Interval arithmetic in Prolog
:- module(interval, [ interval/2, interval/3, op(150, xfx, ...) ]).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- discontiguous interval/1, interval/3, int/3, unary/3, binary/4.

% Determine result using integer arithmetic
interval(Expr, Res)
 => interval(Expr, Res, []).

interval(Expr, Res, Flags)
 => int(Expr, Res, Flags).

%
% Allows for external definitions
%
:- multifile hook/3.
int(Expr, Res, Flags),
    hook(Expr, R, Flags)
 => Res = R.

:- multifile hook/4.
int(Expr, Res, Flags),
    hook(Expr, Flags, Expr1, Flags1)
 => int(Expr1, Res, Flags1).

% Different order of arguments (used in maplist)
int_(Flags, A, Res) :-
    int(A, Res, Flags).

%
% Standard math, hookable
%
:- multifile monotonical/1.
:- multifile hook/2.
eval(X, Res),
    hook(X, R)
 => Res = R.

% default is/2
eval(X, Res)
 => Res is X.
    
%
% internal calculations
%
% int/3 is given priority, so this is the default at the end
% int(Expr, Res, Flags),
%     int(Expr, Res0)
%  => Res = Res0.

% Atoms like pi
int(Atom, Res, _Flags),
    atom(Atom)
 => eval(Atom, Res).

int(Number, Res, _Flags),
    number(Number)
 => eval(Number, Res).

int(String, Res, _Flags),
    string(String)
 => Res = String.

int(L ... U, Res, _Flags)
 => eval(L, U, Res).

int([], Res, _Flags)
 => Res = [].

int([H | T], Res, _Flags)
 => Res = [H | T]. % todo: check if maplist is needed

% These are example calls. Todo: unit tests
interval :-
    interval(X),
    writeln(this-was-X).

interval(type-1) :-
    interval(1.0, Res),
    writeln(Res).

interval(type-2) :-
    interval(0.99 ... 1.01, Res),
    writeln(Res).

%
% Confidence intervals
%
hook(A, Res, Flags) :-
    \+ member(ci(_), Flags),
    ci(Flags, A),
    !,
    cihook(A, Res, Flags).

cihook(A =@= B, Res, Flags) :-
    !,
    int(A =@= B, L, [ci(lower) | Flags]),
    int(A =@= B, U, [ci(upper) | Flags]),
    (   L = true, U = true
    ->  Res = true
    ;   Res = false
    ).

cihook(A, Res, Flags) :-
    int(A, L, [ci(lower) | Flags]),
    int(A, U, [ci(upper) | Flags]),
    Res = ci(L, U).

% Test if a CI is to be calculated
ci(_Flags, pm(_, _)) :-
    !.

ci(_Flags, ci(_, _)) :-
    !.

ci(_Flags, neginf(_)) :-
    !.

ci(_Flags, ninfpos(_)) :-
    !.

ci(Flags, A) :-
    compound(A),
    compound_name_arguments(A, _, Args),
    include(ci(Flags), Args, [_ | _]).

% Plus/Minus: return a confidence interval
hook(pm(X, Y), Res, Flags) :-
    option(ci(lower), Flags),
    !,
    int(X - Y, Res, Flags).

hook(pm(X, Y), Res, Flags) :-
    option(ci(upper), Flags),
    !,
    int(X + Y, Res, Flags).

interval(ci1) :-
    D = 2.0 ... 2.1,
    S = 1.6 ... 1.7,
    N = 20,
    interval(D + pm(0.0, 1.96 * S / sqrt(N)), CI),
    writeln(D + pm(0.0, 1.96 * S / sqrt(N)) --> CI).

interval(ci2) :-
    interval(ci(1 ... 2, 3 ... 4) =@= ci(1 ... 2, 3 ... 4), Res),
    writeln(ci(1 ... 2, 3 ... 4) =@= ci(1 ... 2, 3 ... 4) --> Res).

% Return a one-tailed confidence interval
hook(neginf(X), Res, Flags) :-
    option(ci(lower), Flags),
    !,
    int(X, Res, Flags).

hook(neginf(_X), Res, Flags) :-
    option(ci(upper), Flags),
    !,
    int(1.0Inf, Res, Flags).

hook(ninfpos(_X), Res, Flags) :-
    option(ci(lower), Flags),
    !,
    int(-1.0Inf, Res, Flags).

hook(ninfpos(X), Res, Flags) :-
    option(ci(upper), Flags),
    !,
    int(X, Res, Flags).

interval(ci1) :-
    D = 2.0 ... 2.1,
    S = 1.6 ... 1.7,
    N = 20,
    interval(D + pm(0.0, 1.96 * S / sqrt(N)), CI),
    writeln(D + pm(0.0, 1.96 * S / sqrt(N)) --> CI).

interval(ci2) :-
    interval(ci(1 ... 2, 3 ... 4) =@= ci(1 ... 2, 3 ... 4), Res),
    writeln(ci(1 ... 2, 3 ... 4) =@= ci(1 ... 2, 3 ... 4) --> Res).

hook(ci(L, _H), Res, Flags) :-
    option(ci(lower), Flags),
    !,
    int(L, Res, Flags).

hook(ci(_L, H), Res, Flags) :-
    option(ci(upper), Flags),
    !,
    int(H, Res, Flags).

hook(A =@= B, Res, Flags) :-
    select_option(ci(lower), Flags, New),
    !,
    int(A, LA, Flags),
    int(B, LB, Flags),
    int(LA =@= LB, Res, New).

hook(A =@= B, Res, Flags) :-
    select_option(ci(upper), Flags, New),
    !,
    int(A, UA, Flags),
    int(B, UB, Flags),
    int(UA =@= UB, Res, New).
  


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
% Equality = overlapping
%
int(A =@= B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(=@=, A1, B1, Res).

binary(=@=, A ... B, C ... D, Res)
 => eval(max(A, C), min(B, D), L ... U),
    (   L =< U
    ->  Res = true
    ;   Res = false
    ).

binary(=@=, A, B, Res),
    atomic(A),
    atomic(B)
 => (   A =@= B
    ->  Res = true
    ;   Res = false
    ).

interval(equality-1) :-
    X = 1.00 ... 1.02,
    Y = 1.01 ... 1.0Inf,
    interval(X =@= Y, Z),
    writeln(X =@= Y --> Z).

interval(equality-2) :-
    interval(1.00 ... 1.02, X),
    interval(1.03 ... 1.04, Y),
    \+ interval(X =@= Y, _),
    writeln(X =@= Y --> fails).

%
% Hickey, Theorem 4
%
int(A + B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(+, A1, B1, Res).

binary(+, A ... B, C ... D, Res)
 => eval(A + C, B + D, Res).

interval(sum-1) :-
    A = 1.00 ... 1.02,
    B = 1.01 ... 1.0Inf,
    interval(A + B, Res),
    writeln(A + B --> Res).

interval(sum-2) :-
    A = 1.00 ... 1.02,
    B = 1.03 ... 1.04,
    interval(A + B, Res),
    writeln(A + B --> Res).

interval(sum-3) :-
    A = 1.00,
    B = 1.01 ... 1.0Inf,
    interval(A + B, Res),
    writeln(A + B --> Res).

interval(sum-4) :-
    A = 1.00 ... 1.02,
    B = 1.01,
    interval(A + B, Res),
    writeln(A + B --> Res).

interval(sum-5) :-
    A = 1.00,
    B = 1.01,
    interval(A + B, Res),
    writeln(A + B --> Res).

% Unary plus
monotonical(+(+)).

%
% Hickey, Theorem 4
%
int(A - B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(-, A1, B1, Res).

binary(-, A ... B, C ... D, Res)
 => eval(A - D, B - C, Res).

interval(diff-1) :-
    A = 0.99 ... 1.01,
    B = 0.99 ... 1.01,
    interval(A - B, Res),
    writeln(A - B --> Res).

interval(diff-2) :-
    A = 1.00 ... 1.02,
    B = 1.03 ... 1.0Inf,
    interval(A - B, Res),
    writeln(A - B --> Res).

interval(diff-3) :-
    A = 1.00,
    B = 1.01 ... 1.0Inf,
    interval(A - B, Res),
    writeln(A - B --> Res).

interval(diff-4) :-
    A = 1.00 ... 1.02,
    B = -pi,
    interval(A - B, Res),
    writeln(A - B --> Res).

interval(diff-5) :-
    A = 1.00,
    B = pi,
    interval(A - B, Res),
    writeln(A - B --> Res).

% Unary minus
monotonical(-(-)).

%
% Hickey Theorem 6 and Figure 3
%
int(A * B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(*, A1, B1, Res).

binary(*, A ... B, C ... D, Res),
    once(zero(A, B); zero(C, D))
 => Res = 0.0 ... 0.0.

binary(*, A ... B, C ... D, Res),
    positive(A, B),
    positive(C, D)
 => eval(A * C, B * D, Res).

binary(*, A ... B, C ... D, Res),
    positive(A, B),
    mixed(C, D)
 => eval(B * C, B * D, Res).

binary(*, A ... B, C ... D, Res),
    positive(A, B),
    negative(C, D)
 => eval(B * C, A * D, Res).

binary(*, A ... B, C ... D, Res),
    mixed(A, B),
    positive(C, D)
 => eval(A * D, B * D, Res).

binary(*, A ... B, C ... D, Res),
    mixed(A, B),
    mixed(C, D)
 => eval(min(A * D, B * C), max(A * C, B * D), Res).

binary(*, A ... B, C ... D, Res),
    mixed(A, B),
    negative(C, D)
 => eval(B * C, A * C, Res).

binary(*, A ... B, C ... D, Res),
    negative(A, B),
    positive(C, D)
 => eval(A * D, B * C, Res).

binary(*, A ... B, C ... D, Res),
    negative(A, B),
    mixed(C, D)
 => eval(A * D, A * C, Res).

binary(*, A ... B, C ... D, Res),
    negative(A, B),
    negative(C, D)
 => eval(B * D, A * C, Res).

interval(example-4.2-1) :-
    A = pi ... pi,
    B = 2,
    interval(A * B, Res),
    writeln(A * B --> Res).

interval(example-4.2-2) :-
    A = 0.0 ... 0.0,
    B = -1.0Inf ... 1.0Inf,
    interval(A * B, Res),
    writeln(A * B --> Res).

%
% Hickey Theorem 8 and Figure 4
%
int(A / B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(/, A1, B1, Res).

% P1 / P (special case, then general case)
binary(/, A ... B, 0.0 ... D, Res),
    strictpos(A, B),
    positive(0.0, D)
 => eval(A / D, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    strictpos(A, B),
    positive(C, D)
 => eval(A / D, B / C, Res).

% P0 / P
binary(/, A ... B, 0.0 ... D, Res),
    zeropos(A, B),
    positive(0.0, D)
 => eval(0.0, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    zeropos(A, B),
    positive(C, D)
 => eval(0.0, B / C, Res).

% M / P
binary(/, A ... B, 0.0 ... D, Res),
    mixed(A, B),
    positive(0.0, D)
 => eval(-1.0Inf, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    mixed(A, B),
    positive(C, D)
 => eval(A / C, B / C, Res).

% N0 / P
binary(/, A ... B, 0.0 ... D, Res),
    zeroneg(A, B),
    positive(0.0, D)
 => eval(-1.0Inf, 0.0, Res).

binary(/, A ... B, C ... D, Res),
    zeroneg(A, B),
    positive(C, D)
 => eval(A / C, 0.0, Res).

% N1 / P
binary(/, A ... B, 0.0 ... D, Res),
    strictneg(A, B),
    positive(0.0, D)
 => eval(-1.0Inf, B / D, Res).

binary(/, A ... B, C ... D, Res),
    strictneg(A, B),
    positive(C, D)
 => eval(A / C, B / D, Res).

% P1 / M (2 solutions)
binary(/, A ... B, C ... D, Res),
    strictpos(A, B),
    mixed(C, D)
 => eval(-1.0Inf, A / C, Res) ; eval(A / D, 1.0Inf, Res).

% P0 / M
binary(/, A ... B, C ... D, Res),
    zeropos(A, B),
    mixed(C, D)
 => eval(-1.0Inf, 1.0Inf, Res).

% M / M
binary(/, A ... B, C ... D, Res),
    mixed(A, B),
    mixed(C, D)
 => eval(-1.0Inf, 1.0Inf, Res).

% N0 / M
binary(/, A ... B, C ... D, Res),
    zeroneg(A, B),
    mixed(C, D)
 => eval(-1.0Inf, 1.0Inf, Res).

% N1 / M (2 solutions)
binary(/, A ... B, C ... D, Res),
    strictneg(A, B),
    mixed(C, D)
 => eval(-1.0Inf, B / D, Res) ; eval(B / C, 1.0Inf, Res).

% P1 / N
binary(/, A ... B, C ... 0.0, Res),
    strictpos(A, B),
    negative(C, 0.0)
 => eval(-1.0Inf, A / C, Res).

binary(/, A ... B, C ... D, Res),
    strictpos(A, B),
    negative(C, D)
 => eval(B / D, A / C, Res).

% P0 / N
binary(/, A ... B, C ... 0.0, Res),
    zeropos(A, B),
    negative(C, 0.0)
 => eval(-1.0Inf, 0.0, Res).

binary(/, A ... B, C ... D, Res),
    zeropos(A, B),
    negative(C, D)
 => eval(B / D, 0.0, Res).

% M / N
binary(/, A ... B, C ... 0.0, Res),
    mixed(A, B),
    negative(C, 0.0)
 => eval(-1.0Inf, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    mixed(A, B),
    negative(C, D)
 => eval(B / D, A / D, Res).

% N0 / N
binary(/, A ... B, C ... 0.0, Res),
    zeroneg(A, B),
    negative(C, 0.0)
 => eval(0.0, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    zeroneg(A, B),
    negative(C, D)
 => eval(0.0, A / D, Res).

% N1 / N
binary(/, A ... B, C ... 0.0, Res),
    strictneg(A, B),
    negative(C, 0.0)
 => eval(B / C, 1.0Inf, Res).

binary(/, A ... B, C ... D, Res),
    strictneg(A, B),
    negative(C, D)
 => eval(B / C, A / D, Res).

interval(page-3-(-inf ... 0, 1...inf)) :-
    A = 1.00 ... 1.00,
    B = -1.0Inf ... 1.0,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(example-4.2-3-(0 ... inf)) :-
    A = 0.00 ... 1.00,
    B = 0.00 ... 1.00,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(example-4.2-4-(r-without-0)) :-
    A = 1.00,
    B = -1.0Inf ... 1.0Inf,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(example-4.2-5-(-inf ... -1, 1 ... inf)) :-
    A = 1.0,
    B = -1.0 ... 1.0,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(example-4.2-6-(-inf ... -1, +0 ... inf)) :-
    A = 1.0 ... 1.0,
    B = -1.0 ... 1.0Inf,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(page-8-(-inf ... -0, 1 ... inf)) :-
    A = 1.0 ... 1.0,
    B = -1.0Inf ... 1.0,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(page-9-a-(0 ... inf)) :-
    A = 0.0 ... 1.0,
    B = 0.0 ... 2.0,
    interval(A / B, Res),
    writeln(A / B --> Res).

interval(page-9-b-(0 ... 1)) :-
    A = 1.0 ... 1.0,
    B = 1.0 ... 1.0Inf,
    interval(A / B, Res),
    writeln(A / B --> Res).

%
% Power
%
% todo: only for positive base
int(A^B, Res, Flags)
 => int(A, A1, Flags),
    int(B, B1, Flags),
    binary(^, A1, B1, Res).

binary(^, A ... B, C, Res)
 => eval(A^C, B^C, Res).

monotonical(^(*, /)).

interval(power-1) :-
    interval(2^2, Res),
    writeln(2^2 --> Res).

interval(power-generic) :-
    interval(2 ... 3 ^ -2, Res),
    writeln(2 ... 3 ^ -2 --> Res).

%
% Available: not NA
%
% todo: boolean function
int(available(A), Res, Flags)
 => int(A, A1, Flags),
    available(A1, Res).

available(A, Res),
    integer(A)
 => eval(A, Res).

available(A, Res),
    number(A)
 => float_class(A, Class),
    dif(Class, nan),
    eval(A, Res).

available(A, Res),
    atom(A)
 => eval(A, A1),
    available(A1, Res).

available(A ... B, Res)
 => available(A, A1),
    available(B, B1),
    eval(A1, B1, Res).

available(ci(A, B), Res)
=> available(A, A1),
    available(B, B1),
    eval(A1, B1, Res).



%
% Absolute value
%
int(abs(A), Res, Flags)
 => int(A, A1, Flags),
    unary(abs, A1, Res).

unary(abs, A ... B, Res)
 => ( positive(A, B)
     -> eval(A, B, Res)
    ; negative(A, B)
     -> eval(abs(B), abs(A), Res)
    ; % mixed
        eval(0.0, max(abs(A), B), Res)
    ).

interval(abs-1) :-
    A = -0.2 ... -0.1,
    interval(abs(A), Res),
    writeln(abs(A) --> Res).

interval(abs-2) :-
    A = 0.1 ... 0.2,
    interval(abs(A), Res),
    writeln(abs(A) --> Res).
    
interval(abs-3) :-
    A = -0.2 ... 0.1,
    interval(abs(A), Res),
    writeln(abs(A) --> Res).

interval(abs-4) :-
    A = -0.1 ... 0.2,
    interval(abs(A), Res),
    writeln(abs(A) --> Res).

interval(abs-5) :-
    A = -0.1,
    interval(abs(A), Res),
    writeln(abs(A) --> Res).

% Fraction
int(frac(A, B), Res, Flags)
 => int(A / B, Res, Flags).

int(dfrac(A, B), Res, Flags)
 => int(A / B, Res, Flags).

interval(frac) :-
    A = 2.0 ... 2.0,
    interval(1 + frac(1, A), Res),
    writeln(1 + frac(1, A) --> Res).

% Multiply
int(dot(A, B), Res, Flags)
 => int(A * B, Res, Flags).

% Format result as a t-ratio
int(tstat(A), Res, Flags)
 => int(format(A), Res, [digits(2) | Flags]).

int(hdrs(A), Res, Flags)
 => int(format(A), Res, [digits(1) | Flags]).

int(chi2ratio(A), Res, Flags)
 => int(format(A), Res, [digits(2) | Flags]).

int(pval(A), Res, Flags)
 => int(format(A), Res, [digits(3) | Flags]).

%
% This needs to be rewritten
%
int(format(A), Res, Flags)
 => option(digits(D), Flags, 2),
    int(A, Fmt, Flags),
    lower(+, Fmt, L0),
    upper(+, Fmt, U0),
    eval(floor(L0*10^D) / 10^D, ceiling(U0*10^D) / 10^D, Res).

% Square root
int(sqrt(A), Res, Flags),
    int(A, A1, Flags)
 => unary(sqrt, A1, Res).

unary(sqrt, A ... B, Res)
 => ( zero(A, B)
     -> eval(0.0, 0.0, Res)
    ; positive(A, B)
     -> eval(sqrt(A), sqrt(B), Res)
    ; mixed(A, B)
     -> eval(0.0, sqrt(B), Res)
    ).

interval(sqrt-1) :-
    A = 2.0 ... 2.0,
    interval(1 + sqrt(A), Res),
    writeln(1 + sqrt(A) --> Res).

% Handle lists
int([], Res, _Flags)
 => Res = [].

int([H | T], Res, _Flags)
 => exclude(omit_(Flags), [H | T], Excluded),
    maplist(int_(Flags), Excluded, Res).

omit_(_Flags, omit(_, _)).

%
% Equation sign: named arguments in R functions (leave name unchanged)
%
int(Name=A, Res, Flags),
    option(engine(r), Flags)
 => int(A, A1, Flags),
    Res = (Name = A1).

%
% Interval bounds for monotonically behaving functions
%
lower(+, A ... _, L)
 => L = A.

lower(+, A, L)
 => L = A.

lower(-, A, L)
 => upper(+, A, L).

lower(*, A, L)
 => lower(+, A, L) ; lower(-, A, L).

lower(/, A, L)
 => L = A.

upper(+, _ ... B, U)
 => U = B.

upper(+, A, U)
 => U = A.

upper(-, A, U)
 => lower(+, A, U).

upper(*, A, U)
 => upper(+, A, U) ; upper(-, A, U).

upper(/, A, U)
 => U = A.

noint(A ... B)
 => throw(error(type_error(atomic, A ... B), noint)).

noint(_)
 => true.

%
% Monotonically behaving in all arguments
%
% +: increasing
% -: decreasing
% *: increasing or decreasing
% /: not an interval
monotonical(A, Dir) :-
    compound(A),
    compound_name_arity(A, Name, Arity),
    compound_name_arity(New, Name, Arity),
    monotonical(New),
    compound_name_arguments(New, Name, Dir).

int(A, Res, Flags),
    compound(A),
    monotonical(A, Dir)
 => compound_name_arguments(A, Name, Args),
    maplist(int_(Flags), Args, Args1),
    findall(R,
        ( maplist(lower, Dir, Args1, Args2),
          compound_name_arguments(C, Name, Args2),
          eval(C, R)
        ), Lower),
    findall(R,
        ( maplist(upper, Dir, Args1, Args2),
          compound_name_arguments(C, Name, Args2),
          eval(C, R)
        ), Upper),
    min_list(Lower, L),
    max_list(Upper, U),
    Res = L ... U.

monotonical(ttest(*, *, -, /)).

hook(ttest(A, B, C, D), Res) :-
    !, Res is (A - B) / (C / sqrt(D)).

interval(ttest) :-
    D = 5.7 ... 5.8,
    Mu = 4.1 ... 4.2,
    S = 3.8 ... 3.9,
    N = 24,
    interval(ttest(D, Mu, S, N), Res),
    writeln(t --> Res).

%
% Sine, not monotonically behaving
%
int(sin(A), Res, Flags)
 => int(A, A1, Flags),
    unary(sin, A1, Res).

unary(sin, A ... B, Res)
 => PA is (A - pi/2) / 2 / pi,
    PB is (B - pi/2) / 2 / pi,
    ( floor(PB) - floor(PA) >= 1
     -> U = 1
      ; U is max(sin(A), sin(B))
    ),
    QA is (A + pi/2) / 2 / pi,
    QB is (B + pi/2) / 2 / pi,
    ( floor(QB) - floor(QA) >= 1
     -> L = -1
      ; L is min(sin(A), sin(B))
    ),
    Res = L ... U.

interval(sin-1) :-
    interval(sin(0.1), Res),
    writeln(sin(0.1) --> Res).

interval(sin-2) :-
    interval(sin(0.0 ... 4*pi), Res),
    writeln(sin(0.0 ... 4*pi) --> Res).

interval(sin-3) :-
    interval((pi/2 - 0.2) ... (pi/2 + 0.3), A),
    interval(sin(A), Res),
    writeln(sin(A) --> Res).

interval(sin-4) :-
    interval((2*pi/2 - 0.2) ... (2*pi/2 + 0.3), A),
    interval(sin(A), Res),
    writeln(sin(A) --> Res).

interval(sin-5) :-
    interval((3*pi/2 - 0.2) ... (3*pi/2 + 0.3), A),
    interval(sin(A), Res),
    writeln(sin(A) --> Res).

interval(sin-6) :-
    interval((4*pi/2 - 0.2) ... (4*pi/2 + 0.3), A),
    interval(sin(A), Res),
    writeln(sin(A) --> Res).

%
% No intervals at play
%
int(A, Res, Flags),
    compound(A)
 => compound_name_arguments(A, Name, Args),
    maplist(int_(Flags), Args, Args1),
    catch_with_backtrace(
        maplist(noint, Args1),
        Error,
        print_message(error, Error)
    ),
    compound_name_arguments(C, Name, Args1),
    eval(C, Res).

hook(ttest2(A, B, C, D), Res) :-
    !, Res is (A - B) / (C / sqrt(D)).

% includes an interval, therefore fails
%interval(ttest2) :-
%    writeln(ttest2-fails),
%    D = 5.7 ... 5.8,
%    Mu = 4.1 ... 4.2,
%    S = 3.8 ... 3.9,
%    N = 24,
%    interval(ttest2(D, Mu, S, N), Res),
%    writeln(t --> Res).

% no intervals, therefore succeeds
interval(ttest3) :-
    writeln(ttest3-succeeds),
    D = 5.7,
    Mu = 4.1,
    S = 3.8,
    N = 24,
    interval(ttest2(D, Mu, S, N), Res),
    writeln(t --> Res).

%
% Operations with atomic elements
%
% For convenience
eval(Expr1, Expr2, L ... U) :-
    eval(Expr1, L),
    eval(Expr2, U).

% Default operations for unary functions
unary(Op, A, Res),
    atomic(A)
 => Call =.. [Op, A],
    eval(Call, Res).

unary(Op, A ... B, Res)
 => CallA =.. [Op, A],
    eval(CallA, A1),
    CallB =.. [Op, B],
    eval(CallB, B1),
    msort([A1, B1], Res).

binary(Op, A, B, Res),
    atomic(A),
    atomic(B)
 => Call =.. [Op, A, B],
    eval(Call, Res).

binary(Op, A, B ... C, Res),
    atomic(A)
 => binary(Op, A ... A, B ... C, Res).

binary(Op, A ... B, C, Res),
    atomic(C)
 => binary(Op, A ... B, C ... C, Res).

