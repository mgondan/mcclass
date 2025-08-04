:- module(interval_wrapper, []).

:- reexport(library(rint)).
:- use_module(util).

:- discontiguous interval_/3.

% Macros (have global scope)
:- asserta((user:term_expansion(macro(Atomic), Clauses) :-
    Options = [hook(r_session:r_topic), prefix(rint)],
    rint:macro_clause(Atomic, Options, Clauses))).

:- asserta((user:term_expansion(macro(Atomic, Options0), Clauses) :-
    add_option(hook(r_session:r_topic), Options0, Options1),
    add_option(prefix(rint), Options1, Options2), 
    rint:macro_clause(Atomic, Options2, Clauses))).

:- asserta((user:term_expansion(macro(Op/Arity, Fn, Dir), Clauses) :-
    Options = [hook(r_session:r_topic), prefix(rint)],
    rint:macro_clause(Op/Arity, Fn, Dir, Options, Clauses))).

/* :- asserta((user:term_expansion(macro(Op/Arity, Dir), Clauses) :-
    Options = [hook(r_session:r_topic), prefix(rint)],
    rint:macro_clause(Op/Arity, all, Dir, Options, Clauses))). */

:- asserta((user:term_expansion(macro(Op/Arity, Fn, Dir, Options0), Clauses) :-
    add_option(hook(r_session:r_topic), Options0, Options1),
    add_option(prefix(rint), Options1, Options2), 
    rint:macro_clause(Op/Arity, Fn, Dir, Options2, Clauses))).

% Assert clauses of interval_/3 at the beginning of module rint
:- initialization(assert_clauses(interval_wrapper)).

%
% Confidence intervals
%
interval_(ci(A, B), Res, Flags) :- 
    rint:interval_(A, A1, Flags),
    rint:interval_(B, B1, Flags),
    !, Res = ci(A1, B1).

interval_(atomic(Var) <- ci(number(A), number(B)), Res, _Flags) :-
    rint:eval(r(Var <- call("ci", A, B)), Res0),
    !, rint:clean(Res0, Res).

interval_(atomic(Var) <- ci(L1...U1, L2...U2), Res, _Flags) :-
    rint:eval(r(Var <- call("ci", call("...", L1, U1), call("...", L2, U2))), Res0),
    !, rint:clean(Res0, Res).

interval_(atomic(Var) <- ci(A0, B0), Res, Flags) :-
    rint:interval_(A0, A, Flags),
    rint:interval_(B0, B, Flags),
    !, rint:interval_(atomic(Var) <- ci(A, B), Res, Flags). 

interval_(round(ci(A, B), number(Dig)), Res, Flags) :- 
    rint:interval_(round(A, number(Dig)), A1, Flags),
    rint:interval_(round(B, number(Dig)), B1, Flags),
    !, Res = ci(A1, B1).

%
% Fractions, i.e., numerator, line, and denominator
%
interval_(frac(A, B), Res, Flags) :-
    rint:option(digits(Dig), Flags, _),
    rint:interval_(round(A, number(Dig)), A1, Flags),
    rint:interval_(round(B, number(Dig)), B1, Flags),
    rint:interval_(A1 / B1, Res0, Flags),
    !, Res = Res0.

interval_(dfrac(A, B), Res, Flags) :-
    !, rint:interval_(frac(A, B), Res, Flags).

%
% Reasonable number of digits
%
interval_(tstat(A), Res, Flags) :-
    !, rint:interval_(round(A, number(2)), Res, Flags).

interval_(hdrs(A), Res, Flags) :-
    !, rint:interval_(round(A, number(1)), Res, Flags).

interval_(chi2ratio(A), Res, Flags) :-
    !, rint:interval_(round(A, number(2)), Res, Flags).

interval_(pval(A), pval(Res), Flags) :-
    !, rint:interval_(A, Res, Flags).

%
% Bugs
%
% Forget parts of an expression
interval_(omit_left(_Bug, A), Res, Flags) :-
    A =.. [_Op, _L, R],
    !, rint:interval_(R, Res, Flags).

interval_(omit_right(_Bug, A), Res, Flags) :-
    A =.. [_Op, L, _R],
    !, rint:interval_(L, Res, Flags).

interval_(omit(_Bug, _A), Res, _Flags) :-
    !, Res = atomic(na).

% Instead
interval_(instead(_Bug, Wrong, _Correct), Res, Flags) :-
    !, rint:interval_(Wrong, Res, Flags).

interval_(instead(_Bug, Wrong, _Correct, _Correct0), Res, Flags) :-
    !, rint:interval_(Wrong, Res, Flags).

% Drop
interval_(drop_right(_Bug, A), Res, Flags) :-
    A =.. [_Op, L, _R],
    !, rint:interval_(L, Res, Flags).

interval_(drop_left(_Bug, A), Res, Flags) :-
    A =.. [_Op, _L, R],
    !, rint:interval_(R, Res, Flags).

% add_left, add_right, add
interval_(add_right(_Bug, A), Res, Flags) :-
    !, rint:interval_(A, Res, Flags).

interval_(add_left(_Bug, A), Res, Flags) :-
    !, rint:interval_(A, Res, Flags).

interval_(add(_Bug, A), Res, Flags) :-
    !, rint:interval_(A, Res, Flags).

%
% Multiply
%
interval_(dot(A, B), Res, Flags) :-
    !, rint:interval_(A * B, Res, Flags).

%
% Available: not NA
%
interval_(available(pval(A)), Res, Flags) :-
    !, rint:interval_(available(A), Res, Flags).

interval_(available(number(A)), Res, _Flags) :-
    rint:avail_(A, _),
    !, Res = bool(true).

interval_(available(number(_)), Res, _Flags) :-
    !, Res = bool(false).

rint:avail_(A, Res),
    integer(A)
 => rint:eval(A, Res).

rint:avail_(A, Res),
    number(A)
 => float_class(A, Class),
    dif(Class, nan),
    rint:eval(A, Res).

rint:avail_(A0, Res)
 => rint:eval(A0, A),
    rint:avail_(A, Res).

interval_(available(L0...U0), Res, _Flags) :-
    rint:avail_(L0, L),
    rint:avail_(U0, U),
    rint:eval(L, U, _),
    !, Res = bool(true).

interval_(available(_..._), Res, _Flags) :-
    !, Res = bool(false).

interval_(available(ci(A, B)), Res, Flags) :-
    rint:interval_(available(A), bool(true), Flags),
    rint:interval_(available(B), bool(true), Flags),
    !, Res = bool(true).

interval_(available(ci(_, _)), Res, _Flags) :-
    !, Res = bool(false).

interval_(available([]), Res, _Flags) :-
    !, Res = bool(true).

interval_(available(A), Res, Flags) :-
    rint:interval_(A, A1, Flags),
    rint:interval_(available(A1), Res0, Flags),
    !, Res = Res0.

%
% Equality
%
interval_(A =@= pval(B), Res, Flags):-
    !, rint:interval_(A =@= B, Res, Flags).

interval_(number(A) =@= number(B), Res, Flags):-
    rint:interval_(number(A) =:= number(B), bool(true), Flags),
    !, Res = bool(true).

interval_(A1...A2 =@= B1...B2, Res, Flags):-
    rint:interval_(number(A1) =< number(B2), bool(true), Flags),
    rint:interval_(number(A2) >= number(B1), bool(true), Flags),
    !, Res = bool(true).

interval_(number(A) =@= B1...B2, Res, Flags):-
    rint:interval_(number(A) >= number(B1), bool(true), Flags),
    rint:interval_(number(A) =< number(B2), bool(true), Flags),
    !, Res = bool(true).

interval_(A1...A2 =@= number(B), Res, Flags):-
    rint:interval_(number(B) >= number(A1), bool(true), Flags),
    rint:interval_(number(B) =< number(A2), bool(true), Flags),
    !, Res = bool(true).

interval_(ci(A, B) =@= ci(C, D), Res, Flags) :-
    rint:interval_(A =@= C, bool(true), Flags),
    rint:interval_(B =@= D, bool(true), Flags),
    !, Res = bool(true).

interval_(_ =@= _, Res, _Flags):-
    !, Res = bool(false).

% Addition CI
interval_(ci(A, B) + C, Res, Flags) :-
    rint:interval_(A + C, A1, Flags),
    rint:interval_(B + C, B1, Flags),
    !, Res = ci(A1, B1).

interval_(C + ci(A, B), Res, Flags) :-
    !, rint:interval_(ci(A, B) + C, Res, Flags).

% Subtraction CI
interval_(ci(A, B) - C, Res, Flags) :-
    rint:interval_(A - C, A1, Flags),
    rint:interval_(B - C, B1, Flags),
    !, Res = ci(A1, B1).

% Multiplication CI
interval_(ci(A, B) * C, Res, Flags) :-
    rint:interval_(A * C, A1, Flags),
    rint:interval_(B * C, B1, Flags),
    !, Res = ci(A1, B1).

% Division CI
interval_(ci(A, B) / C, Res, Flags) :-
    rint:interval_(A / C, A1, Flags),
    rint:interval_(B / C, B1, Flags),
    !, Res = ci(A1, B1).

% Exponential CI
interval_(exp(ci(A, B)), Res, Flags) :-
    rint:interval_(exp(A), A1, Flags),
    rint:interval_(exp(B), B1, Flags),
    !, Res = ci(A1, B1).

% Plus/minus
interval_(pm(A, B), Res, Flags) :-
    rint:interval_(A - B, A1, Flags),
    rint:interval_(A + B, B1, Flags),
    !, Res = ci(A1, B1).

% Return a one-tailed confidence interval
interval_(neginf(A0), Res, Flags) :-
    rint:interval_(A0, A, Flags),
    !, Res = ci(A, atomic(1.0Inf)).

interval_(ninfpos(A0), Res, Flags) :-
    rint:interval_(A0, A, Flags),
    !, Res = ci(atomic(-1.0Inf), A).

%
% Denote
%
interval_(denote(_Sym, A, _Text), Res, Flags) :-
    rint:interval_(A, Res0, Flags),
    !, Res = Res0.

%
% Color
%
interval_(color(_Col, A), Res, Flags) :-
    rint:interval_(A, Res0, Flags),
    !, Res = Res0.

%
% Read intervals from input
%
interval_(input(A), Res, Flags) :-
    option(digits(D), Flags, 2),
    rint:eval(10^(-D)/2, Eps),
    rint:eval(-Eps, MEps),
    !, rint:interval_(A + MEps...Eps, Res, Flags).

interval_(input(ci(A, B)), Res, Flags) :-
    option(digits(D), Flags, 2),
    rint:eval(10^(-D)/2, Eps),
    rint:eval(-Eps, MEps),
    rint:interval_(A + MEps...Eps, A1, Flags),
    rint:interval_(B + MEps...Eps, B1, Flags),
    !, Res = ci(A1, B1).

%
% Other
%
interval_(';'(A, B), Res, Flags) :-
    rint:interval_(A, _, Flags),
    rint:interval_(B, Res0, Flags),
    !, Res = Res0.

interval_(';'(A, B, C), Res, Flags) :-
    rint:interval_(A, _, Flags),
    rint:interval_(B, _, Flags),
    rint:interval_(C, Res0, Flags),
    !, Res = Res0.

interval_('{}'(A), Res, Flags) :-
    !, rint:interval_(A, Res, Flags).

%
% Upper tail, lower tail, both tails
%
% dist/3 just returns the second argument. It is needed for mathematical
% rendering expressions like P_T(X >= x; df=n-1) via mathml.
%
interval_(dist(_, X, _), Res, Flags) :-
    !, rint:interval_(X, Res, Flags).

%
% This forwards the tail argument to lower.tail of the R function, e.g. in
% pt(T, DF, lower.tail=TRUE)
%
interval_(tail(A), Res, _Flags) :-
    !, Res = A.

interval_(tail(A, _), Res, _Flags) :-
    !, Res = A.

interval_(arg(A, _K), Res, Flags) :-
    !, rint:interval_(A, Res, Flags).

%
% cbinom/5: for testbinom topic
%
interval_(cbinom(Alpha, N, Pi, string("upper"), string("min")), Res, Flags) :-
    !, rint:interval_(qbinom(Alpha, N, Pi, bool(false)) + number(1), Res, Flags).

interval_(cbinom(Alpha, N, Pi, string("lower"), string("max")), Res, Flags) :-
    !, rint:interval_(qbinom(Alpha, N, Pi, bool(true)) - number(1), Res, Flags).

interval_(cbinom(Alpha, N, Pi, string("densi"), string("min")), Res, Flags) :-
    !, rint:interval_(udbinom(Alpha, N, Pi), Res, Flags).

interval_(cbinom(Alpha, N, Pi, string("densi"), string("max")), Res, Flags) :-
    !, rint:interval_(ldbinom(Alpha, N, Pi), Res, Flags).

macro(ldbinom/3, all, [+, +, +]).

macro(udbinom/3, all, [-, +, +]).

%
% pwbinom/4
%
interval_(pwbinom(Crit, N, Pi, string("lower")), Res, Flags) :-
    !, rint:interval_(pbinom(Crit, N, Pi), Res, Flags).

interval_(pwbinom(Crit, N, Pi, string("upper")), Res, Flags) :-
    !, rint:interval_(pbinom(Crit - number(1), N, Pi, bool(false)), Res, Flags).

interval_(pwbinom(Crit, N, Pi, string("densi")), Res, Flags) :-
    !, rint:interval_(dbinom(Crit, N, Pi), Res, Flags).

%
% pbinom wrapper
%
interval_(pbinom1(K, N, Pi, string("lower")), Res, Flags) :-
    !, rint:interval_(pbinom(K, N, Pi, bool(true)), Res, Flags).

interval_(pbinom1(K, N, Pi, string("upper")), Res, Flags) :-
	!, rint:interval_(pbinom(K - number(1), N, Pi, bool(false)), Res, Flags).

interval_(pbinom1(K, N, Pi, string("densi")), Res, Flags) :-
    !, rint:interval_(dbinom(K, N, Pi), Res, Flags).

%
% pt wrapper
%
interval_(pt(A, Df, string("lower")), Res, Flags) :-
    !, rint:interval_(pt(A, Df, bool(true)), Res, Flags).

interval_(pt(A, Df, string("upper")), Res, Flags) :-
    !, rint:interval_(pt(A, Df, bool(false)), Res, Flags).

interval_(pt(A, Df, string("two.sided")), Res, Flags) :-
    !, rint:interval_(number(2) * pt(abs(A), Df, bool(false)), Res, Flags).

interval_(pt(A, Df, string("density")), Res, Flags) :-
    !, rint:interval_(dt(A, Df), Res, Flags).

%
% ancova functions for baseline and subgroups task
% a better solution might be possible
%
interval_(ancova_f(string(Outcome), Cov0, Strata0, Other0, Interaction0, Exclude0, string(Therapy)), Res, Flags) :-
    rint:interval_(Cov0, Cov1, Flags),
    rint:clean(Cov, Cov1),
    rint:interval_(Strata0, Strata1, Flags),
    rint:clean(Strata, Strata1),
    rint:interval_(Other0, Other1, Flags),
    rint:clean(Other, Other1),
    rint:interval_(Interaction0, Interaction1, Flags),
    rint:clean(Interaction, Interaction1),
    rint:interval_(Exclude0, Exclude1, Flags),
    rint:clean(Exclude, Exclude1),
    rint:eval(hook(r_session:r_topic, ancova_f(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy)), Res0),
    !, Res = number(Res0).

interval_(ancova_p(string(Outcome), Cov0, Strata0, Other0, Interaction0, Exclude0, string(Therapy)), Res, Flags) :-
    rint:interval_(Cov0, Cov1, Flags),
    rint:clean(Cov, Cov1),
    rint:interval_(Strata0, Strata1, Flags),
    rint:clean(Strata, Strata1),
    rint:interval_(Other0, Other1, Flags),
    rint:clean(Other, Other1),
    rint:interval_(Interaction0, Interaction1, Flags),
    rint:clean(Interaction, Interaction1),
    rint:interval_(Exclude0, Exclude1, Flags),
    rint:clean(Exclude, Exclude1),
    rint:eval(hook(r_session:r_topic, ancova_p(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy)), Res0),
    !, Res = number(Res0).

interval_(ancova_ci(string(Outcome), Cov0, Strata0, Other0, Interaction0, Exclude0, string(Therapy)), Res, Flags) :-
    rint:interval_(Cov0, Cov1, Flags),
    rint:clean(Cov, Cov1),
    rint:interval_(Strata0, Strata1, Flags),
    rint:clean(Strata, Strata1),
    rint:interval_(Other0, Other1, Flags),
    rint:clean(Other, Other1),
    rint:interval_(Interaction0, Interaction1, Flags),
    rint:clean(Interaction, Interaction1),
    rint:interval_(Exclude0, Exclude1, Flags),
    rint:clean(Exclude, Exclude1),
    rint:eval(hook(r_session:r_topic, ancova_ci(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy)), Res0),
    !, Res = number(Res0).


    