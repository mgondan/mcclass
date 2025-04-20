:- module(interval_wrapper, []).

:- reexport(library(rint)).

:- initialization(init).

init :-
    asserta(rint:instantiate(ci, ci(_, _))).

% Evaluate variables in R
rint:interval_hook(atomic(A), Res, _Flags) :-
    rint:r_hook(_R, A),
    !,
    rint:eval(A, Res1),
    rint:clean(Res1, Res). 

%
% Confidence intervals
%
rint:interval_hook(ci(A, B), Res, Flags) :- 
    rint:interval_(A, A1, Flags),
    rint:interval_(B, B1, Flags),
    Res = ci(A1, B1).

rint:int_hook(assign, assign3(atomic, ci), ci, []).
rint:assign3(atomic(Var), ci(atomic(A), atomic(B)), Res, _Flags) :-
    rint:eval(Var <- call("ci", A, B), Res1),
    rint:clean(Res1, Res).

rint:assign3(atomic(Var), ci(L1...U1, L2...U2), Res, _Flags) :-
    rint:eval(Var <- call("ci", call("...", L1, U1), call("...", L2, U2)), Res1),
    rint:clean(Res1, Res). 

%
% Addition (for testing)
%
rint:int_hook(plus, plus1(atomic, atomic), atomic, []).
rint:plus1(atomic(A), atomic(B), atomic(Res), _Flags) :-
    !,
    Res is A + B.

rint:int_hook(plus, plus2(..., ...), ..., []).
rint:plus2(A, B, Res, Flags) :-
    !,
    rint:interval_(A + B, Res, Flags).

%
% Fractions, i.e., numerator, line, and denominator
%
rint:int_hook(dfrac, frac0(_, _), _, []).
rint:int_hook(frac, frac0(_, _), _, []).
rint:frac0(A, B, Res, Flags) :-
    rint:option(digits(Dig), Flags, _),
    rint:interval_(round(A, atomic(Dig)), A1, Flags),
    rint:interval_(round(B, atomic(Dig)), B1, Flags),
    !,
    rint:interval_(A1 / B1, L...U, Flags),
    rint:return(L, U, Res).

%
% Reasonable number of digits
%
rint:int_hook(tstat, tstat(_), _, []).
rint:tstat(A, Res, Flags) :-
    rint:interval_(round(A, atomic(2)), Res, Flags).

rint:int_hook(hdrs, hdrs(_), _, []).
rint:hdrs(A, Res, Flags) :-
    rint:interval_(round(A, atomic(1)), Res, Flags).

rint:int_hook(chi2ratio, chi2ratio(_), _, []).
rint:chi2ratio(A, Res, Flags) :-
    rint:interval_(round(A, atomic(2)), Res, Flags).

rint:int_hook(pval, pval(_), _, []).
rint:pval(A, pval(Res), Flags) :-
    rint:interval_(A, Res, Flags).

%
% Bugs
%
% Forget parts of an expression
rint:int_hook(omit_left, left(_, _), _, [evaluate(false)]).
rint:left(_Bug, A, Res, Flags) :-
    A =.. [_Op, _L, R],
    rint:interval_(R, Res, Flags).

rint:int_hook(omit_right, right(_, _), _, [evaluate(false)]).
rint:right(_Bug, A, Res, Flags) :-
    A =.. [_Op, L, _R],
    rint:interval_(L, Res, Flags).

rint:int_hook(omit, omit(_, _), _, [evaluate(false)]).
rint:omit(_Bug, _Expr, na, _Flags).

% Instead
rint:int_hook(instead, instead1(_, _, _), _, [evaluate(false)]).
rint:instead1(_Bug, Wrong, _Correct, Res, Flags) :-
    rint:interval_(Wrong, Res, Flags).

rint:int_hook(instead, instead2(_, _, _, _), _, [evaluate(false)]).
rint:instead2(_Bug, Wrong, _Correct, _Correct0, Res, Flags) :-
    rint:interval_(Wrong, Res, Flags).

% Drop
rint:int_hook(drop_right, drop_right(_, _), _, [evaluate(false)]).
rint:drop_right(Bug, A, Res, Flags) :-
    rint:right(Bug, A, Res, Flags).

rint:int_hook(drop_left, drop_left(_, _), _, [evaluate(false)]).
rint:drop_left(Bug, A, Res, Flags) :-
    rint:left(Bug, A, Res, Flags).

% add_left, add_right, add
rint:int_hook(add_right, add(_, _), _, [evaluate(false)]).
rint:add(_Bug, A, Res, Flags) :-
    rint:interval_(A, Res, Flags).

rint:int_hook(add_left, add(_, _), _, [evaluate(false)]).

rint:int_hook(add, add(_, _), _, [evaluate(false)]).
%
% Multiply
%
rint:int_hook(dot, dot(_, _), _, []).
rint:dot(A, B, Res, Flags) :-
    rint:interval_(A * B, Res, Flags).

%
% Available: not NA
%
rint:int_hook(available, avail0(_), _, []).
rint:avail0(pval(A), Res, Flags) :-
    !,
    rint:interval_(available1(A), Res, Flags).

rint:avail0(A, Res, Flags) :-
    rint:interval_(available1(A), Res, Flags).

rint:int_hook(available1, avail1(atomic), _, []).
rint:avail1(atomic(A), Res, _Flags) :-
    (  avail2(atomic(A), _Res1)
    -> Res = true 
    ;  Res = false
    ).

avail2(atomic(A), Res),
    integer(A)
 => rint:eval(A, Res).

avail2(atomic(A), Res),
    number(A)
 => float_class(A, Class),
    dif(Class, nan),
    rint:eval(A, Res).

avail2(atomic(A), Res)
 => rint:eval(A, A1),
    rint:clean(A1, A2),
    avail2(A2, Res).
    
rint:int_hook(available1, avail3(...), _, []).
rint:avail3(A ... B, Res, _Flags)
 => avail2(atomic(A), A1),
    avail2(atomic(B), B1),
    (  rint:eval(A1, B1, _)
    -> Res = true
    ;  Res = false
    ).

rint:int_hook(available1, avail4(ci), _, []).
rint:avail4(ci(A, B), Res, Flags)
 => ( rint:interval_(available(A), true, Flags),
      rint:interval_(available(B), true, Flags)
    -> Res = true
    ;  Res = false
    ). 

rint:int_hook(available1, avail5(_), _, []).
rint:avail5([], true, _Flags).

rint:int_hook(=@=, equal0(_, _), _, []).
rint:equal0(A, pval(B), Res, Flags) :-
    !,
    rint:interval_(equal1(A, B), Res, Flags).

rint:equal0(ci(A, B), ci(C, D), Res, Flags) :-
    !,
    rint:interval_(A =@= C, Res0, Flags),
    rint:interval_(B =@= D, Res1, Flags),
    (   Res0 = true, 
        Res1 = true
    ->  Res = true
    ;   Res = false
    ).

rint:equal0(A, B, Res, Flags) :-
    rint:interval_(equal1(A, B), Res, Flags).

rint:int_hook(equal1, equal1(_, _), _, []).
rint:equal1(A, B, Res, Flags) :-
    rint:interval_(A =:= B, Res, Flags).

% Addition CI
rint:int_hook(+, ciplus1(ci, _), ci, []).
rint:ciplus1(ci(A, B), C, Res, Flags) :-
    rint:interval_(A + C, A1, Flags),
    rint:interval_(B + C, B1, Flags),
    Res = ci(A1, B1).

rint:int_hook(+, ciplus2(_, ci), ci, []).
rint:ciplus2(C, ci(A, B), Res, Flags) :-
    rint:ciplus1(ci(A, B), C, Res, Flags).

% Subtraction CI
rint:int_hook(-, ciminus(ci, _), ci, []).
rint:ciminus(ci(A, B), C, Res, Flags) :-
    rint:interval_(A - C, A1, Flags),
    rint:interval_(B - C, B1, Flags),
    Res = ci(A1, B1).

% Multiplication CI
rint:int_hook(*, cimult(ci, _), ci, []).
rint:cimult(ci(A, B), C, Res, Flags) :-
    rint:interval_(A * C, A1, Flags),
    rint:interval_(B * C, B1, Flags),
    Res = ci(A1, B1).

% Division CI
rint:int_hook(/, cidiv(ci, _), ci, []).
rint:cidiv(ci(A, B), C, Res, Flags) :-
    rint:interval_(A / C, A1, Flags),
    rint:interval_(B / C, B1, Flags),
    Res = ci(A1, B1).

% Exponential CI
rint:int_hook(exp, ciexp(ci), ci, []).
rint:ciexp(ci(A, B), Res, Flags) :-
    rint:interval_(exp(A), A1, Flags),
    rint:interval_(exp(B), B1, Flags),
    Res = ci(A1, B1).

% Plus/minus
rint:int_hook(pm, pm(_, _), ci, []).
rint:pm(A, B, Res, Flags) :-
    rint:interval_(A - B, A1, Flags),
    rint:interval_(A + B, B1, Flags),
    Res = ci(A1, B1).

% Return a one-tailed confidence interval
rint:int_hook(neginf, neginf0(_), ci, []).
rint:neginf0(A, Res, _Flags) :-
    Res = ci(A, atomic(1.0Inf)).

rint:int_hook(ninfpos, ninfpos0(_), ci, []).
rint:ninfpos0(A, Res, _Flags) :-
    Res = ci(atomic(-1.0Inf), A).

%
% Equation sign: named arguments in R functions (leave name unchanged)
%
/* rint:int_hook(=, equ(_, _), _, []).
rint:equ(Name, A, Res, Flags) :-
    rint:interval_(A, A1, Flags),
    Res = (Name = A1). */

%
% Denote
%
rint:int_hook(denote, den(_, _, _), _, [evaluate(false)]).
rint:den(_Sym, A, _Text, Res, Flags) :-
    rint:interval_(A, A1, Flags),
    Res = A1.

%
% Color
%
rint:int_hook(color, col(_, _), _, [evaluate(false)]).
rint:col(_Col, A, Res, Flags) :-
    rint:interval_(A, A1, Flags),
    Res = A1.

%
% Read intervals from input
%
rint:int_hook(input, input1(atomic), ..., []).
rint:input1(A, Res, Flags) :-
    option(digits(D), Flags, 2),
    Eps is 10^(-D)/2,
    MEps is -Eps,
    rint:interval_(A + MEps...Eps, Res, Flags).

rint:int_hook(input, input2(ci), ci, []).
rint:input2(ci(A, B), Res, Flags) :-
    option(digits(D), Flags, 2),
    Eps is 10^(-D)/2,
    MEps is -Eps,
    rint:interval_(A + MEps...Eps, A1, Flags),
    rint:interval_(B + MEps...Eps, B1, Flags),
    Res = ci(A1, B1).

%
% Other
%
rint:int_hook(';', or(_, _), _, []).
rint:or(A, B, Res, Flags) :-
    rint:interval_(A, _, Flags),
    rint:interval_(B, Res, Flags).

rint:int_hook(';', or(_, _, _), _, []).
rint:or(A, B, C, Res, Flags) :-
    rint:interval_(A, _, Flags),
    rint:interval_(B, _, Flags),
    rint:interval_(C, Res, Flags).


rint:int_hook('{}', curly(_), _, []).
rint:curly(A, Res, Flags) :-
    rint:interval_(A, Res, Flags).

%
% Upper tail, lower tail, both tails
%
% dist/3 just returns the second argument. It is needed for mathematical
% rendering expressions like P_T(X >= x; df=n-1) via mathml.
%
rint:int_hook(dist, dist(atomic, _, atomic), _, []).
rint:dist(_, X, _, Res, Flags) :-
    rint:interval_(X, Res, Flags).

%
% This forwards the tail argument to lower.tail of the R function, e.g. in
% pt(T, DF, lower.tail=TRUE)
%
rint:int_hook(tail, tail(atomic), atomic, []).
rint:tail(A, A, _).
rint:int_hook(tail, tail(atomic, atomic), atomic, []).
rint:tail(Tail, _, Tail, _Flags).

rint:int_hook(arg, arg(_, _), _, [evaluate(false)]).
rint:arg(A, _K, Res, Flags) :-
  rint:interval_(A, Res, Flags).

%
% cbinom
%
rint:r_hook(r_session:r_topic, ldbinom/3).
rint:r_hook(r_session:r_topic, udbinom/3).
rint:mono(udbinom/3, [-, +, +]).
rint:mono(ldbinom/3, [+, +, +]).

rint:int_hook(cbinom, cbinom0(_, _, _, atomic, atomic), _, []).
rint:cbinom0(Alpha, N, Pi, atomic("upper"), atomic("min"), Res, Flags) :-
    rint:interval_(qbinom(Alpha, N, Pi, atomic(false)) + atomic(1), Res, Flags).

rint:cbinom0(Alpha, N, Pi, atomic("lower"), atomic("max"), Res, Flags) :-
    rint:interval_(qbinom(Alpha, N, Pi, atomic(true)) - atomic(1), Res, Flags).

rint:cbinom0(Alpha, N, Pi, atomic("densi"), atomic("min"), Res, Flags) :-
    rint:interval_(udbinom(Alpha, N, Pi), Res, Flags).

rint:cbinom0(Alpha, N, Pi, atomic("densi"), atomic("max"), Res, Flags) :-
    rint:interval_(ldbinom(Alpha, N, Pi), Res, Flags).

%
% pwbinom
%
rint:int_hook(pwbinom, pwbinom0(_, _, _, atomic), _, []).
rint:pwbinom0(Crit, N, Pi, atomic("lower"), Res, Flags) :-
    rint:interval_(pbinom(Crit, N, Pi), Res, Flags).

rint:pwbinom0(Crit, N, Pi, atomic("upper"), Res, Flags) :-
    rint:interval_(pbinom(Crit - atomic(1), N, Pi, atomic(false)), Res, Flags).

rint:pwbinom0(Crit, N, Pi, atomic("densi"), Res, Flags) :-
    rint:interval_(dbinom(Crit, N, Pi), Res, Flags).

%
% pbinom wrapper
%
rint:int_hook(pbinom1, pbinom1(_, _, _, atomic), _, []).
rint:pbinom1(K, N, Pi, atomic("lower"), Res, Flags) :-
    rint:interval_(pbinom(K, N, Pi, atomic(true)), Res, Flags).

rint:pbinom1(K, N, Pi, atomic("upper"), Res, Flags) :-
    rint:interval_(pbinom(K, N, Pi, atomic(false)), Res, Flags).

rint:pbinom1(K, N, Pi, atomic("densi"), Res, Flags) :-
    rint:interval_(dbinom(K, N, Pi), Res, Flags).