% Interval arithmetics in R
:- module(interval, [r_int/2]).

:- reexport(r).
:- use_module(session).
:- use_module(library(http/http_log)).

r_int(Expr, Res) :-
    init,
    r(int(Expr), R),
    findall('...'(L, U), member(#(L, U), R), Res).

init :-
    session_data(init(interval)),
    !.

init :-
    r(source("interval.R")),
    session_assert(init(interval)).

test :-
    init,
    r('<-'(d, list(c(3.3, 3.4), c(3.3, 3.4)))),
    r('<-'(mu, list(2.5))),
    r('<-'(s, list(c(1.9, 1.9)))),
    r('<-'(n, list(c(20, 20)))),
    r_int(dfrac(d - mu, s / sqrt(n)), Res),
    writeln(Res).

