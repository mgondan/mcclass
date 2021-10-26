% Interval arithmetics in R
:- module(interval, [r_session_int/2, matches/2]).

:- reexport(r).
:- use_module(session).
:- use_module(library(http/http_log)).

member_('...'(L, U), R) :-
    member(#(L, U), R).

member_('...'(M, M), R) :-
    member(#(M), R).

r_session_int(Expr, Res) :-
    init,
    r_session(int(Expr), R),
    findall('...'(L, U), member_('...'(L, U), R), Res).

matches(Number, ['...'(L, U) | _]) :-
    floor(100*L) =< 100*Number,
    100*Number =< ceiling(100*U).

matches(Number, [_ | T]) :-
    matches(Number, T).

init :-
    session_data(interval),
    !.

init :-
    r(source("interval.R")),
    session_assert(interval).

test :-
    init,
    r_session('<-'(d, list(c(3.3, 3.4), c(3.3, 3.4)))),
    r_session('<-'(mu, list(2.5))),
    r_session('<-'(s, list(c(1.9, 1.9)))),
    r_session('<-'(n, list(c(20, 20)))),
    r_session_int(dfrac(d - mu, s / sqrt(n)), Res),
    writeln(Res).

