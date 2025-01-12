% DELETE BEFORE FINAL MERGE
:- module(rint, []).

:- reexport(interval). 
:- use_module(r).
:- use_module(session).

:- multifile r_hook/1.
interval:hook(Expr, Res) :-
    r_hook(Expr),
    !, 
    r_topic(Expr, Res).

interval:monotonical(pbinom(+, -, -)).

r_hook(pbinom(_K, _N, _Pi)).

:- r_source(interval).

rint :-
    K = 10 ... 11,
    N = 20 ... 21,
    Pi = 0.50 ... 0.55,
    b_setval(topic, topic),
    interval(pbinom(K, N, Pi), Res),
    writeln(pbinom(K, N, Pi) --> Res).

