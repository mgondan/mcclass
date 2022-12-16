:- module(rint, []).

:- use_module(interval).
:- use_module(r).

:- multifile r_hook/1.
interval:hook(Expr, Res) :-
    r_hook(Expr),
    !, r_task(Expr, Res).

interval:monotonical(pbinom(+, -, -)).

% interval:hook(pbinom(K, N, Pi), Res) :-
%     !, r(pbinom(K, N, Pi), Res).

r_hook(pbinom(_K, _N, _Pi)).

rint :-
    K = 10 ... 11,
    N = 20 ... 21,
    Pi = 0.50 ... 0.55,
    interval(pbinom(K, N, Pi), Res),
    writeln(pbinom(K, N, Pi) --> Res).

