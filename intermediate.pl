:- module(intermediate, [final/2]).

final(Topic, A) :-
    atomic(A),
    !,
    \+ intermediate(Topic: A/0).

final(Topic, instead_of(_, Instead, _, Of, _)) :-
    !,
    final(Topic, Instead),
    final(Topic, Of).

final(Topic, denoting(_, Expr, _)) :-
    !, final(Topic, Expr).

final(Topic, expert(_ >> New)) :-
    !,
    final(Topic, New).

final(Topic, buggy(_ >> New)) :-
    !,
    final(Topic, New).

final(Topic, left_landed(_, Expr)) :-
    !,
    final(Topic, Expr).

final(Topic, right_landed(_, Expr)) :-
    !,
    final(Topic, Expr).

final(Topic, omit_left(_, Expr)) :-
    !,
    Expr =.. [_, _, R],
    final(Topic, R).

final(Topic, omit_right(_, Expr)) :-
    !,
    Expr =.. [_, L, _],
    final(Topic, L).

final(Topic, skip(_, _, Expr)) :-
    final(Topic, Expr).

final(Topic, A) :-
    compound(A),
    functor(A, Name, Arity),
    \+ intermediate(Topic: Name/Arity),
    A =.. [Name | Args],
    maplist(final(Topic), Args).

