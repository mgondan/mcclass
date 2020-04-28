:- module(intermediate, [final/1]).

final(A) :-
    atomic(A),
    !,
    \+ intermediate(A/0).

final(instead_of(_, Instead, _, Of, _)) :-
    !,
    final(Instead),
    final(Of).

final(denoting(_, Expr, _)) :-
    !, final(Expr).

final(expert(_ >> New)) :-
    !,
    final(New).

final(buggy(_ >> New)) :-
    !,
    final(New).

final(left_landed(_, Expr)) :-
    !,
    final(Expr).

final(right_landed(_, Expr)) :-
    !,
    final(Expr).

final(omit_left(_, Expr)) :-
    !,
    compound_name_arguments(Expr, _, [_, R]),
    final(R).

final(omit_right(_, Expr)) :-
    !,
    compound_name_arguments(Expr, _, [L, _]),
    final(L).

final(A) :-
    compound(A),
    functor(A, Name, Arity),
    \+ intermediate(Name/Arity),
    A =.. [Name | Args],
    maplist(final, Args).





















