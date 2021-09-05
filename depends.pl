% Check dependencies in flags
:- module(depends, [dependencies/1, compatible/1]).

% Check if there is a name(X) or bug(X) for each depends(X)
dependencies(Flags) :-
    findall(D, member(depends(D), Flags), Dependencies),
    findall(D, (member(name(D), Flags) ; member(bug(D), Flags)), Bugs),
    subtract(Dependencies, Bugs, []).

% Check if bugs are compatible
compatible(Term) :-
    atomic(Term).

% No bugs in omitted things
compatible(omit_left(_, Expr)) :-
    Expr =.. [_, L, R],
    !,
    expert(L),
    compatible(R).

compatible(omit_right(_, Expr)) :-
    Expr =.. [_, L, R],
    !,
    expert(R),
    compatible(L).

compatible(Term) :-
    compound(Term),
    Term =.. [_ | Args],
    maplist(compatible, Args).

% Check for bugs in (sub)expressions
expert(Term) :-
    atomic(Term).

expert(instead(_, _, _)) :-
    !,
    fail.

expert(omit_left(_, _)) :-
    !,
    fail.

expert(omit_right(_, _)) :-
    !,
    fail.

expert(Term) :-
    compound(Term),
    Term =.. [_ | Args],
    maplist(expert, Args).

