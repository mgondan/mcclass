% Check dependencies in flags
:- module(depends, [dependencies/1, exclusive/1, compatible/1]).

% Check if there is a step for each dependency
dependencies(Flags) :-
    findall(D, member(depends(D), Flags), Dependencies),
    findall(S, member(step(_, S, _), Flags), Steps),
    subtract(Dependencies, Steps, []).

exclusive(Flags) :-
    findall(D, member(excludes(D), Flags), Excludes),
    findall(S, member(step(_, S, _), Flags), Steps),
    subtract(Excludes, Steps, Excludes).

% Check if bugs are compatible
compatible(Term) :-
    atomic(Term),
    !.

% Experimental
compatible(Term) :-
    var(Term).

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

compatible(omit(_, X)) :-
    !,
    expert(X).

% Same for dropped things
%compatible(drop_left(_, Expr)) :-
%    Expr =.. [_, L, R],
%    !,
%    expert(L),
%    compatible(R).
%
%compatible(drop_right(_, Expr)) :-
%    Expr =.. [_, L, R],
%    !,
%    expert(R),
%    compatible(L).

compatible(instead(_, Wrong, Correct)) :-
    !,
    compatible(Wrong),
    expert(Correct).

compatible(instead(_, Wrong, Correct0, Correct)) :-
    !,
    compatible(Wrong),
    expert(Correct0),
    expert(Correct).

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

expert(instead(_, _, _, _)) :-
    !,
    fail.

expert(omit_left(_, _)) :-
    !,
    fail.

expert(omit_right(_, _)) :-
    !,
    fail.

expert(omit(_, _X)) :-
    !,
    fail.

expert(drop_left(_, _)) :-
    !,
    fail.

expert(drop_right(_, _)) :-
    !,
    fail.

expert(invent_left(_, _)) :-
    !,
    fail.

expert(invent_right(_, _)) :-
    !,
    fail.

expert(Term) :-
    compound(Term),
    Term =.. [_ | Args],
    maplist(expert, Args).

