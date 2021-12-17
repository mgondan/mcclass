% Check dependencies in flags
:- module(depends, [dependencies/1, compatible/1]).

% Check if there is a name(X) or bug(X) for each depends(X)
dependencies(Flags) :-
    findall(D, member(depends(D), Flags), Dependencies),
    findall(S, member(step(_, S, _), Flags), Steps),
    subtract(Dependencies, Steps, []).

% Check if bugs are compatible
compatible(Term) :-
    atomic(Term).

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

