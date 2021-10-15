:- module(intermediate, [complete/2]).

:- use_module(tasks).

% Atoms (e.g. s_t0) are always complete
complete(_, X) :-
    atomic(X),
    !.

% Experimental: variables
complete(_, X) :-
    var(X),
    !.

% The correct part of instead(Bug, Wrong, Correct) does not need to be 
% complete.
complete(Task, instead(_Bug, Wrong, _Correct)) :-
    !,
    complete(Task, Wrong).

% The omitted part does not need to be complete
complete(Task, omit_left(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, _L, R],
    complete(Task, R).

complete(Task, omit_right(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, L, _R],
    complete(Task, L).

% Compounds are complete 
% - if they haven't been declared as intermediate 
% - and if all their arguments are complete
complete(Task, X) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    not(intermediate(Task, Name)),
    maplist(complete(Task), Arguments).

