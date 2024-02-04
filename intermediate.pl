:- module(intermediate, [complete/3]).

:- use_module(tasks).

% Atoms (e.g. s_t0) are always complete
complete(_, _, X) :-
    atomic(X),
    !.

% Experimental: variables
complete(_, _, X) :-
    var(X),
    !.

% The correct part of instead(Bug, Wrong, Correct) does not need to be 
% complete.
complete(Topic, Task, instead(_Bug, Wrong, _Correct)) :-
    !,
    complete(Topic, Task, Wrong).

complete(Topic, Task, instead(_Bug, Wrong, _Correct0, Correct)) :-
    !,
    complete(Topic, Task, Wrong),
    complete(Topic, Task, Correct).

% The omitted part does not need to be complete
complete(Topic, Task, omit_left(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, _L, R],
    complete(Topic, Task, R).

complete(Topic, Task, omit_right(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, L, _R],
    complete(Topic, Task, L).

complete(_Topic, _Task, omit(_Bug, _Expr)).

complete(Topic, Task, drop_left(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, _L, R],
    complete(Topic, Task, R).

complete(Topic, Task, drop_right(_Bug, Expr)) :-
    !,
    Expr =.. [_Op, L, _R],
    complete(Topic, Task, L).

% The invented parts need to be complete
complete(Topic, Task, add_left(_Bug, Expr)) :-
    !,
    complete(Topic, Task, Expr).

complete(Topic, Task, invent_right(_Bug, Expr)) :-
    !,
    complete(Topic, Task, Expr).

% Compounds are complete 
% - if they haven't been declared as intermediate 
% - and if all their arguments are complete
complete(Topic, Task, X) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    not(Topic:intermediate(Task, Name)),
    maplist(complete(Topic, Task), Arguments).

