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

% Compounds are complete 
% - if they haven't been declared as intermediate 
% - and if all their arguments are complete
complete(Task, X) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    not(intermediate(Task, Name)),
    maplist(complete(Task), Arguments).

