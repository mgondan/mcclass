:- module(intermediate, [complete/2]).

:- multifile intermediate/2.

% Atoms (e.g. s_t0) are always complete
complete(_, X) :-
    atomic(X).

% Compounds are complete 
% - if they haven't been declared as intermediate 
% - and if all their arguments are complete
complete(Task, X) :-
    compound(X),
    compound_name_arguments(X, Name, Arguments),
    not(intermediate(Task, Name)),
    maplist(complete(Task), Arguments).

