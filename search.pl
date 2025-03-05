:- module(search, [search/4, searchall/3, searchdep/3, codes/2]).

:- use_module(library(mcclass)).
% :- use_module(interval).
:- use_module(tasks).
:- use_module(steps).
:- use_module(intermediate).
:- use_module(depends).
:- use_module(feedback).
:- use_module(mathml).

% Reached the goal
search_(_, _, _, Y, Y, []).

% Continue search
search_(Topic, Task, Stage, X, Y, Path) :-
    step(Topic, Task, Stage, X, Z, Flags),
    search_(Topic, Task, Stage, Z, Y, Steps),
    append(Flags, Steps, Path).

% Return a solution for a given task
%
% Search is done in three stages to avoid redundancies. Typical buggy rules for
% stage(1) are mix-up of parameters (see examples in tpaired.pl). At stage(2),
% bugs refer to wrong steps in the calculation method. In the end, we check 
% if the solution is complete (not intermediate). The flags are sorted to allow
% elimination of redundant solutions that occur within stages (e.g., 
% permutations)
search(Topic, Task, Expr, Flags) :-
    search(Topic, Task, Expr, Flags, _).

search(Topic, Task, Expr, Flags, Sorted) :-
    Topic:start(X0),
    search_(Topic, Task, stage(1), X0, X1, Flags1),
    search_(Topic, Task, stage(2), X1, X2, Flags2),
    search_(Topic, Task, stage(3), X2, Expr, Flags3),
    complete(Topic, Task, Expr),    % no intermediate solutions
    compatible(Expr),               % no incompatible bugs
    append([Flags1, Flags2, Flags3], Flags),
    sort(Flags, Sorted).

% Codes for steps
codes(Steps, Codes) :-
    findall(C, member(step(_Type, C, _Args), Steps), Codes).

% Return all solutions for a given task
%
% The sort/4 in the 2nd-to-last line eliminates redundant solutions 
% (redundant = same flags and same numerical result).
%
% Moreover, solutions with NA as numerical result are eliminated.
searchdep(Topic, Task, Expr_Res_Flags) :-
    findall(res(E, R/C, F), 
      ( search(Topic, Task, E, F, S),
        dependencies(S),            % check dependencies here
        exclusive(S),
        codes(S, C),
        interval(E, R, [topic(Topic), task(Task)]),
        interval(available(R), true)
      ), Results),
    sort(2, @<, Results, Sorted),
    findall(E-R/F, member(res(E, R/_, F), Sorted), Expr_Res_Flags).

searchall(Topic, Task, Expr_Res_Flags) :-
    findall(res(E, R/S, F),
      ( search(Topic, Task, E, F, S),
        % dependencies(S),          % do not check dependencies (needed for the traps)
        interval(E, R, [topic(Topic), task(Task)])
      ), Results),
    sort(2, @<, Results, Sorted),
    findall(E-R/F, member(res(E, R/_, F), Sorted), Expr_Res_Flags).

