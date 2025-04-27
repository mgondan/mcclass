:- module(search, [searchall/3, searchdep/3, codes/2]).

:- use_module(interval/interval).
% :- use_module(interval).
:- use_module(tasks).
:- use_module(steps).
:- use_module(intermediate).
:- use_module(depends).
:- use_module(feedback).
:- use_module(mathml).

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
      ( search(Topic, Task, E, F),
        sort(F, S),
        dependencies(S),
        exclusive(S),
        codes(S, C),
        interval_ex(E, R, [topic(Topic), task(Task)]),
%       interval(E, R, [topic(Topic), task(Task)]),
        interval(available(R), true)
      ), Results),
    sort(2, @<, Results, Sorted),
    findall(E-R/F, member(res(E, R/_, F), Sorted), Expr_Res_Flags).

searchall(Topic, Task, Expr_Res_Flags) :-
    findall(res(E, R/S, F),
      ( search(Topic, Task, E, F),
        sort(F, S),
        % dependencies(S), % do not check dependencies (needed for the traps)
        interval(E, R, [topic(Topic), task(Task)])
      ), Results),
    sort(2, @<, Results, Sorted),
    findall(E-R/F, member(res(E, R/_, F), Sorted), Expr_Res_Flags).

interval_ex(E, R, Flags) :-
    interval(E, R, Flags),
    !.

interval_ex(E, _, _) :-
  throw(error(existence_error(interval, E), E)).

