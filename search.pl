:- module(search, [searchall/3, searchdep/3, codes/2]).

:- use_module(interval).
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
    findall(res(Expr, Res-Codes, Flags), 
      ( Topic:res(Task, Expr, Flags),
        sort(Flags, Sorted),
	dependencies(Sorted),
        exclusive(Sorted),
        codes(Sorted, Codes),
        interval_ex(Expr, Res, [topic(Topic), task(Task)]),
        interval(available(Res), true)
      ), Results),
    sort(2, @<, Results, Unique),
    findall(Expr-Res/Flags, member(res(Expr, Res-_, Flags), Unique), Expr_Res_Flags).

searchall(Topic, Task, Expr_Res_Flags) :-
    findall(res(Expr, Res-Sorted, Flags),
      ( search(Topic, Task, Expr, Flags),
        sort(Flags, Sorted),
        % do not check dependencies (needed for the traps)
        interval(Expr, Res, [topic(Topic), task(Task)])
      ), Results),
    sort(2, @<, Results, Unique),
    findall(Expr-Res/Flags, member(res(Expr, Res-_, Flags), Unique), Expr_Res_Flags).

interval_ex(E, R, Flags) :-
    interval(E, R, Flags),
    !.

interval_ex(E, _, _) :-
  throw(error(existence_error(interval, E), E)).

