:- module(traps, [init_traps/1]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(util).
:- use_module(mathml).
:- use_module(steps).

% Determine traps at module startup
init_traps(Topic) :-
    dynamic(Topic:trap/4),
    foreach(init_traps1(Topic), true).

init_traps1(Topic) :-
    Topic:task(Task),
    findall(trap(Task-Name, Expr), init_traps(Topic, Task, Expr, Name), Traps),
    sort(1, @<, Traps, Unique),
    foreach(
      ( member(trap(Task-Name, Expr), Unique),
        colors(Expr, Colors),
        assert(Topic:trap(Task, Expr, Name, Colors))
      ), true).

init_traps(Topic, Task, Expr, Name) :-
    search(Topic, Task, Expr, Steps),
    findall(Name, member(step(buggy, Name, _), Steps), [Name]).
