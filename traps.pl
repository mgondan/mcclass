:- module(traps, [init_traps/1, show_traps//2]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/http_log)).
:- use_module(util).
:- use_module(mathml).
:- use_module(steps).

% Determine traps at module startup
% 
% These bugs are critical in the sense that if you commit them, you lost the
% path to the correct solution. Any other bugs that occur downstream (e.g., in
% tpaired, the school bug that may occur in the formula of the independent
% t-test) are less relevant for feedback, we only use them to understand what
% the user is doing.
init_traps(Topic) :-
    dynamic(Topic:traps/2),
    dynamic(Topic:trap/4),
    dynamic(Topic:trap_codes/2),
    foreach(init_traps1(Topic), true),
    foreach(init_traps2(Topic), true).

init_traps1(Topic) :-
    Topic:task(Task),
    findall(trap(Task-Name, Expr), 
      ( search(Topic, Task, Expr, Steps),
        findall(Name, member(step(buggy, Name, _), Steps), [Name])
      ), Traps),
    sort(1, @<, Traps, Unique),
    findall(Name, member(trap(_-Name, _), Unique), TrapCodes),
    assert(Topic:trap_codes(Task, TrapCodes)),
    http_log("Trap codes ~w~n", [TrapCodes]),
    foreach(
      ( member(trap(Task-Name, Expr), Unique),
        colors(Expr, Colors),
        assert(Topic:trap(Task, Expr, Name, Colors))
      ), true).

% Pretty print
init_traps2(Topic) :-
    Topic:task(Task),
    findall(li(Trap),
      ( Topic:trap(Task, _, Name, _),
        Topic:hint(Name, [topic(Topic), task(Task)], Trap)
      ), Traps),
    Card = div(class(card),
      [ div(class('card-header text-white bg-warning'), "Traps"),
        div(class('card-body'),
          [ p(class('card-text'), "Avoid these traps"),
            p(class('card-text'), ul(Traps))
          ])
      ]),
    phrase(html(Card), HTML),
    with_output_to(string(S), print_html(HTML)),
    assert(Topic:traps(Task, S)).

show_traps(Topic, Task)
--> { Topic:traps(Task, S) },
    html(\[S]).
