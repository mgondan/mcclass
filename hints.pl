:- module(hints, [init_hints/1, init_hints/2, pp_hints//2]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(util).
:- use_module(mathml).

pp_hints(Topic, Task)
--> { Topic:hints(Task, _, Accordion) },
    html(Accordion).

% Codes of correct steps
init_hints(Topic, Task) :-
    findall(sol(Task, Expr, Steps), Topic:sol(Task, Expr, Steps), Solutions),
    nth1(Id, Solutions, sol(Task, Expr, Steps)),
    init_hint(Topic, Task, Expr, Steps, Id).

init_hint(Topic, Task, Expr, Steps, 1) :-
    colors(Expr, Col),
    findall(H, member(step(expert, H, _), Steps), Hints),
    findall(li(Hint),
      ( member(step(expert, H, Arg), Steps),
        Topic:hint(H, Arg, [topic(Topic), task(Task) | Col], Hint)
      ), List),
    AccItem = div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button hint'), type(button),
              'data-bs-toggle'(collapse),
              'data-bs-target'('#collapse-~w-~w-1'-[Topic, Task]),
              'aria-expanded'(true),
              'aria-controls'('collapse-~w-~w-1'-[Topic, Task])],
            "Result 1")),
        div([class('accordion-collapse collapse show'),
            id('collapse-~w-~w-1'-[Topic, Task]),
            'data-bs-parent'('#accordion-~w-~w'-[Topic, Task])],
          div(class('accordion-body'), ul(List)))
      ]),
    assert(Topic:hints(Task, Expr, Hints, AccItem)).

init_hint(Topic, Task, Expr, Steps, Id) :-
    Id > 1,
    colors(Expr, Col),
    findall(H, member(step(expert, H, _), Steps), Hints),
    findall(li(Hint),
      ( member(step(expert, H, Arg), Steps),
        Topic:hint(H, Arg, [topic(Topic), task(Task) | Col], Hint)
      ), List),
    AccItem = div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button collapsed hint'), type(button),
              'data-bs-toggle'(collapse),
              'data-bs-target'('#collapse-~w-~w-~w'-[Topic, Task, Id]),
              'aria-expanded'(false),
              'aria-controls'('collapse-~w-~w-~w'-[Topic, Task, Id])],
            "Result ~w"-[Id])),
        div([class('accordion-collapse collapse'),
            id('collapse-~w-~w-~w'-[Topic, Task, Id]),
            'data-bs-parent'('#accordion-~w-~w'-[Topic, Task])],
          div(class('accordion-body'), ul(List)))
      ]),
    assert(Topic:hints(Task, Expr, Hints, AccItem)).

% pretty print the set of hints for one given result
init_hints(Topic) :-
    Topic:task(Task),
    foreach(init_hints(Topic, Task), true),
    findall(Hint-Item, Topic:hints(Task, _Expr, Hint, Item), Pairs),
    pairs_keys_values(Pairs, Hints, AccItems),
    Accordion = div(class('card card-body'),
      [ p(button([class('btn btn-warning'), type(button),
            'data-bs-toggle'(collapse),
            'data-bs-target'('#hint-~w-~w'-[Topic, Task]),
            'aria-expanded'(false),
            'aria-controls'('hint-~w-~w'-[Topic, Task])],
          "Show hints")),
        div([class('card collapse'), id('hint-~w-~w'-[Topic, Task])],
          div([class(accordion), id('accordion-~w-~w'-[Topic, Task])],
            AccItems))
      ]),
    assert(Topic:hints(Task, Hints, Accordion)).

