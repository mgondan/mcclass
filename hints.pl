:- module(hints, [init_hints/1, show_hints//2]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(util).
:- use_module(mathml).

show_hints(Topic, Task)
--> { Topic:hints(Task, Accordion) },
    html(Accordion).

% Codes of correct steps
init_hints(Topic, Task, Solutions) :-
    nth1(Id, Solutions, sol(Task, Expr, Steps, Colors)),
    init_hint(Topic, Task, Expr, Steps, Colors, Id, Solutions).

init_hint(Topic, Task, Expr, Steps, Colors, _, [_]) :-
    findall(H, member(step(expert, H, _), Steps), Hints),
    findall(li(Hint),
      ( member(step(expert, H, Arg), Steps),
        Topic:hint(H, Arg, [topic(Topic), task(Task) | Colors], Hint)
      ), List),
    AccItem = div(class('accordion-item'),
      div([class('accordion-collapse collapse show'),
          id('collapse-~w-~w-1'-[Topic, Task]),
          'data-bs-parent'('#accordion-~w-~w'-[Topic, Task])],
        div(class('accordion-body'), ul(List)))),
    assert(Topic:hints(Task, Expr, Hints, AccItem)).

init_hint(Topic, Task, Expr, Steps, Colors, 1, [_, _ | _]) :-
    findall(H, member(step(expert, H, _), Steps), Hints),
    findall(li(Hint),
      ( member(step(expert, H, Arg), Steps),
        Topic:hint(H, Arg, [topic(Topic), task(Task) | Colors], Hint)
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

init_hint(Topic, Task, Expr, Steps, Colors, Id, [_, _ | _]) :-
    Id > 1,
    findall(H, member(step(expert, H, _), Steps), Hints),
    findall(li(Hint),
      ( member(step(expert, H, Arg), Steps),
        Topic:hint(H, Arg, [topic(Topic), task(Task) | Colors], Hint)
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
    findall(sol(Task, Expr, Steps, Colors), Topic:sol(Task, Expr, Steps, Colors), Solutions),
    foreach(init_hints(Topic, Task, Solutions), true),
    findall(Hint-Item, Topic:hints(Task, _Expr, Hint, Item), Pairs),
    pairs_keys_values(Pairs, _Hints, AccItems),
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
    assert(Topic:hints(Task, Accordion)).

