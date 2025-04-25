:- module(hints, [hints/3, pp_hints//3]).

:- use_module(library(http/html_write)).
:- use_module(tasks).
:- use_module(util).
:- use_module(mathml).

% return codes of correct steps
hints(_Expr-_Res/Flags, Hints) :-
    findall(H, member(step(expert, H, _), Flags), Hints).

% extract hints from multiple solutions of a given task
hints(Topic, Task, Hints) :-
    solutions(Topic, Task, Solutions),
    maplist(hints, Solutions, Hints).

% pretty print the set of hints for one given result
pp_hint(Topic, Task, _Data, Expr-Result/Flags, 1)
--> { colors(Expr, Col),
      hints(Expr-Result/Flags, Hints),
      findall(li(H),
        ( member(step(expert, Name, Arg), Flags),
          memberchk(Name, Hints),
          Topic:hint(Name, Arg, [topic(Topic), task(Task) | Col], H)
        ), List)
    },
    html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button bg-warning bg-opacity-50'), type(button), 'data-bs-toggle'(collapse),
            'data-bs-target'('#collapse-~w-~w-1'-[Topic, Task]), 'aria-expanded'(true), 'aria-controls'('collapse-~w-~w-1'-[Topic, Task])],
            "Result 1")),
        div([class('accordion-collapse collapse show'), id('collapse-~w-~w-1'-[Topic, Task]), 'data-bs-parent'('#accordion-~w-~w'-[Topic, Task])],
          div(class('accordion-body'), ul(List)))
      ])).

pp_hint(Topic, Task, _Data, Expr-Result/Flags, Id)
--> { colors(Expr, Col),
      hints(Expr-Result/Flags, Hints),
      findall(li(H),
        ( member(step(expert, Name, Arg), Flags),
          memberchk(Name, Hints),
          Topic:hint(Name, Arg, [topic(Topic), task(Task) | Col], H)
        ), List)
    },
    html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button collapsed bg-warning bg-opacity-50'), type(button), 'data-bs-toggle'(collapse),
            'data-bs-target'('#collapse-~w-~w-~w'-[Topic, Task, Id]), 'aria-expanded'(false), 'aria-controls'('collapse-~w-~w-~w'-[Topic, Task, Id])],
            "Result ~w"-[Id])),
        div([class('accordion-collapse collapse'), id('collapse-~w-~w-~w'-[Topic, Task, Id]), 'data-bs-parent'('#accordion-~w-~w'-[Topic, Task])],
          div(class('accordion-body'), ul(List)))
      ])).

pp_hints(Topic, Task, Data)
--> { member(solutions(Solutions), Data),
      findall(I-S, nth1(I, Solutions, S), Enum)
    },
    html(div(class('card card-body'),
      [ p(button([class('btn btn-warning'), type(button),
          'data-bs-toggle'(collapse), 'data-bs-target'('#hint-~w-~w'-[Topic, Task]), 
          'aria-expanded'(false), 'aria-controls'('hint-~w-~w'-[Topic, Task])],
          "Show hints")),
        div([class('card collapse'), id('hint-~w-~w'-[Topic, Task])],
          div([class(accordion), id('accordion-~w-~w'-[Topic, Task])],
            \foreach(member(I-S, Enum),
              html(\pp_hint(Topic, Task, Data, S, I)))))
      ])).
