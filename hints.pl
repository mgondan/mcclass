:- module(hints, [hints/3, pp_hints//3]).

:- use_module(library(http/html_write)).
:- use_module(tasks).

% return codes of correct steps
hints(_Expr-_Res/Flags, Hints) :-
    findall(H, member(step(expert, H, _), Flags), Hints).

% extract hints from multiple solutions of a given task
hints(Topic, Task, Hints) :-
    solutions(Topic, Task, Solutions),
    maplist(hints, Solutions, Hints).

% pretty print the set of hints for one given result
pp_hint1(Topic, Task, _Data, Expr-Result/Flags)
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
          button([class('accordion-button'), type(button)],
            \mmlm([topic(Topic), task(Task) | Col], Result))),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'), ul(List)))
      ])).

% pretty print the hints for all alternatives
pp_hints1(Topic, Task, Data)
--> { member(solutions(Solutions), Data) },
    html(div(class(card),
      [ div(class('card-header text-white bg-info'), "Hints"),
        div(class('card-body'),
          [ p(class('card-text'), "Steps to the solution(s)"),
            div(class('accordion accordion-flush'),
              html(\foreach(member(S, Solutions),
                html(\pp_hint(Topic, Task, Data, S)))))
          ])
      ])).

% pretty print the set of hints for one given result
pp_hint(Topic, Task, _Data, Expr-Result/Flags)
--> { colors(Expr, Col),
      hints(Expr-Result/Flags, Hints),
      findall(li(H),
        ( member(step(expert, Name, Arg), Flags),
          memberchk(Name, Hints),
          Topic:hint(Name, Arg, [topic(Topic), task(Task) | Col], H)
        ), List)
    },
    html(div([class(collapse), id('hint-~w-~w'-[Topic, Task])],
        div(class('card card-body'), ul(List)))).

pp_hints(Topic, Task, Data)
--> { member(solutions(Solutions), Data),
      [ Sol1 | _ ] = Solutions
    },
    html(div(class('card card-body'),
      [ p(button([class('btn btn-primary'), type(button),
          'data-bs-toggle'(collapse), 'data-bs-target'('#hint-~w-~w'-[Topic, Task]), 
          'aria-expanded'(false), 'aria-controls'('hint-~w-~w'-[Topic, Task])],
          "Show hints")),
        html(\pp_hint(Topic, Task, Data, Sol1))
      ])).

