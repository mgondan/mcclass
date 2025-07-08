:- module(solutions, [init_solutions/1, show_solutions//3]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(util).
:- use_module(mathml).
:- use_module(steps).

show_sol(Topic, Task, sol(_Expr, Result, _Flags, Colors, String))
--> { phrase(html(\mmlm([topic(Topic), task(Task) | Colors], Result)), HTML),
      with_output_to(string(Res), print_html(HTML))
    },
    html(\[String-Res]).

show_solutions(Topic, Task, Data)
--> { memberchk(solutions(Solutions), Data) },
    html(div(class(card),
      [ div(class('card-header text-white bg-success'), "Solution(s)"),
        div(class('card-body'),
          [ p(class('card-text'), "The system accepts the following correct response(s)"),
            div(class('accordion accordion-flush'),
              \foreach(member(S, Solutions), html(\show_sol(Topic, Task, S))))
          ])
      ])).

% Determine solutions at module startup
init_solutions(Topic) :-
    dynamic(Topic:sol/5),
    foreach(init_sol(Topic), true).

init_sol(Topic) :-
    Topic:task(Task),
    search([expert], Topic, Task, Expr, Steps),
    colors(Expr, Colors),
    findall(li(F),
      ( member(step(expert, Name, Args), Steps),
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
      ), Feedback),
    AccItem = html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button'), type(button)], "~w")),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'),
           [ p(\mmlm([topic(Topic), task(Task) | Colors], Expr)), ul(Feedback) ]
          )
        )
      ])),
    phrase(AccItem, HTML),
    with_output_to(string(S), print_html(HTML)),
    assert(Topic:sol(Task, Expr, Steps, Colors, S)).
