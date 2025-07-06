:- module(mistakes, [init_mistakes/1, show_mistakes//3]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(util).
:- use_module(mathml).
:- use_module(steps).

show_wrong(Topic, Task, wrong(_Expr, Result, _Flags, Colors, String))
--> { phrase(html(\mmlm([topic(Topic), task(Task), error(highlight) | Colors], Result)), HTML),
      with_output_to(string(Res), print_html(HTML))
    },
    html(\[String-Res]).

show_mistakes(Topic, Task, Data)
--> { memberchk(mistakes(Mistakes), Data) },
    html(div(class(card),
      [ div(class('card-header text-white bg-danger'), "Incorrect responses"),
        div(class('card-body'),
          [ p(class('card-text'), "The system detects the following incorrect responses"),
            div(class('accordion accordion-flush'),
              \foreach(member(S, Mistakes), html(\show_wrong(Topic, Task, S))))
          ])
      ])).

% Determine mistakes at module startup
init_mistakes(Topic) :-
    dynamic(Topic:wrong/5),
    foreach(init_mistakes1(Topic), true).

init_mistakes1(Topic) :-
    Topic:task(Task),
    % member(traps(Traps), Data),
    search([expert, buggy], Topic, Task, Expr, Steps),
    memberchk(step(buggy, _, _), Steps),
    colors(Expr, Colors),
    findall(li(F),
      ( member(step(_, Name, Args), Steps),
        % memberchk(Name, Traps), % show only relevant feedback
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
      ), Feedback),
    AccItem = html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button mistake'), type(button)], "~w")),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'),
           [ p(\mmlm([topic(Topic), task(Task), error(highlight) | Colors], Expr)), 
             ul(Feedback)
           ]))
      ])),
    phrase(AccItem, HTML),
    with_output_to(string(S), print_html(HTML)),
    assert(Topic:wrong(Task, Expr, Steps, Colors, S)).

