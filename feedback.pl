% Collect feedback from Flags
:- module(feedback, [praise/4, blame/4, show_feedback//3]).

:- use_module(library(quantity)).
:- use_module(interval).
:- use_module(util).
:- use_module(library(http/http_log)).

% This may become more complex if we change the representation.
praise(Task, Flags, Col, Praise) :-
    findall(P, (member(step(expert, N, Args), Flags), Task:feedback(N, Args, Col, P)), Praise).

blame(Task, Flags, Col, Blame) :-
    findall(B, (member(step(buggy, N, Args), Flags), Task:feedback(N, Args, Col, B)), Blame).

% Correct response
show_feedback(Topic, Task, Data)
--> { session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(solutions(Solutions), Data),
      member(sol(_Expr, Res, Flags, Colors, _S), Solutions),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Colors]),
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
        ), Items)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-success'), "Congratulations"),
        div(class('card-body'),
          [ p(class('card-text'), "Correct response!"), ul(Items) ])
      ])).

% Buggy response
show_feedback(Topic, Task, Data)
--> { session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(mistakes(Mistakes), Data),
      member(wrong(Expr, Res, Flags, Colors, _S), Mistakes),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Colors]),
      % Relevant feedback
      Topic:hint_codes(Task, Hints),
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          memberchk(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
        ), Correct0),
      ( Correct0 = []
        -> Correct = p(class('card-text'), "")
         ; Correct = p(class('card-text'),
             [ "Correct steps", ul(class('card-text'), ul(Correct0)) ])
      ),
      % Same for incorrect steps
      Topic:trap_codes(Task, Traps),
      findall(li(F),
        ( member(step(buggy, Name, Args), Flags),
	  memberchk(Name, Traps),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Wrong0),
      ( Wrong0 = []
        -> Wrong = p(class('card-text'), "")
         ; Wrong = p(class('card-text'),
             [ "Wrong steps", ul(class('card-text'), ul(Wrong0)) ])
      ),
      % Irrelevant feedback
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          \+ member(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Praise0),
      ( Praise0 = []
        -> Praise = p(class('card-text'), "")
         ; Praise = p(class('card-text'),
             [ "Other praise", ul(class('card-text'), ul(Praise0)) ])
      ),
      % Same for mistakes
      findall(li(F),
        ( member(step(buggy, Name, Args), Flags),
          \+ member(Name, Traps),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Blame0),
      ( Blame0 = []
        -> Blame = p(class('card-text'), "")
         ; Blame = p(class('card-text'),
             [ "Other mistakes", ul(class('card-text'), ul(Blame0)) ])
      )
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-warning'), "Careful"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the correct expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(fix) | Colors], Expr)),
            p(class('card-text'), "Your response matches the following expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(highlight) | Colors], Expr)),
            Correct, Wrong, Praise, Blame
          ])
      ])).

show_feedback(Topic, Task, _Data)
--> { session_data(resp(Topic, Task, R)),
      quantity(N, _Opt, R)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-warning'), "Careful"),
        div(class('card-body'),
          p(class('card-text'), "The response ~p is not correct and cannot be attributed to any known mistake."-[N]))
      ])).

show_feedback(Topic, Task, _Data)
--> { session_data(resp(Topic, Task, R)) },
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Response not recognized: ~p"-[R]))
      ])).

show_feedback(_Topic, _Task, _Data)
--> html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Waiting for response..."))
      ])).
