:- module(tasks, [task/3, feedback//4, solutions/3, pp_solutions//3,  
    pp_wrongs//3, pp_traps//3, download/1
  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_log)).
:- use_module(library(dcg/high_order)).
:- use_module(mathml).
:- use_module(search).
:- use_module(depends).
:- use_module(steps).
:- use_module(r_session).
:- use_module(interval).
:- use_module(session).
:- use_module(library(quantity)).
:- use_module(hints).
:- use_module(util).

user:term_expansion(mono(A, B), rint:mono(A, B)).
user:term_expansion(r_hook(A), rint:r_hook(r_session:r_topic, A)).

use_topic(Topic) :-
    use_module(Topic),
    dynamic(Topic:math_hook/2),
    foreach(init_sol(Topic), true),
    foreach(init_hints(Topic), true),
    foreach(init_wrong(Topic), true).

% Determine solutions at module startup
init_sol(Topic) :-
    Topic:task(Task),
    search([expert], Topic, Task, Expr, Steps),
    colors(Expr, Colors),
    assert(Topic:sol(Task, Expr, Steps, Colors)).

% Same for incorrect results
init_wrong(Topic) :-
    Topic:task(Task),
    search([expert, buggy], Topic, Task, Expr, Steps),
    memberchk(step(buggy, _, _), Steps),
    colors(Expr, Colors),
    assert(Topic:wrong(Task, Expr, Steps, Colors)).

:- use_topic(tpaired).
:- use_topic(tpairedupper).
:- use_topic(tpairedlower).
:- use_topic(baseline).
:- use_topic(oddsratio).
:- use_topic(oddsratio2).
:- use_topic(easyodds).
:- use_topic(tgroups).
:- use_topic(ztrans).
:- use_topic(dbinom).
:- use_topic(testbinom).
:- use_topic(chisq).
:- use_topic(subgroups).
:- use_topic(regression).

% Render R result
mathml:math_hook(r(Expr), Res) :-
    r_topic(Expr, Res).

% Gather useful information
%
% 1. Identify (correct) solution
% 2. Steps in (1) = Hints
% 3. Identify (incorrect) alternatives
% 4. Subset of (3) with exactly one buggy rule
% 5. Buggy steps in (4) = Traps
%
% more to come
task(Topic, Task, Data) :-
    session_data(taskdata(Topic, Task, D)),
    !, Data=D.

task(Topic, Task, Data) :-
    r_init_session,
    r_session_source(Topic),
    solutions(Topic, Task, Solutions),
    wrongs(Topic, Task, E_R_F),
    wrongall(Topic, Task, E_R_F_All),
    traps(E_R_F_All, Traps),
    Data = task(Topic, Task, 
      [ solutions(Solutions), 
        wrong(E_R_F),
        wrongall(E_R_F_All), % this needs a better solution
        traps(Traps)
      ]),
    session_assert(taskdata(Topic, Task, Data)).

% Correct response
feedback(Topic, Task, Data, _Form)
--> { % option(resp(R), Form),
      session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(solutions(Solutions), Data),
      member(sol(_Expr, Res, Flags, Colors), Solutions),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Colors]),
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], FB)
        ), Items)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-success'), "Congratulations"),
        div(class('card-body'),
          [ p(class('card-text'), "Correct response!"),
            ul(Items)
          ])
      ])).

% Buggy response
feedback(Topic, Task, Data, _Form)
--> { % option(resp(R), Form),
      session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(wrongall(Wrongs), Data),
      member(Expr-Res/Flags, Wrongs),
      colors(Expr, Col),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Col]),
      member(traps(Traps), Data),
      findall(H, Topic:hints(Task, _, H, _), Hints0),
      append(Hints0, Hints1),
      sort(Hints1, Hints),
      % relevant feedback
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          memberchk(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Col], FB)
        ), Correct0),
      ( Correct0 = []
        -> Correct = p(class('card-text'), "")
         ; Correct = p(class('card-text'), 
                       [ "Correct steps",
                         ul(class('card-text'), ul(Correct0))
                       ])
      ),
      findall(li(FB),
        ( member(step(buggy, Name, Args), Flags),
          memberchk(Name, Traps),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Col], FB)
        ), Wrong0),
      ( Wrong0 = []
        -> Wrong = p(class('card-text'), "")
         ; Wrong = p(class('card-text'),
                       [ "Wrong steps",
                         ul(class('card-text'), ul(Wrong0))
                       ])
      ),
      % irrelevant feedback
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          \+ memberchk(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Col], FB)
        ), Praise0),
      ( Praise0 = []
        -> Praise = p(class('card-text'), "")
         ; Praise = p(class('card-text'),
                       [ "Other praise",
                         ul(class('card-text'), ul(Praise0))
                       ])
      ),
      findall(li(FB),
        ( member(step(buggy, Name, Args), Flags),
          \+ memberchk(Name, Traps),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Col], FB)
        ), Blame0),
      ( Blame0 = []
        -> Blame = p(class('card-text'), "")
         ; Blame = p(class('card-text'),
                       [ "Other mistakes",
                         ul(class('card-text'), ul(Blame0))
                       ])
      )
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-warning'), "Careful"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the correct expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(fix) | Col], Expr)),
            p(class('card-text'), "Your response matches the following expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(highlight) | Col], Expr)),
            Correct, Wrong, Praise, Blame
          ])
      ])).

feedback(Topic, Task, _Data, _Form) -->
  { % option(resp(R), Form),
    session_data(resp(Topic, Task, R)),
    quantity(N, _Opt, R)
  },
  html(div(class(card),
    [ div(class('card-header text-white bg-warning'), "Careful"),
      div(class('card-body'),
        p(class('card-text'), "The response ~p is not correct and cannot be attributed to any known mistake."-[N]))
    ])).

feedback(Topic, Task, _Data, _Form) -->
    { % option(resp(R), Form),
      session_data(resp(Topic, Task, R))
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Response not recognized: ~p"-[R]))
      ])).

feedback(_Topic, _Task, _Data, _Form) -->
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Waiting for response..."))
      ])).

% Solutions with numerical results
solutions(Topic, Task, List) :-
    findall(s(Expr, Res-Codes, Flags, Colors),
      ( Topic:sol(Task, Expr, Flags, Colors),
        interval(Expr, Res, [topic(Topic)]),
        sort(Flags, Sorted),
        codes(Sorted, Codes)
      ), List1),
    % avoid duplicates by permutations
    sort(2, @<, List1, List2),
    findall(sol(Expr, Res, Flags, Colors), member(s(Expr, Res-_, Flags, Colors), List2), List).

% Incorrect results
wrongs(Topic, Task, List) :-
    findall(s(Expr, Res-Codes, Flags, Colors),
      ( Topic:wrong(Task, Expr, Flags, Colors),
        interval(Expr, Res, [topic(Topic)]),
        sort(Flags, Sorted),
	dependencies(Sorted),
	exclusive(Sorted),
        codes(Sorted, Codes)
      ), List1),
    % avoid duplicates by permutations
    sort(2, @<, List1, List2),
    findall(wrong(Expr, Res, Flags, Colors), member(s(Expr, Res-_, Flags, Colors), List2), List).

% Todo: prepare traps at module initialization

% Pretty print
pp_solution(Topic, Task, sol(Expr, Result, Flags, Colors))
--> { findall(li(FB),
      ( member(step(expert, Name, Args), Flags),
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], FB)
      ), Items)
    },
    html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button'), type(button)], 
            \mmlm([topic(Topic), task(Task) | Colors], Result))),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'), 
           [ p(\mmlm([topic(Topic), task(Task) | Colors], Expr)), 
             ul(Items)
           ]))
      ])).

pp_solutions(Topic, Task, Data)
--> { member(solutions(Expr_Res_Flags), Data) },
    html(div(class(card),
      [ div(class('card-header text-white bg-success'), "Solution(s)"),
        div(class('card-body'),
          [ p(class('card-text'), "The system accepts the following correct response(s)"),
            div(class('accordion accordion-flush'), 
              \foreach(member(ERF, Expr_Res_Flags), html(\pp_solution(Topic, Task, ERF))))
          ])
      ])).

% Pretty print
pp_wrong(Topic, Task, Data, wrong(_Expr, _Res, Flags, Col), Items) :-
    member(traps(Traps), Data),
    findall(li(FB), 
      ( member(step(_, Name, Args), Flags),
        memberchk(Name, Traps), % show only relevant feedback
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Col], FB)
      ), Items).

pp_wrongs(Topic, Task, Data)
--> { member(wrong(Expr_Res_Flags), Data),
      findall(
        li([ \mmlm([topic(Topic), task(Task), error(highlight) | Col], E = R), ul(FB) ]), 
        ( member(wrong(E, R, F, Col), Expr_Res_Flags),
          pp_wrong(Topic, Task, Data, wrong(E, R, F, Col), FB)
        ), List)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-danger'), "Wrong alternatives"),
        div(class('card-body'),
          [ p(class('card-text'), "These wrong responses are recognized by the system"),
            p(class('card-text'), ol(List))
          ])
      ])).

% Succeeds if Flags include exactly one (critical) bug
%
% These bugs are critical in the sense that if you commit them, you lost the
% path to the correct solution. Any other bugs that occur downstream (e.g., in
% tpaired, the school bug that may occur in the formula of the independent 
% t-test) are less relevant for feedback, we only use them to understand what 
% the user is doing.

% The incorrect response alternatives (without check for dependencies)
wrongall(Topic, Task, Expr_Res_Flags) :-
    searchall(Topic, Task, E_R_F),
    findall(E-R/F,
      ( member(E-R/F, E_R_F),
        memberchk(step(buggy, _, _), F)
      ), Expr_Res_Flags).

trap(Flags, Trap) :-
    findall(T, member(step(buggy, T, _), Flags), [Trap]).

% Codes for wrong steps
traps(E_R_F, Sorted) :-
    findall(T, (member(_E-_R/Flags, E_R_F), trap(Flags, T)), Traps),
    % Duplicates occur if there are multiple solutions
    sort(Traps, Sorted).

% Pretty print
pp_trap(Topic, Task, Data, Expr-_Res/Flags, li(Trap)) :-
    findall(N-A, member(step(buggy, N, A), Flags), [Name-Args]),
    colors(Expr, Col),
    member(traps(Traps), Data),
    memberchk(Name, Traps),
    Topic:hint(Name, Args, [topic(Topic), task(Task) | Col], Trap).

pp_traps(Topic, Task, Data)
--> { member(wrongall(E_R_F), Data),
      findall(L, 
        ( member(Wrong, E_R_F), 
          pp_trap(Topic, Task, Data, Wrong, L)
        ), Traps),
      sort(Traps, Sorted) % Duplicates due to multiple solutions
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-warning'), "Traps"),
        div(class('card-body'),
          [ p(class('card-text'), "Avoid these traps"),
            p(class('card-text'), ul(Sorted))
          ])
      ])).

% Download task data
download(File) :-
    session_tmpfile(File),
    r_topic(download(File)).

%
% Run example outside of webserver
%
% $ swipl
% ?- [tasks].
% ?- trace.   (if needed)
% ?- tasks:tasks.
%
tasks :-
    tasks(tpaired, tratio).

tasks(Topic, Task) :-
    b_setval(http_session_id, default_session),
    r_init_session,
    r('set.seed'(4711)),
    r_session_source(Topic),
    b_setval(topic, Topic),
    writeln("All solutions"),
    solutions(Topic, Task, AllSolutions),
    writeln(AllSolutions),
    writeln("All hints"),
    findall(Task-Hints, Topic:hints(Task, Hints, _, _), AllHints),
    writeln(AllHints),
    task(Topic, Task, TaskData),
    TaskData = task(Topic, Task, Data),
    writeln("Task data"),
    writeln(Data),
    writeln("Task"),
    html(\(Topic:render([topic(Topic)])), Item, []),
    writeln("Task as HTML"),
    writeln(Item),
    memberchk(solutions(S), Data), 
    writeln("Solutions"),
    writeln(S),
    html(\pp_solutions(Topic, Task, Data), Sol, []),
    writeln(Sol),
    memberchk(wrong(W), Data), 
    length(W, L), 
    format("Wrong alternatives: ~w~n", [L]),
    html(\pp_wrongs(Topic, Task, Data), Wrong, []),
    writeln(Wrong),
    memberchk(traps(T), Data),
    format("Traps: ~w~n", [T]),
    html(\pp_traps(Topic, Task, Data), Traps, []),
    writeln(Traps).
