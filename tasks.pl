:- module(tasks, [task/3, feedback//4, pp_solutions//3, pp_hints//3, 
    pp_wrongs//3, pp_traps//3, download/1, hints/3
  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_log)).
:- use_module(library(dcg/high_order)).
:- use_module(mathml).
:- use_module(search).
:- use_module(r).
:- use_module(session).
:- use_module(interval).
:- use_module(library(quantity)).

:- use_module(tpaired).
:- use_module(baseline).
:- use_module(oddsratio).
:- use_module(oddsratio2).
:- use_module(easyodds).
:- use_module(tgroups).
:- use_module(tgroupsdf).
:- use_module(ztrans).
:- use_module(ztrans2).
:- use_module(dbinom).
:- use_module(qbinom).
:- use_module(powbinom).
:- use_module(chisq).
:- use_module(power).
:- use_module(cigroups).

% Render R result
mathml:mathml_hook(Flags, r(Expr), Flags, Res) :-
    r_topic(Expr, Res).

% read intervals from input
interval:hook(@(Expr, Options), Res, Flags) :-
    !, append(Options, Flags, New),
    option(digits(D), New, 1.0Inf),
    Eps is 10^(-D)/2,
    MEps is -Eps,
    interval(Expr + MEps...Eps, Res, New).

interval:hook(abbrev(_Sym, Expr, _Text), Flags, Expr, Flags).

interval:hook(color(_Col, Expr), Flags, Expr, Flags).

interval:hook(instead(_Bug, Wrong, _Correct), Flags, Wrong, Flags).

interval:hook(instead(_Bug, Wrong, _Correct0, _Correct), Flags, Wrong, Flags).

interval:hook(omit_right(_Bug, Expr), Flags, Left, Flags) :-
    Expr =.. [_Op, Left, _Right].

interval:hook(omit_left(_Bug, Expr), Flags, Right, Flags) :-
    Expr =.. [_Op, _Left, Right].

interval:hook(omit(_Bug, _Expr), Flags, na, Flags).

interval:hook(drop_right(_Bug, Expr), Flags, Left, Flags) :-
    Expr =.. [_Op, Left, _Right].

interval:hook(drop_left(_Bug, Expr), Flags, Right, Flags) :-
    Expr =.. [_Op, _Left, Right].

interval:hook(invent_left(_Bug, Expr), Flags, Expr, Flags).

interval:hook(invent_right(_Bug, Expr), Flags, Expr, Flags).

interval:hook('<-'(Var, Expr), Res, Flags) :-
    interval(Expr, Res, Flags),
    ( Res = L ... _
     -> r_topic('<-'(Var, L)) % incomplete
     ;  r_topic('<-'(Var, Res))
    ).

interval:hook(';'(Expr1, Expr2), Res, Flags) :-
    interval(Expr1, _, Flags),
    interval(Expr2, Res, Flags).

interval:hook('{}'(Expr), Res, Flags) :-
    interval(Expr, Res, Flags).

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
    r_initialize,
    r_session_source(Topic),
    solutions(Topic, Task, Solutions),
    hints(Topic, Task, Hints),
    wrong(Topic, Task, E_R_F),
    wrongall(Topic, Task, E_R_F_All),
    traps(E_R_F_All, Traps),
    Data = task(Topic, Task, 
      [ solutions(Solutions), 
        hints(Hints), 
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
      interval(@(N0, Opt), Num, [topic(Topic), task(Task)]),
      memberchk(solutions(Solutions), Data),
      member(Expr-Res/Flags, Solutions),
      colors(Expr, Col),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Col]),
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Col], FB)
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
      interval(@(N0, Opt), Num, [topic(Topic), task(Task)]),
      memberchk(wrongall(Wrongs), Data),
      member(Expr-Res/Flags, Wrongs),
      colors(Expr, Col),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Col]),
      member(traps(Traps), Data),
      member(hints(Hints0), Data),
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
          Topic:feedback(Name, Args, [topic(Topic), task(Task), abbrev(false) | Col], FB)
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
          Topic:feedback(Name, Args, [topic(Topic), task(Task), abbrev(false) | Col], FB)
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
          Topic:feedback(Name, Args, [topic(Topic), task(Task), abbrev(false) | Col], FB)
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

feedback(Topic, Task, _Data, Form) -->
    { http_log("Form: ~w~n", [Form]),
      % option(resp(R), Form),
      session_data(resp(Topic, Task, R)),
      quantity(N, Opt, R)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Response: ~p ~p"-[N, Opt]))
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

% Solution and correct numerical result
solution(Topic, Task, Expr-Res/Flags) :-
    search(Topic, Task, Expr, Flags),
    findall(Bug, member(step(buggy, Bug, _), Flags), []),
    interval(Expr, Res, [topic(Topic)]).

solutions(Topic, Task, List) :-
    findall(ERF, solution(Topic, Task, ERF), List0),
    % avoid duplicates by permutations, see search.pl
    findall(sol(Expr, Res/Codes, Flags),
      ( member(Expr-Res/Flags, List0),
        sort(Flags, Sorted),
        codes(Sorted, Codes)
      ), List1),
    sort(2, @<, List1, List2),
    findall(Expr-Res/Flags, member(sol(Expr, Res/_, Flags), List2), List).

% Pretty print
pp_solution(Topic, Task, Expr-Result/Flags)
--> { colors(Expr, Col),
      findall(li(FB),
      ( member(step(expert, Name, Args), Flags),
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Col], FB)
      ), Items)
    },
    html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button'), type(button)], 
            \mmlm([topic(Topic), task(Task) | Col], Result))),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'), 
           [ p(\mmlm([topic(Topic), task(Task) | Col], Expr)), 
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

% Codes for correct steps    
hint(_Expr-_Res/Flags, Hint) :-
    findall(H, member(step(expert, H, _), Flags), Hint).

% Cycle through all solutions of a given task
hints(Topic, Task, Hints) :-
    solutions(Topic, Task, Sol),
    maplist(hint, Sol, Hints).

% Pretty print
pp_hint(Topic, Task, _Data, Expr-Result/Flags)
--> { colors(Expr, Col),
      hint(Expr-Result/Flags, Hints),
      findall(li(H),
        ( member(step(expert, Name, Arg), Flags), 
          memberchk(Name, Hints), 
          Topic:hint(Name, Arg, [topic(Topic), task(Task) | Col], H)
        ), Items)
    },
    html(div(class('accordion-item'),
      [ h2(class('accordion-header'),
          button([class('accordion-button'), type(button)],
            \mmlm([topic(Topic), task(Task) | Col], Result))),
        div(class('accordion-collapse collapse show'),
          div(class('accordion-body'),
           ul(Items)))
      ])).

pp_hints(Topic, Task, Data)
--> { member(solutions(Expr_Res_Flags), Data) },
    html(div(class(card),
      [ div(class('card-header text-white bg-info'), "Hints"),
        div(class('card-body'),
          [ p(class('card-text'), "Steps to the solution(s)"),
            div(class('accordion accordion-flush'), 
              html(\foreach(member(ERF, Expr_Res_Flags), 
                html(\pp_hint(Topic, Task, Data, ERF)))))
          ])
      ])).

% The incorrect response alternatives
wrong(Topic, Task, Expr_Res_Flags) :-
    searchdep(Topic, Task, ERF),
    findall(E-R/F, 
      ( member(E-R/F, ERF), 
        memberchk(step(buggy, _, _), F)
      ), Expr_Res_Flags).

% Pretty print
pp_wrong(Topic, Task, Data, Expr-_Res/Flags, Items) :-
    member(traps(Traps), Data),
    colors(Expr, Col),
    findall(li(FB), 
      ( member(step(_, Name, Args), Flags),
        memberchk(Name, Traps), % show only relevant feedback
        Topic:feedback(Name, Args, [topic(Topic), task(Task) | Col], FB)
      ), Items).

pp_wrongs(Topic, Task, Data)
--> { member(wrong(Expr_Res_Flags), Data),
      findall(
        li([ \mmlm([topic(Topic), task(Task), error(highlight) | Col], E = R), ul(FB) ]), 
        ( member(E-R/F, Expr_Res_Flags),
          colors(E, Col),
          pp_wrong(Topic, Task, Data, E-R/F, FB)
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
    tasks(cigroups, cigroups).

tasks(Topic, Task) :-
    r_initialize,
    r('set.seed'(4711)),
    r_session_source(Topic),
    b_setval(topic, Topic),
    writeln("All solutions"),
    solutions(Topic, Task, AllSolutions),
    writeln(AllSolutions),
    writeln("All hints"),
    hints(Topic, Task, AllHints),
    writeln(AllHints),
    task(Topic, Task, TaskData),
    TaskData = task(Topic, Task, Data),
    writeln("Task data"),
    writeln(Data),
    writeln("Task"),
    html(\(Topic:render), Item, []),
    writeln("Task as HTML"),
    writeln(Item),
    memberchk(solutions(S), Data), 
    writeln("Solutions"),
    writeln(S),
    html(\pp_solutions(Topic, Task, Data), Sol, []),
    writeln(Sol),
    memberchk(hints(H), Data),
    format("Hints: ~w~n", [H]),
    html(\pp_hints(Topic, Task, Data), Hints, []),
    writeln(Hints),
    memberchk(wrong(W), Data), 
    length(W, L), 
    format("Wrong alternatives: ~w~n", [L]),
    html(\pp_wrongs(Topic, Task, Data), Wrong, []),
    writeln(Wrong),
    memberchk(traps(T), Data),
    format("Traps: ~w~n", [T]),
    html(\pp_traps(Topic, Task, Data), Traps, []),
    writeln(Traps),
    true.

