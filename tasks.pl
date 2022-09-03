:- module(tasks, 
  [ task/2, feedback//3, solutions//1, hints//1, wrongs//1,
    traps//1, download/1, hints/2
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
:- use_module(oddsratio).
:- use_module(oddsratio2).
:- use_module(easyodds).
:- use_module(tgroups).
:- use_module(tgroups2).
:- use_module(tgroupsdf).
:- use_module(ztrans).
:- use_module(ztrans2).
:- use_module(dbinom).
:- use_module(qbinom).
:- use_module(chisq).
:- use_module(power).

% Render R result
mathml:mathml_hook(Flags, r(Expr), Flags, Res) :-
    r_task(Expr, Res),
    number(Res).

% Gather useful information
%
% 1. Identify (correct) solution
% 2. Steps in (1) = Hints
% 3. Identify (incorrect) alternatives
% 4. Subset of (3) with exactly one buggy rule
% 5. Buggy steps in (4) = Traps
%
% more to come
task(Task, Data) :-
    r_initialize,
    r_session_source(Task),
    solutions(Task, Solutions),
    hints(Task, Hints),
    wrong(Task, E_R_F),
    wrongall(Task, E_R_F_All),
    traps(E_R_F_All, Traps),
    Data = task(Task, 
      [ solutions(Solutions), 
        hints(Hints), 
        wrong(E_R_F),
        wrongall(E_R_F_All), % this needs a better solution
        traps(Traps)
      ]).

% Correct response
feedback(Task, Data, Form) -->
    { option(resp(R), Form),
      quantity(N0, Opt, R),
      interval([task(Task)], @(N0, Opt), Num),
      memberchk(solutions(Solutions), Data),
      member(Expr-Res/Flags, Solutions),
      colors(Expr, Col),
      interval([task(Task) | Col], Num =@= Res, _),
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          Task:feedback(Name, Args, [task(Task) | Col], FB)
        ), Items)
    },
    html(div(class("card"),
      [ div(class("card-header text-white bg-success"), "Congratulations"),
        div(class("card-body"),
          [ p(class("card-text"), "Correct response!"),
            ul(Items)
          ])
      ])).

% Buggy response
feedback(Task, Data, Form) -->
    { option(resp(R), Form),
      quantity(N0, Opt, R),
      interval([task(Task)], @(N0, Opt), Num),
      member(traps(Traps), Data),
      member(hints(Hints0), Data),
      append(Hints0, Hints1),
      sort(Hints1, Hints),
      member(wrongall(ERF), Data),
      member(Expr-Res/Flags, ERF),
      colors(Expr, Col),
      interval([task(Task) | Col], Num =@= Res, _),
      % relevant feedback
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          memberchk(Name, Hints),
          Task:feedback(Name, Args, [task(Task) | Col], FB)
        ), Correct0),
      ( Correct0 = []
        -> Correct = p(class("card-text"), "")
         ; Correct = p(class("card-text"), 
                       [ "Correct steps",
                         ul(class("card-text"), ul(Correct0))
                       ])
      ),
      findall(li(FB),
        ( member(step(buggy, Name, Args), Flags),
          memberchk(Name, Traps),
          Task:feedback(Name, Args, [task(Task) | Col], FB)
        ), Wrong0),
      ( Wrong0 = []
        -> Wrong = p(class("card-text"), "")
         ; Wrong = p(class("card-text"),
                       [ "Wrong steps",
                         ul(class("card-text"), ul(Wrong0))
                       ])
      ),
      % irrelevant feedback
      findall(li(FB),
        ( member(step(expert, Name, Args), Flags),
          \+ memberchk(Name, Hints),
          Task:feedback(Name, Args, [task(Task) | Col], FB)
        ), Praise0),
      ( Praise0 = []
        -> Praise = p(class("card-text"), "")
         ; Praise = p(class("card-text"),
                       [ "Other praise",
                         ul(class("card-text"), ul(Praise0))
                       ])
      ),
      findall(li(FB),
        ( member(step(buggy, Name, Args), Flags),
          \+ memberchk(Name, Traps),
          Task:feedback(Name, Args, [task(Task) | Col], FB)
        ), Blame0),
      ( Blame0 = []
        -> Blame = p(class("card-text"), "")
         ; Blame = p(class("card-text"),
                       [ "Other mistakes",
                         ul(class("card-text"), ul(Blame0))
                       ])
      )
    },
    html(div(class("card"),
      [ div(class("card-header text-white bg-warning"), "Careful"),
        div(class("card-body"),
          [ p(class("card-text"), "This is the correct expression:"),
            p(class("card-text"), \mmlm([task(Task), error(fix) | Col], Expr)),
            p(class("card-text"), "Your response matches the following expression:"),
            p(class("card-text"), \mmlm([task(Task), error(highlight) | Col], Expr)),
            Correct, Wrong, Praise, Blame
          ])
      ])).

feedback(_Task, _Data, Form) -->
    { option(resp(R), Form),
      quantity(N, Opt, R)
    },
    html(div(class("card"),
      [ div(class("card-header text-white bg-secondary"), "Feedback"),
        div(class("card-body"),
          p(class("card-text"), "Response: ~p ~p"-[N, Opt]))
      ])).

feedback(_Task, _Data, Form) -->
    { option(resp(R), Form) },
    html(div(class("card"),
      [ div(class("card-header text-white bg-secondary"), "Feedback"),
        div(class("card-body"),
          p(class("card-text"), "Response not recognized: ~p"-[R]))
      ])).

feedback(_Task, _Data, _Form) -->
    html(div(class("card"),
      [ div(class("card-header text-white bg-secondary"), "Feedback"),
        div(class("card-body"),
          p(class("card-text"), "Waiting for response..."))
      ])).

% Solution and correct numerical result
solution(Task, Expr-Res/Flags) :-
    search(Task, Expr, Flags),
    findall(Bug, member(step(buggy, Bug, _), Flags), []),
    interval([task(Task)], Expr, Res).

solutions(Task, List) :-
    findall(ERF, solution(Task, ERF), List0),
    % avoid duplicates by permutations, see search.pl
    findall(sol(Expr, Res/Codes, Flags),
      ( member(Expr-Res/Flags, List0),
        sort(Flags, Sorted),
        codes(Sorted, Codes)
      ),
      List1),
    sort(2, @<, List1, List2),
    findall(Expr-Res/Flags, member(sol(Expr, Res/_, Flags), List2), List).

% Pretty print
solution(Task, Expr-Result/Flags) -->
    { colors(Expr, Col),
      findall(li(FB),
      ( member(step(expert, Name, Args), Flags),
        Task:feedback(Name, Args, [task(Task) | Col], FB)
      ), Items)
    },
    html(div(class("accordion-item"),
      [ h2(class("accordion-header"),
          button([class("accordion-button"), type("button")], 
            \mmlm([task(Task) | Col], Result))),
        div(class("accordion-collapse collapse show"),
          div(class("accordion-body"), 
           [ p(\mmlm([task(Task) | Col], Expr)), 
             ul(Items)
           ]))
      ])).

solutions(task(Task, Data)) -->
    { member(solutions(Expr_Res_Flags), Data) },
    html(div(class("card"),
      [ div(class("card-header text-white bg-success"), "Solution(s)"),
        div(class("card-body"),
          [ p(class("card-text"), "The system accepts the following correct response(s)"),
            div(class("accordion accordion-flush"), 
              \foreach(member(ERF, Expr_Res_Flags), html(\solution(Task, ERF))))
          ])
      ])).

% Codes for correct steps    
hint(_Expr-_Res/Flags, Hint) :-
    findall(H, member(step(expert, H, _), Flags), Hint).

% Cycle through all solutions of a given task
hints(Task, Hints) :-
    solutions(Task, Sol),
    maplist(hint, Sol, Hints).

% Pretty print
hint(Task, _Data, Expr-Result/Flags) -->
    { colors(Expr, Col),
      hint(Expr-Result/Flags, Hints),
      findall(li(H),
        ( member(step(expert, Name, Arg), Flags), 
          memberchk(Name, Hints), 
          Task:hint(Name, Arg, [task(Task) | Col], H)
        ), Items)
    },
    html(div(class("accordion-item"),
      [ h2(class("accordion-header"),
          button([class("accordion-button"), type("button")],
            \mmlm([task(Task) | Col], Result))),
        div(class("accordion-collapse collapse show"),
          div(class("accordion-body"),
           ul(Items)))
      ])).

hints(task(Task, Data)) -->
    { member(solutions(Expr_Res_Flags), Data) },
    html(div(class("card"),
      [ div(class("card-header text-white bg-info"), "Hints"),
        div(class("card-body"),
          [ p(class("card-text"), "Steps to the solution(s)"),
            div(class("accordion accordion-flush"), 
              html(\foreach(member(ERF, Expr_Res_Flags), html(\hint(Task, Data, ERF)))))
          ])
      ])).

% The incorrect response alternatives
wrong(Task, Expr_Res_Flags) :-
    searchdep(Task, ERF),
    findall(E-R/F, 
      ( member(E-R/F, ERF), 
        memberchk(step(buggy, _, _), F)
      ), Expr_Res_Flags).

% Pretty print
wrong(task(Task, Data), Expr-_Res/Flags, Items) :-
    member(traps(Traps), Data),
    colors(Expr, Col),
    findall(li(FB), 
      ( member(step(_, Name, Args), Flags),
        memberchk(Name, Traps), % show only relevant feedback
        Task:feedback(Name, Args, [task(Task) | Col], FB)
      ), Items).

wrongs(task(Task, Data)) -->
    { member(wrong(Expr_Res_Flags), Data),
      findall(
        li([ \mmlm([task(Task), error(highlight) | Col], E = R), ul(FB) ]), 
        ( member(E-R/F, Expr_Res_Flags),
          colors(E, Col),
          wrong(task(Task, Data), E-R/F, FB)
        ), List)
    },
    html(div(class("card"),
      [ div(class("card-header text-white bg-danger"), "Wrong alternatives"),
        div(class("card-body"),
          [ p(class("card-text"), "These wrong responses are recognized by the system"),
            p(class("card-text"), ol(List))
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
wrongall(Task, Expr_Res_Flags) :-
    searchall(Task, E_R_F),
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
trap(task(Task, Data), Expr-_Res/Flags, li(Trap)) :-
    findall(N-A, member(step(buggy, N, A), Flags), [Name-Args]),
    colors(Expr, Col),
    member(traps(Traps), Data),
    memberchk(Name, Traps),
    Task:hint(Name, Args, [task(Task) | Col], Trap).

traps(task(Task, Data)) -->
    { member(wrongall(E_R_F), Data),
      findall(L, 
        ( member(Wrong, E_R_F), 
          trap(task(Task, Data), Wrong, L)
        ), Traps),
      sort(Traps, Sorted) % Duplicates due to multiple solutions
    },
    html(div(class("card"),
      [ div(class("card-header text-white bg-warning"), "Traps"),
        div(class("card-body"),
          [ p(class("card-text"), "Avoid these traps"),
            p(class("card-text"), ul(Sorted))
          ])
      ])).

% Download task data
download(File) :-
    session_tmpfile(File),
    r_task(download(File)).

%
% Run example outside of webserver
% $ swipl
% ?- [tasks].
% ?- trace.   (if needed)
% ?- tasks:test.
%
test :-
    test(tgroups).

test(Task) :-
    r_initialize,
    r('set.seed'(4711)),
    r_session_source(Task),
    b_setval(task, Task),
    writeln("All solutions"),
    solutions(Task, AllSolutions),
    writeln(AllSolutions),
    writeln("All hints"),
    hints(Task, AllHints),
    writeln(AllHints),
    task(Task, TaskData),
    TaskData = task(Task, Data),
    writeln("Task data"),
    writeln(Data),
    writeln("Task"),
    Task:start(I),
    html(\(Task:render(I, [])), Item, []),
    writeln("Task as HTML"),
    writeln(Item),
    memberchk(solutions(S), Data), 
    writeln("Solutions"),
    writeln(S),
    html(\solutions(TaskData), Sol, []),
    writeln(Sol),
    memberchk(hints(H), Data),
    format("Hints: ~w~n", [H]),
    html(\hints(TaskData), Hints, []),
    writeln(Hints),
    memberchk(wrong(W), Data), 
    length(W, L), 
    format("Wrong alternatives: ~w~n", [L]),
    html(\wrongs(TaskData), Wrong, []),
    writeln(Wrong),
    memberchk(traps(T), Data),
    format("Traps: ~w~n", [T]),
    html(\traps(TaskData), Traps, []),
    writeln(Traps),
    % download(Task, File),
    % writeln(download-File),
    true.

