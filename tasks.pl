:- module(tasks, [task/2, feedback//2, solution//1, hints//1, wrongs//1, traps//1,
                  start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_log)).
:- use_module(mathml).
:- use_module(search).
:- use_module(r).
:- use_module(interval).
:- use_module(library(quantity)).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

:- consult(tpaired).
:- consult(oddsratio).
:- consult(oddsratio2).
:- consult(easyodds).
:- consult(tgroups).
:- consult(ztrans).
:- consult(ztrans2).
:- consult(dbinom).
:- consult(qbinom).
:- consult(chisq).

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
    r_session_eval(Expr, Res),
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
    init(Task),
    solution(Task, Expr-Res/Flags),
    hints(Flags, H),
    wrong(Task, E_R_F),
    wrongall(Task, E_R_F_All),
    traps(E_R_F_All, T),
    Data = task(Task, 
      [ sol(Expr-Res/Flags), 
        hints(H), 
        wrong(E_R_F),
        wrongall(E_R_F_All), % this needs a better solution
        traps(T)
      ]).

% Correct response
feedback(Task, Form) -->
    { option(resp(R), Form),
      quantity(N, _Opt, R),
      solution(Task, Expr-Res/Flags),
      int(pl, N =@= Res, _),
      colors(Expr, Col),
      findall(li(FB),
        ( member(step(_, Name, Args), Flags),
          feedback(Task, Name, Args, Col, FB)
        ), Items)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-success"),
              "Congratulation"),
            div(class("card-body"),
              [ p(class("card-text"), "Correct response!"),
                ul(class("card-text"), ul(Items))
              ])
          ])).

% Buggy response
feedback(Task, Form) -->
    { option(resp(R), Form),
      quantity(N, _Opt, R),
      wrongall(Task, ERF),
      member(Expr-Res/Flags, ERF),
      int(pl, N =@= Res, _),
      colors(Expr, Col),
      findall(li(FB),
        ( member(step(_, Name, Args), Flags),
          feedback(Task, Name, Args, Col, FB)
        ), Items)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-warning"),
              "Careful"),
            div(class("card-body"),
              [ p(class("card-text"), "This is the correct expression:"),
                p(class("card-text"), \mmlm([error(fix) | Col], Expr)),
                p(class("card-text"), "Your response matches the following expression:"),
                p(class("card-text"), \mmlm([error(highlight) | Col], Expr)),
                p(class("card-text"), "Please check the following hints:"),
                ul(class("card-text"), ul(Items))
              ])
          ])).

feedback(_Task, Form) -->
    { option(resp(R), Form),
      quantity(N, Opt, R)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-secondary"),
              "Feedback"),
            div(class("card-body"),
              p(class("card-text"), "Response: ~p ~p"-[N, Opt]))
          ])).

feedback(_Task, Form) -->
    { option(resp(R), Form) },
    html(div(class("card"),
          [ div(class("card-header text-white bg-secondary"),
              "Feedback"),
            div(class("card-body"),
              p(class("card-text"), "Response not recognized: ~p"-[R]))
          ])).

feedback(_Task, _Form) -->
    html(div(class("card"),
          [ div(class("card-header text-white bg-secondary"),
              "Feedback"),
            div(class("card-body"),
              p(class("card-text"), "Waiting for response..."))
          ])).

% Solution and correct numerical result
%
% Most efficient if the correct solution appears early in the list. This may change in the
% future, when we allow, e.g., tasks with multiple correct solutions.
solution(Task, Expr-Res/Flags) :-
    search(Task, Expr_, Flags_),
    findall(Bug, member(step(buggy, Bug, _), Flags_), []),
    !,
    Flags = Flags_,
    Expr = Expr_,
    int(pl, Expr, Res). % todo: session

% Pretty print
solution(task(_Task, Data)) -->
    { member(sol(Expr-Result/_Flags), Data),
      colors(Expr, Col)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-success"), "Solution"),
            div(class("card-body"),
              p(class("card-text"), \mmlm([error(correct) | Col], Expr = Result)))
          ])).

% Codes for correct steps    
hints(Flags, Hints) :-
    findall(H, member(step(expert, H, _), Flags), Hints).

% Pretty print
hints(task(Task, Data)) -->
    { member(sol(Expr-_Result/Flags), Data),
      colors(Expr, Col),
      member(hints(Hints), Data),
      findall(li(H), 
        ( member(step(expert, Name, Arg), Flags), 
          member(Name, Hints), 
          (   hint(Task, Name, Arg, Col, H)
           -> true
           ;  throw(error(hint_argument(Name, Arg)))
          )
        ), List)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-info"), "Hints"),
            div(class("card-body"),
              [ p(class("card-text"), "Steps to the solution"),
                p(class("card-text"), ul(List))])])).

% The incorrect response alternatives
wrong(Task, Expr_Res_Flags) :-
    searchdep(Task, E_R_F),
    findall(E-R/F, 
      ( member(E-R/F, E_R_F), 
        memberchk(step(buggy, _, _), F)
      ), Expr_Res_Flags).

% Pretty print
wrong(task(Task, Data), Expr-_Res/Flags, Items) :-
    member(traps(Traps), Data),
    colors(Expr, Col),
    findall(li(FB), 
      ( member(step(_, Name, Args), Flags),
        member(Name, Traps), % show only relevant feedback
        feedback(Task, Name, Args, Col, FB)
      ), Items).

wrongs(task(Task, Data)) -->
    { member(wrong(Expr_Res_Flags), Data),
      findall(
        li([ \mmlm([error(highlight) | Col], E = R), ul(FB) ]), 
        ( member(E-R/F, Expr_Res_Flags),
          colors(E, Col),
          wrong(task(Task, Data), E-R/F, FB)
        ), List)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-danger"), "Wrong alternatives"),
            div(class("card-body"),
              [ p(class("card-text"), "These wrong responses are recognized by the system"),
                p(class("card-text"), ol(List))])])).

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
traps(E_R_F, Traps) :-
    findall(T, (member(_E-_R/Flags, E_R_F), trap(Flags, T)), Traps).

% Pretty print
trap(task(Task, Data), Expr-_Res/Flags, li(Trap)) :-
    findall(N-A, member(step(buggy, N, A), Flags), [Name-Args]),
    colors(Expr, Col),
    member(traps(Traps), Data),
    member(Name, Traps),
    hint(Task, Name, Args, Col, Trap).

traps(task(Task, Data)) -->
    { member(wrongall(E_R_F), Data),
      findall(L, 
        ( member(Wrong, E_R_F), 
          trap(task(Task, Data), Wrong, L)
        ), Traps)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-warning"), "Traps"),
            div(class("card-body"),
              [ p(class("card-text"), "Avoid these traps"),
                p(class("card-text"), ul(Traps))])])).

%
% Run example outside of webserver
% $ swipl
% ?- [tasks].
% ?- trace.   (if needed)
% ?- tasks:test.
%
test :-
    test(ztrans).

test(Task) :-
    task(Task, TaskData),
    TaskData = task(Task, Data),
    writeln("Task data"),
    writeln(Data),
    memberchk(sol(S), Data), 
    format("Solution: ~w~n", [S]),
    html(\solution(TaskData), Sol, []),
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
    writeln(Traps).

