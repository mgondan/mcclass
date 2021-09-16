:- module(tasks, [task/2, solution//1, hints//1, wrongs//1, traps//1]).

:- use_module(library(http/html_write)).
:- use_module(mathml).
:- use_module(search).

:- reexport(tpaired).
:- reexport(oddsratio).

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
    traps(E_R_F, T),
    Data = task(Task, 
      [ sol(Expr-Res/Flags), 
        hints(H), 
        wrong(E_R_F),
        traps(T)
      ]).

% Solution and correct numerical result
%
% At present, this is the first solution in the list, because expert rules are
% tried before buggy rules (see steps.pl). This may change in the future, when
% we allow, e.g., tasks with multiple correct solutions.
solution(Task, Expr-Res/Flags) :-
    once(search(Task, Expr, Res, Flags)).

% Pretty print
solution(task(_Task, Data)) -->
    { member(sol(Expr-Result/_Flags), Data),
      colors(Expr, Col)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-success"), "Solution"),
            div(class("card-body"),
              p(class("card-text"), \mmlm(Col, Expr = Result)))
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
          hint(Task, Name, Arg, Col, H)
        ), List)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-info"), "Hints"),
            div(class("card-body"),
              [ p(class("card-text"), "Steps to the solution"),
                p(class("card-text"), ul(List))])])).

% The incorrect response alternatives
wrong(Task, Expr_Res_Flags) :-
    searchall(Task, [_ | Expr_Res_Flags]).

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
        li([ \mmlm(Col, E = R), ul(FB) ]), 
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
    { member(wrong(E_R_F), Data),
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
    test(tpaired).

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

