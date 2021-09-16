:- module(search, [searchall/2, solution//1, critical//2]).

:- use_module(r).
:- use_module(tasks).
:- use_module(steps).
:- use_module(intermediate).
:- use_module(depends).
:- use_module(feedback).
:- use_module(mathml).

% Reached the goal
search_(_, _, Y, Y, []).

% Continue search
search_(Task, Stage, X, Y, Path) :-
    step(Task, Stage, X, Z, Flags),
    search_(Task, Stage, Z, Y, Steps),
    append(Flags, Steps, Path).

% Return a solution for a given task
%
% Search is done in two stages to avoid redundancies. Typical buggy rules for
% stage(1) are mix-up of parameters (see examples in tpaired.pl). At stage(2),
% bugs refer to wrong steps in the calculation method. In the end, we check 
% if the solution is complete (not intermediate). The flags are sorted to allow
% elimination of redundant solutions that occur within stages (e.g., 
% permutations).
search(Task, Expr, Result, Flags, Sorted) :-
    start(Task, X),
    search_(Task, stage(1), X, Y, Flags1),
    search_(Task, stage(2), Y, Expr, Flags2),
    complete(Task, Expr),           % no intermediate solutions
    compatible(Expr),               % no incompatible bugs
    append(Flags2, Flags1, Flags),  % confusions (stage 1) last in feedback
    sort(Flags, Sorted),
    dependencies(Sorted),           % dependencies between bugs
    Result <- Expr.

% Return all solutions for a given task
%
% The sort/4 in the last line eliminates redundant solutions (redundant = same
% flags and same numerical result).
searchall(Task, Expr_Res_Flags) :-
    findall(res(E, R/S, F), search(Task, E, R, F, S), Results),
    sort(2, @<, Results, Sorted),
    findall(E-R/F, member(res(E, R/_, F), Sorted), Expr_Res_Flags).

% Find correct solution for a given task
%
% At present, this is the first solution in the list, because expert rules are
% tried before buggy rules (see steps.pl). This may change in the future, when
% we allow, e.g., tasks with multiple correct solutions.
solution(Task, Expr, Result, Flags) :-
    once(search(Task, Expr, Result, Flags, _Sorted)).

solution(Task) -->
    { solution(Task, Expr, Result, Flags),
      colors(Expr, Col),
      hints(Task, Flags, Col, Hints),
      findall(li(L), member(L, Hints), List) 
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-success"), "Solution"),
            div(class("card-body"), 
              [ p(class("card-text"), \mmlm(Expr = Result)),
                p(class("card-text"), "Hints"),
                p(class("card-text"), ul(List))
              ])
          ])).

% Return all wrong alternatives with exactly one bug. These bugs are critical,
% that is, if you commit them, you lost the path to the correct solution. Any
% other bugs that occur downstream (e.g., in tpaired, the school bug that 
% may occurs in the wrong formula of the independent t-test) are less relevant
% for feedback, we only use them to understand what the user is doing.
onebug(Task, Expr, Flags, FB) :-
    findall(B-A, member(step(buggy, B, A), Flags), [Bug-Arg]),
    colors(Expr, Col),
    hint(Task, Bug, Arg, Col, FB).

critical(Task, Expr_Res_Flags, Crit) :-
    findall(FB, (member(E-_R/F, Expr_Res_Flags), onebug(Task, E, F, FB)), Crit).

critical(Task, Expr_Res_Flags) -->
    { critical(Task, Expr_Res_Flags, Crit),
      findall(li(L), member(L, Crit), List)
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-danger"), "Traps"),
            div(class("card-body"),
              [ p(class("card-text"), "Avoid these mistakes"),
                p(class("card-text"), ul(List))
              ])
          ])).

%
% Run example outside of webserver
% $ swipl
% ?- [search].
% ?- trace.   (if needed)
% ?- search:test.
%
test :-
    test(tpaired),
    test(tpaired). % Change to or

test(Task) :-
    use_module(tasks),
    init(Task),
    writeln("All alternatives"),
    searchall(Task, E_R_F),
    writeln(E_R_F),
    writeln("Correct response"),
    solution(Task, Expr, Res, Flags),
    writeln(Expr-Res/Flags),
    writeln("Critical bugs"),
    critical(Task, E_R_F, Crit),
    writeln(Crit).

