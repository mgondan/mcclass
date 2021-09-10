:- module(search, [searchall/2, solution//1]).

:- use_module(r).
:- use_module(tasks).
:- use_module(steps).
:- use_module(intermediate).
:- use_module(depends).
:- use_module(feedback).

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
      feedback(Task, Flags, Col, FB),
      findall(li(L), member(L, FB), Feedback) 
    },
    html(div(class("card"),
          [ div(class("card-header text-white bg-success"), "Solution"),
            div(class("card-body"), 
              [ p(class("card-text"), \mmlm(Expr = Result)),
                p(class("card-text"), ul(Feedback))
              ])
          ])).

% Invoke with search:test.
test :-
    use_module(tpaired),
    r_init,
    searchall(tpaired, E_R_F),
    writeln(E_R_F).

