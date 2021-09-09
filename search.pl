:- module(search, [searchall/2]).

:- use_module(r).
:- use_module(tasks).
:- use_module(steps).
:- use_module(intermediate).
:- use_module(depends).
:- use_module(feedback).

% Reached the goal
search(_, _, Y, Y, []).

% Continue search
search(Task, Stage, X, Y, Path) :-
    step(Task, Stage, X, Z, Flags),
    search(Task, Stage, Z, Y, Steps),
    append(Flags, Steps, Path).

% Return a solution for a given task
%
% Search is done in two stages to avoid redundancies. Typical buggy rules for
% stage(1) are mix-up of parameters (see examples in tpaired.pl). At stage(2),
% bugs refer to wrong steps in the calculation method. In the end, we check 
% if the solution is complete (not intermediate). The flags are sorted to allow
% elimination of redundant solutions that occur within stages.
search(Task, Expr, Result, Flags) :-
    start(Task, X),
    search(Task, stage(1), X, Y, Flags1),
    search(Task, stage(2), Y, Expr, Flags2),
    complete(Task, Expr),    % no intermediate solutions
    compatible(Expr),        % no incompatible bugs
    append(Flags1, Flags2, Unsorted),
    sort(Unsorted, Flags),
    dependencies(Flags),     % dependencies between bugs
    Result <- Expr.

% Return all solutions for a given task
%
% The sort/4 in the last line eliminates redundant solutions (redundant = same
% flags). This is a bit dangerous, since wrong steps are not commutative in
% general. It will be replaced by a stricter definition that also checks for
% equality of the numerical results).
searchall(Task, Expr_Res_Flags) :-
    % Kleines Rätsel :-)
    % Warum schreibe ich hier E-R/F? Hint: hat was mit "Punkt vor Strich" zu tun, es wird aber nichts gerechnet.
    findall(E-R/F, search(Task, E, R, F), E_R_F),
    sort(2, @<, E_R_F, Expr_Res_Flags).

% Invoke with search:test.
test :-
    use_module(tpaired),
    r_init,
    search(tpaired, Expr, Result, Flags),
    writeln(Expr-Result/Flags),
    colors(Expr, Col),
    feedback(tpaired, Flags, Col, Feedback),
    writeln(Feedback).

