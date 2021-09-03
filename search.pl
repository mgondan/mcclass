:- module(search, [search/3, searchall/2]).

:- use_module(steps).
:- use_module(intermediate).

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
search(Task, Z, Sorted) :-
    start(Task, X),
    search(Task, stage(1), X, Y, Flags1),
    search(Task, stage(2), Y, Z, Flags2),
    append(Flags1, Flags2, Flags),
    complete(Task, Z),
    sort(Flags, Sorted).

% Return all solutions for a given task
%
% The sort/4 in the last line eliminates redundant solutions (redundant = same
% flags). This is a bit dangerous, since wrong steps are not commutative in
% general. It will be replaced by a stricter definition that also checks for
% equality of the numerical results).
searchall(Task, Expr_Flags) :-
    findall(E-F, search(Task, E, F), E_F),
    sort(2, @<, E_F, Expr_Flags).

% Invoke with search:test.
test :-
    use_module(tpaired),
    search(tpaired, Y, Flags),
    writeln(Y-Flags).

