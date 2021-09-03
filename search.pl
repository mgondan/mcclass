
:- module(search, [search/3]).

:- use_module(steps).
:- use_module(intermediate).

% Reached the goal
search(_, _, Y, Y, []).

% Continue search
search(Task, Stage, X, Y, Path) :-
    step(Task, Stage, X, Z, Flags),
    search(Task, Stage, Z, Y, Steps),
    append(Flags, Steps, Path).

% Convenience function
search(Task, Z, Flags) :-
    start(Task, X),
    search(Task, stage(1), X, Y, Flags1),
    search(Task, stage(2), Y, Z, Flags2),
    append(Flags1, Flags2, Flags),
    complete(Task, Z).

% Invoke with search:test.
test :-
    use_module(tpaired),
    search(tpaired, Y, Flags),
    writeln(Y-Flags).
