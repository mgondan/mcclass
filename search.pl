
:- module(search, [search/3]).

:- use_module(steps).
:- use_module(intermediate).

% Reached the goal
search(_, _, Y, Y, []).

% Continue search
search(Stage, Task, X, Y, Path) :-
    step(Stage, Task, X, Z, Flags),
    search(Stage, Task, Z, Y, Steps),
    append(Flags, Steps, Path).

% Convenience function
search(Task, Z, Flags) :-
    start(Task, X),
    search(stage(1), Task, X, Y, Flags1),
    search(stage(2), Task, Y, Z, Flags2),
    append(Flags1, Flags2, Flags),
    complete(Task, Z).

% Invoke with search:test.
test :-
    use_module(tpaired),
    search(tpaired, Y, Flags),
    writeln(Y-Flags).
