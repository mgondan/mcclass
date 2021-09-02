:- module(search, [search/3]).

:- use_module(steps).
:- use_module(intermediate).

% Reached the goal
search(_, Y, Y, []).

% Continue search
search(Task, X, Y, Path) :-
    step(Task, X, Z, Flags),
    search(Task, Z, Y, Steps),
    append(Flags, Steps, Path).

% Convenience function
search(Task, Y, Flags) :-
    start(Task, X),
    search(Task, X, Y, Flags),
    complete(Task, Y).

% Invoke with search:test.
test :-
    use_module(tpaired),
    search(tpaired, Y, Flags),
    writeln(Y-Flags).
