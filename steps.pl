:- module(steps, [search/4, search/5]).

:- use_module(tasks).
:- use_module(intermediate).
:- use_module(depends).

% Return a solution for a given task
%
% Search is done in three stages to avoid redundancies. Typical buggy rules for
% stage(1) are mix-up of parameters (see examples in tpaired.pl). At stage(2),
% bugs refer to wrong steps in the calculation method. In the end, we check
% if the solution is complete (not intermediate). The flags are sorted to allow
% elimination of redundant solutions that occur within stages (e.g.,
% permutations)
search(Topic, Task, Expr, Path) :-
    search([expert, buggy], Topic, Task, Expr, Path).

search(Type, Topic, Task, Expr, Path) :-
    Topic:start(X),
    search(Type, Topic, Task, stage(1), X, Y, Path1),
    search(Type, Topic, Task, stage(2), Y, Z, Path2),
    search(Type, Topic, Task, stage(3), Z, Expr, Path3),
    % discard intermediate solutions
    complete(Topic, Task, Expr),
    % discard incompatible bugs
    compatible(Expr),
    append([Path1, Path2, Path3], Path).

% Reached the goal
search(_, _, _, _, Y, Y, []).

% Continue search
search(Type, Topic, Task, Stage, X, Y, Path) :-
    step(Type, Topic, Task, Stage, X, Z, Step),
    search(Type, Topic, Task, Stage, Z, Y, Steps),
    append(Step, Steps, Path).

% Unclear why list and member/2 is needed
step(Type, Topic, Task, Stage, X, Y, Step) :-
    member(expert, Type),
    Topic:expert(Task, Stage, X, Y, Step).

step(Type, Topic, Task, Stage, X, Y, Step) :-
    member(buggy, Type),
    Topic:buggy(Task, Stage, X, Y, Step).

% Handle special compounds
step(Type, Topic, Task, Stage, instead(Bug, X, Of), Y, Flags) :-
    !,
    step(Type, Topic, Task, Stage, instead(Bug, X, Of, Of), Y, Flags).

step(Type, Topic, Task, Stage, instead(Bug, X, Of0, Of), Z, Flags) :-
    step(Type, Topic, Task, Stage, X, Y, Flags),
    Z = instead(Bug, Y, Of0, Of).

step(_Type, Topic, Task, Stage, instead(Bug, Wrong, Of0, X), Z, []) :-
    !,
    step([expert], Topic, Task, Stage, X, Y, _Flags),
    Z = instead(Bug, Wrong, Of0, Y).

step(Type, Topic, Task, Stage, omit_left(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Type, Topic, Task, Stage, R, New, Flags),
    Y =.. [Op, L, New],
    Z = omit_left(Bug, Y).

step(Type, Topic, Task, Stage, omit_right(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Type, Topic, Task, Stage, L, New, Flags),
    Y =.. [Op, New, R],
    Z = omit_right(Bug, Y).

step(Type, Topic, Task, Stage, omit(Bug, Expr), Z, Flags) :-
    !,
    step(Type, Topic, Task, Stage, Expr, New, Flags),
    Z = omit(Bug, New).

step(Type, Topic, Task, Stage, drop_left(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Type, Topic, Task, Stage, R, New, Flags),
    Y =.. [Op, L, New],
    Z = drop_left(Bug, Y).

step(Type, Topic, Task, Stage, drop_right(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Type, Topic, Task, Stage, L, New, Flags),
    Y =.. [Op, New, R],
    Z = drop_right(Bug, Y).

step(Type, Topic, Task, Stage, denote(denote, Expr, Text), Z, Flags) :-
    !,
    step(Type, Topic, Task, Stage, Expr, New, Flags),
    Z = denote(denote, New, Text).

% Enter term and apply rule to components. For example, enter 
% dfrac(Numerator, Denominator) and check if a rule can be applied to the
% numerator and/or the denominator.
step(Type, Topic, Task, Stage, X, Y, Flags) :-
    compound(X),
    compound_name_arguments(X, Name, XArgs),
    nth1(Index, XArgs, Arg, Rest),
    step(Type, Topic, Task, Stage, Arg, New, Flags),
    nth1(Index, YArgs, New, Rest),
    compound_name_arguments(Y, Name, YArgs).

