:- module(steps, [step/6]).

:- use_module(tasks).

step(Topic, Task, Stage, X, Y, Flags) :-
    step([expert, buggy], Topic, Task, Stage, X, Y, Flags).

step(Type, Topic, Task, Stage, X, Y, Flags) :-
    member(expert, Type),
    Topic:expert(Task, Stage, X, Y, Flags).

step(Type, Topic, Task, Stage, X, Y, Flags) :-
    member(buggy, Type),
    Topic:buggy(Task, Stage, X, Y, Flags).

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

step(Type, Topic, Task, Stage, abbrev(Abbrev, Expr, Text), Z, Flags) :-
    !,
    step(Type, Topic, Task, Stage, Expr, New, Flags),
    Z = abbrev(Abbrev, New, Text).

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

