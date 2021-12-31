:- module(steps, [step/5]).

:- use_module(tasks).

% Apply expert rule
step(Task, Stage, X, Y, Flags) :-
    expert(Task, Stage, X, Y, Flags).

% Apply buggy rule
step(Task, Stage, X, Y, Flags) :-
    buggy(Task, Stage, X, Y, Flags).

% Handle special compounds
step(Task, Stage, instead(Bug, X, Of), Z, Flags) :-
    !,
    step(Task, Stage, X, Y, Flags),
    Z = instead(Bug, Y, Of, Of).

step(Task, Stage, instead(Bug, X, Of0, Of), Z, Flags) :-
    step(Task, Stage, X, Y, Flags),
    Z = instead(Bug, Y, Of0, Of).

step(Task, Stage, instead(Bug, Wrong, Of0, X), Z, []) :-
    !,
    step(Task, Stage, X, Y, _Flags),
    Z = instead(Bug, Wrong, Of0, Y).

step(Task, Stage, omit_left(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Task, Stage, R, New, Flags),
    Y =.. [Op, L, New],
    Z = omit_left(Bug, Y).

step(Task, Stage, omit_right(Bug, Expr), Z, Flags) :-
    !,
    Expr =.. [Op, L, R],
    step(Task, Stage, L, New, Flags),
    Y =.. [Op, New, R],
    Z = omit_right(Bug, Y).

step(Task, Stage, omit(Bug, Expr), Z, Flags) :-
    !,
    step(Task, Stage, Expr, New, Flags),
    Z = omit(Bug, New).

step(Task, Stage, abbrev(Abbrev, Expr, Text), Z, Flags) :-
    !,
    step(Task, Stage, Expr, New, Flags),
    Z = abbrev(Abbrev, New, Text).

% Enter term and apply rule to components. For example, enter 
% dfrac(Numerator, Denominator) and check if a rule can be applied to the
% numerator and/or the denominator.
step(Task, Stage, X, Y, Flags) :-
    compound(X),
    compound_name_arguments(X, Name, XArgs),
    nth1(Index, XArgs, Arg, Rest),
    step(Task, Stage, Arg, New, Flags),
    nth1(Index, YArgs, New, Rest),
    compound_name_arguments(Y, Name, YArgs).

