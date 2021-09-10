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
    Z = instead(Bug, Y, Of).

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

