:- module(relevant, 
    [
        step/5, 
        solution/3, 
        route/3, 
	hints/3,
        traps/2, 
        feedback/3,
        praise/3,
        mistakes/3,
	relevant/4,
        mathex/3
    ]).

:- use_module(intermediate).

%
% Apply rule to term
%
% Some special compounds
step(Rule, Code, instead_of(Err, A, Wrong, Instead) >> To, Flags, Feedback) :-
    !,
    step(Rule, Code, A >> B, Flags, Feedback),
    To = instead_of(Err, B, Wrong, Instead).

step(Rule, Code, denoting(Symbol, A, Label) >> To, Flags, Feedback) :-
    !, 
    step(Rule, Code, A >> B, Flags, Feedback),
    To = denoting(Symbol, B, Label).

% Direct match
step(expert, Code, Step, Flags, feedback(Feedback)) :-
    expert(Code, Step, Flags, Feedback, _).

step(buggy, Code, Step, Flags, feedback(Feedback)) :-
    buggy(Code, Step, Flags, Feedback).

step(expert, Code, Step, Flags, hint(Hint)) :-
    expert(Code, Step, Flags, _, Hint).

% General compounds
step(Rule, Code, From >> To, Flags, Feedback) :-
    compound(From),
    compound_name_arguments(From, Name, From_args),
    nth1(Index, From_args, Arg, Rest),
    step(Rule, Code, Arg >> New, Flags, Feedback),
    nth1(Index, To_args, New, Rest),
    compound_name_arguments(To, Name, To_args).

%
% Find correct result
%
solution(A, B, Path) :-
    route([expert], A, B, [A], Path).

%
% Find any result
%
route(A, B, Path) :-
    route([expert, buggy], A, B, [A], Path).

% reached goal
route(_, A, A, Steps, Path) :-
    final(A),
    reverse(Steps, Path).

% Continue search
route(Rules, A, B, Steps, Path) :-
    member(R, Rules),
    step(R, _, A >> X, _, feedback(_)),
    route(Rules, X, B, [X | Steps], Path).

%
% Find "relevant" buggy rules (also known as "traps")
%
% These are the bugs along the path to the correct result. Bugs can also
% occur at other places, for example, mistakes specific to the
% two-sample t-test. But these bugs are not relevant for feedback, they
% are only needed to diagnose a mistake.
%
% Traps at a specific location
trap(A, Trap) :-
    findall(Code-Feed, step(buggy, Code, A >> _, [color-auto], feedback(Feed)), Trap).

% Traps along a path
traps(Path, Traps) :-
    maplist(trap, Path, Trap),
    append(Trap, Traps).

%
% Hints to correct result
%
hints(_, [_], []).

hints(Flags, [A, X | Path], [Code-Hint | Hints]) :-
    step(_, Code, A >> X, Flags, hint(Hint)),
    hints(Flags, [X | Path], Hints).

%
% Feedback: all
%
feedback(_, [_], []).

feedback(Flags, [A, X | Path], [Code-Feed | Feedback]) :-
    step(_, Code, A >> X, Flags, feedback(Feed)),
    feedback(Flags, [X | Path], Feedback).

%
% Feedback: only praise
%
praise(_, [_], []).

praise(Flags, [A, X | Path], [Code-F | Feedback]) :-
    step(expert, Code, A >> X, Flags, feedback(F)),
    praise(Flags, [X | Path], Feedback).

praise(Flags, [A, X | Path], Feedback) :-
    step(buggy, _, A >> X, Flags, feedback(_)),
    praise(Flags, [X | Path], Feedback).

%
% Feedback: only mistakes
%
mistakes(_, [_], []).

mistakes(Flags, [A, X | Path], Mistakes) :-
    step(expert, _, A >> X, Flags, feedback(_)),
    mistakes(Flags, [X | Path], Mistakes).

mistakes(Flags, [A, X | Path], [Code-Mistake | Mistakes]) :-
    step(buggy, Code, A >> X, Flags, feedback(Mistake)),
    mistakes(Flags, [X | Path], Mistakes).

%
% Only relevant feedback
%
relevant(Mistakes, Traps, Relevant, Irrelevant) :-
    pairs_keys(Traps, Trap_keys),
    pairs_keys(Mistakes, Mistake_keys),
    intersection(Mistake_keys, Trap_keys, Relevant_keys),
    subtract(Mistake_keys, Trap_keys, Irrelevant_keys),
    findall(Key-L, (member(Key, Relevant_keys), member(Key-L, Mistakes)), Relevant),
    findall(Key-L, (member(Key, Irrelevant_keys), member(Key-L, Mistakes)), Irrelevant).

%
% Math expressions
%
mathex(fix, instead_of(_, _, _, Correct), Out) :-
    !,
    solution(Correct, Solution, _),
    mathex(fix, Solution, Out).

mathex(show, instead_of(_, Wrong, _, _), Out) :-
    !,
    mathex(show, Wrong, Out).

mathex(fix, left_landed(_, Expr), Out) :-
    !,
    Expr =.. [_, _, R],
    mathex(fix, R, Out).

mathex(show, left_landed(_, Expr), Out) :-
    !,
    mathex(show, Expr, Out).

mathex(fix, right_landed(_, Expr), Out) :-
    !,
    Expr =.. [_, L, _],
    mathex(fix, L, Out).

mathex(show, right_landed(_, Expr), Out) :-
    !,
    mathex(show, Expr, Out).

mathex(fix, omit_left(_, Expr), Out) :-
    !,
    mathex(fix, Expr, Out).

mathex(show, omit_left(_, Expr), Out) :-
    !,
    Expr =.. [_, _, R],
    mathex(show, R, Out).

mathex(fix, omit_right(_, Expr), Out) :-
    !,
    mathex(fix, Expr, Out).

mathex(show, omit_right(_, Expr), Out) :-
    !,
    Expr =.. [_, L, _],
    mathex(show, L, Out).

mathex(_, Out, Out) :-
    atomic(Out).

mathex(Mode, In, Out) :-
    compound(In),
    compound_name_arguments(In, Name, Args_in),
    maplist(mathex(Mode), Args_in, Args_out),
    compound_name_arguments(Out, Name, Args_out).

