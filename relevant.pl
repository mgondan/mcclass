:- module(relevant, 
    [
        step/5, 
        solution/3, 
	wrong/3,
	wrong/4,
	wrongs_paths_results/2,
        route/3, 
	hints/3,
        traps/3, 
	codes/2,
        feedback/3,
        praise/4,
        mistakes/4,
	relevant/4,
        mathex/3
    ]).

:- use_module(intermediate).
:- use_module(r).

%
% Apply rule to term
%
% Some special compounds
step(Rule, Code, instead_of(Err, Instead, Of) >> To, Flags, Feedback) :-
    !,
    step(Rule, Code, instead_of(Err, Instead, Instead, Of, Of) >> To, Flags, Feedback).

step(Rule, Code, instead_of(Err, A, Wrong, Of, Correct) >> To, Flags, Feedback) :-
    !,
    step(Rule, Code, A >> B, Flags, Feedback),
    solution(Of, Solution, _),
    To = instead_of(Err, B, Wrong, Solution, Correct).

step(Rule, Code, denoting(Symbol, A, Label) >> To, Flags, Feedback) :-
    !, 
    step(Rule, Code, A >> B, Flags, Feedback),
    To = denoting(Symbol, B, Label).

step(Rule, Code, left_elsewhere(Err, Expr) >> To, Flags, Feedback) :-
    !,
    Expr =.. [Op, Left, A],
    step(Rule, Code, A >> B, Flags, Feedback),
    New =.. [Op, Left, B],
    To = left_elsewhere(Err, New).

step(Rule, Code, right_elsewhere(Err, Expr) >> To, Flags, Feedback) :-
    !,
    Expr =.. [Op, A, Right],
    step(Rule, Code, A >> B, Flags, Feedback),
    New =.. [Op, B, Right],
    To = right_elsewhere(Err, New).

step(Rule, Code, left_landed(Err, Expr) >> To, Flags, Feedback) :-
    !,
    Expr =.. [Op, Left, A],
    step(Rule, Code, A >> B, Flags, Feedback),
    New =.. [Op, Left, B],
    To = left_landed(Err, New).

step(Rule, Code, right_landed(Err, Expr) >> To, Flags, Feedback) :-
    !,
    Expr =.. [Op, A, Right],
    step(Rule, Code, A >> B, Flags, Feedback),
    New =.. [Op, B, Right],
    To = right_landed(Err, New).

% Direct match
step(expert, Code, Step, Flags, feedback(Feedback)) :-
    expert(Code, Step, Flags, Feedback, _).

step(buggy, Code, Step, Flags, feedback(Feedback)) :-
    buggy(Code, Step, Flags, Feedback, _).

step(expert, Code, Step, Flags, hint(Hint)) :-
    expert(Code, Step, Flags, _, Hint).

step(buggy, Code, Step, Flags, hint(Hint)) :-
    buggy(Code, Step, Flags, _, Hint).

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

% Wrong result - assumes one single correct result
wrong(Item, Wrong, Path) :-
    solution(Item, Solution, _),
    wrong(Item, Solution, Wrong, Path).

wrong(Item, Solution, Wrong, Path) :-
    route(Item, Wrong, Path),
    dif(Wrong, Solution).

wrongs_paths_results(Item, Wrongs_Paths_Results) :-
    solution(Item, Solution, _),
    findall((W-P)-(S-R), (wrong(Item, Solution, W, P), codes(P, C), sort(C, S), sur(R <- W)), List),
    % Avoid duplicate results in which only the steps are permuted
    sort(2, @<, List, Sorted),
    findall(L-wrong(Item, W, P, R), (member((W-P)-(_-R), Sorted), code_mistakes([], P, M), length(M, L)), LWPR),
    % Few errors first
    keysort(LWPR, Sorted_LWPR),
    pairs_values(Sorted_LWPR, Wrongs_Paths_Results).
    
% 
% Extract abbreviations of steps
%
codes([_], []).

codes([A, X | Path], [Code | Codes]) :-
    step(_, Code, A >> X, _, feedback(_)),
    codes([X | Path], Codes).

%
% Find "relevant" buggy rules (also known as "traps")
%
% These are the bugs along the path to the correct result. Bugs can also
% occur at other places, for example, mistakes specific to the
% two-sample t-test. But these bugs are not relevant for feedback, they
% are only needed to diagnose a mistake.
%
% Traps at a specific location
trap(A, Sorted) :-
    findall(Code-Feed, step(buggy, Code, A >> _, [color-auto], hint(Feed)), Traps),
    sort(Traps, Sorted).

% Traps along a path
traps(Path, Traps, Code_Traps) :-
    maplist(trap, Path, Trap),
    append(Trap, Code_Traps),
    pairs_values(Code_Traps, Traps).

%
% Hints to correct result
%
hints(Path, Hints, Code_Hints) :-
    hints([], Path, Hints, Code_Hints).

hints(_, [_], [], []).

hints(Flags, [A, X | Path], [Hint | Hints], [Code-Hint | Code_Hints]) :-
    step(_, Code, A >> X, Flags, hint(Hint)),
    hints(Flags, [X | Path], Hints, Code_Hints).

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
praise(Flags, Path, Praise, Code_Praise) :-
    code_praise(Flags, Path, Praise, Code_Praise).

code_praise(_, [_], [], []).

code_praise(Flags, [A, X | Path], [Feed | Praise], [Code-Feed | Code_Praise]) :-
    step(expert, Code, A >> X, Flags, feedback(Feed)),
    code_praise(Flags, [X | Path], Praise, Code_Praise).

code_praise(Flags, [A, X | Path], Praise, Code_Praise) :-
    step(buggy, _, A >> X, Flags, feedback(_)),
    code_praise(Flags, [X | Path], Praise, Code_Praise).

%
% Feedback: only mistakes
%
mistakes(Flags, Path, Mistakes, Code_Mistakes) :-
    code_mistakes(Flags, Path, Code_Mistakes),
    pairs_values(Code_Mistakes, Mistakes).

code_mistakes(_, [_], []).

code_mistakes(Flags, [A, X | Path], Code_Mistakes) :-
    step(expert, _, A >> X, Flags, feedback(_)),
    code_mistakes(Flags, [X | Path], Code_Mistakes).

code_mistakes(Flags, [A, X | Path], [Code-Mistake | Code_Mistakes]) :-
    step(buggy, Code, A >> X, Flags, feedback(Mistake)),
    code_mistakes(Flags, [X | Path], Code_Mistakes).

%
% Only relevant feedback
%
relevant(Mistakes, Traps, Relevant, Irrelevant) :-
    pairs_keys(Traps, Trap_keys),
    pairs_keys(Mistakes, Mistake_keys),
    intersection(Mistake_keys, Trap_keys, Relevant_keys),
    subtract(Mistake_keys, Trap_keys, Irrelevant_keys),
    findall(L, (member(Key, Relevant_keys), member(Key-L, Mistakes)), Relevant),
    findall(L, (member(Key, Irrelevant_keys), member(Key-L, Mistakes)), Irrelevant).

%
% Math expressions
%
mathex(fix, instead_of(_, _, _, Correct, _), Out) :-
    !,
    mathex(fix, Correct, Out).

mathex(show, instead_of(_, Wrong, _, _, _), Out) :-
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

