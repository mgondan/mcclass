:- module(relevant, 
    [
        solution/4, 
        hints/5,
        traps/5,
        praise/5,
        correct/1,
        wrong/4,
    	wrong/5,
        feedback/6,
        mistakes/6,
        relevant/4,
        mathex/3,
        wrongs_paths_results/3,
        buggies/2
    ]).

:- use_module(intermediate).
:- use_module(r).
:- use_module(library(r/r_call)).

%
% Apply rule to term
%
% Some special compounds
step(Rule, Code, protect(A) >> To, Request) :-
    !,
    Rule = expert,
    step(Rule, Code, A >> B, Request),
    To = protect(B).

step(Rule, Code, instead_of(Err, Instead, Of) >> To, Request) :-
    !,
    step(Rule, Code, instead_of(Err, Instead, Instead, Of, Of) >> To, Request).

step(Rule, Topic: Code, instead_of(Err, A, Wrong, Of, Correct) >> To, Request) :-
    !,
    step(Rule, Topic: Code, A >> B, Request),
    solution(Topic, Of, Solution, _),
    To = instead_of(Err, B, Wrong, Solution, Correct).

step(Rule, Code, denoting(Symbol, A, Label) >> To, Request) :-
    !, 
    step(Rule, Code, A >> B, Request),
    To = denoting(Symbol, B, Label).

step(Rule, Code, abbrev(_, A) >> To, Request) :-
    !,
    step(Rule, Code, A >> B, Request),
    To = B.

step(Rule, Code, left_elsewhere(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, Left, A],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, Left, B],
    To = left_elsewhere(Err, New).

step(Rule, Code, right_elsewhere(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, A, Right],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, B, Right],
    To = right_elsewhere(Err, New).

step(Rule, Code, left_landed(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, Left, A],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, Left, B],
    To = left_landed(Err, New).

step(Rule, Code, right_landed(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, A, Right],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, B, Right],
    To = right_landed(Err, New).

step(Rule, Code, omit_left(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, Left, A],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, Left, B],
    To = omit_left(Err, New).

step(Rule, Code, omit_right(Err, Expr) >> To, Request) :-
    !,
    Expr =.. [Op, A, Right],
    step(Rule, Code, A >> B, Request),
    New =.. [Op, B, Right],
    To = omit_right(Err, New).

step(Rule, Code, skip(Err, Fn, A) >> To, Request) :-
    !,
    step(Rule, Code, A >> B, Request),
    To = skip(Err, Fn, B).

% Direct match
step(expert, Code, A >> B, none) :-
    expert(Code, A >> B, _, _, _),
    consistent(B).

step(expert, Code, A >> B, praise(Praise)) :-
    expert(Code, A >> B, [color-auto], Praise, _).

step(expert, Code, A >> B, feedback(Flags, Feed)) :-
    expert(Code, A >> B, Flags, Feed, _).

step(expert, Code, A >> B, hint(Hint)) :-
    expert(Code, A >> B, [color-auto], _, Hint).

step(buggy, Code, A >> B, none) :-
    buggy(Code, A >> B, _, _, _),
    consistent(B).

step(buggy, Code, A >> B, trap(Trap)) :-
    buggy(Code, A >> B, [color-auto], _, Trap).

step(buggy, Code, A >> B, feedback(Flags, Feed)) :-
    buggy(Code, A >> B, Flags, Feed, _).

% General compounds
step(Rule, Code, From >> To, Request) :-
    compound(From),
    compound_name_arguments(From, Name, From_args),
    nth1(Index, From_args, Arg, Rest),
    step(Rule, Code, Arg >> New, Request),
    nth1(Index, To_args, New, Rest),
    compound_name_arguments(To, Name, To_args).

% Step without feedback
%:- multifile expert/2.
%:- multifile expert/5.
%expert(Code, A >> B, _, "123", "456") :-
%    expert(Code, A >> B).

%
% Check if Expr is consistent 
% 
% Example: In omit(Expr), Expr should not include additional errors since it is
% omitted anyway.
%
consistent(Expr) :-
    atomic(Expr).

consistent(protect(Expr)) :-
    !, 
    correct(Expr).

consistent(instead_of(_, Wrong, Instead)) :-
    !,
    % correct(Instead),
    consistent(Instead),
    consistent(Wrong).

consistent(instead_of(_, Wrong, _, Instead, _)) :-
    !,
    % correct(Instead),
    consistent(Instead),
    consistent(Wrong).

consistent(denoting(_, Expr, _)) :-
    !,
    consistent(Expr).

consistent(abbrev(_, _)) :-
    !.

consistent(left_elsewhere(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    consistent(L),
    consistent(R).

consistent(right_elsewhere(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    consistent(R),
    consistent(L).

consistent(left_landed(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    consistent(L),
    consistent(R).

consistent(right_landed(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    consistent(L),
    consistent(R).

consistent(omit_left(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    correct(L),
    consistent(R).

consistent(omit_right(_, Expr)) :-
    !,
    Expr =.. [_, L, R],
    correct(R),
    consistent(L).

consistent(skip(_, _Fn, Expr)) :-
    !,
    consistent(Expr).

consistent(add(_, Expr)) :-
    !,
    consistent(Expr).

consistent(omit(_, Expr)) :-
    !,
    correct(Expr).

consistent(Compound) :-
    compound(Compound),
    compound_name_arguments(Compound, _, Arguments),
    maplist(consistent, Arguments).

% Check if Expr is correct (i.e., does not include errors)
correct(Expr) :-
    \+ wrong(Expr).

wrong(instead_of(_, _, _)).

wrong(instead_of(_, _, _, _, _)).

wrong(denoting(_, Expr, _)) :-
    wrong(Expr).

wrong(abbrev(_, _)) :-
    fail.

wrong(left_elsewhere(_, _)).

wrong(right_elsewhere(_, _)).

wrong(left_landed(_, _)).

wrong(right_landed(_, _)).

wrong(omit_left(_, _)).

wrong(omit_right(_, _)).

wrong(add(_, _)).

wrong(omit(_, _)).

wrong(skip(_, _, _)).

wrong(Compound) :-
    compound(Compound),
    compound_name_arguments(Compound, _, Arguments),
    \+ maplist(correct, Arguments).

%
% Find correct result(s)
%
solution(Topic, Item, Solution, Path) :-
    solution(Topic, Item, Solution, [], Path).

% Reached goal
solution(Topic, Solution, Solution, Steps, Path) :-
    final(Topic, Solution),
    reverse(Steps, Path).

% Continue search
solution(Topic, A, B, Steps, Path) :-
    step(expert, Topic: Code, A >> X, none),
    solution(Topic, X, B, [Code-X | Steps], Path).

%
% Stay on path with these hints
%
hints(Topic, Item, Path, Code_Hints, Hints) :-
    hints(Topic, Item, Path, Code_Hints),
    pairs_values(Code_Hints, Hints).

hints(_, _, [], []).

hints(Topic, A, [Code-B | Path], [Code-H | Hints]) :-
    step(expert, Topic: Code, A >> B, hint(H)),
    hints(Topic, B, Path, Hints).

%
% Find "relevant" buggy rules (also known as "traps")
%
% These are the bugs along the path to the correct result. Bugs can also occur
% occur at other places, for example, mistakes specific to the two-sample
% t-test in a problem with paired samples. But these latter bugs are not 
% relevant for feedback, they are only needed to diagnose a mistake.
%
traps(Topic, Item, Path, Code_Traps, Traps) :-
    traps(Topic, Item, Path, List),
    append(List, All),
    sort(1, @<, All, Code_Traps),
    pairs_values(Code_Traps, Traps).
    
traps(Topic, Solution, [], [Traps]) :-
    traps(Topic, Solution, Traps).

traps(Topic, A, [Code-B | Path], [T | Traps]) :-
    traps(Topic, A, T),
    step(expert, Topic: Code, A >> B, none),
    traps(Topic, B, Path, Traps).

% Traps at a specific location
traps(Topic, A, Traps) :-
    findall(C-T, step(buggy, Topic: C, A >> _, trap(T)), Unsorted),
    sort(Unsorted, Traps).

%
% Feedback: only praise
%
praise(Topic, Item, Path, Code_Praise, Praise) :-
    praise(Topic, Item, Path, Code_Praise),
    pairs_values(Code_Praise, Praise).

praise(_, _, [], []).

praise(Topic, A, [Code-X | Path], Praise) :-
    step(expert, Topic: Code, A >> X, praise([])),
    !, 
    praise(Topic, X, Path, Praise).

praise(Topic, A, [Code-X | Path], [Code-P | Praise]) :-
    step(expert, Topic: Code, A >> X, praise(P)),
    praise(Topic, X, Path, Praise).

praise(Topic, A, [Code-X | Path], Praise) :-
    step(buggy, Topic: Code, A >> X, none),
    praise(Topic, X, Path, Praise).
                    
%
% Wrong result, correct solutions not known
%
wrong(Topic, Item, Wrong, Wodden) :-
    findall(S, solution(Topic, Item, S, _), Solutions),
    wrong(Topic, Item, Solutions, Wrong, Wodden).

% Wrong result, correct solutions known
wrong(Topic, Item, Solutions, Wrong, Woodden) :-
    wrong(Topic, Item, Solutions, Wrong, [], Woodden).

% Arrived at goal
wrong(Topic, Wrong, Solutions, Wrong, Steps, Woodden) :-
    final(Topic, Wrong),
    \+ member(Wrong, Solutions),
    reverse(Steps, Woodden).

% Continue search
wrong(Topic, A, Solutions, B, Steps, Woodden) :-
    step(_, Topic: Code, A >> X, none),
    wrong(Topic, X, Solutions, B, [Code-X | Steps], Woodden).

%
% Feedback: all
%
feedback(Topic, Item, Path, Flags, Code_Feedback, Feedback) :-
    feedback(Topic, Item, Path, Flags, Code_Feedback),
    pairs_values(Code_Feedback, Feedback).

feedback(_, _, [], _, []).

feedback(Topic, A, [Code-X | Path], Flags, [Code-F | Feedback]) :-
    step(_, Topic: Code, A >> X, feedback(Flags, F)),
    feedback(Topic, X, Path, Flags, Feedback).

%
% Feedback: only mistakes
%
mistakes(Topic, Item, Path, Flags, Code_Mistakes, Mistakes) :-
    mistakes(Topic, Item, Path, Flags, Code_Mistakes),
    pairs_values(Code_Mistakes, Mistakes).

mistakes(_, _, [], _, []).

mistakes(Topic, A, [Code-X | Path], Flags, Mistakes) :-
    step(expert, Topic: Code, A >> X, none),
    mistakes(Topic, X, Path, Flags, Mistakes).

mistakes(Topic, A, [Code-X | Path], Flags, [Code-M | Mistakes]) :-
    step(buggy, Topic: Code, A >> X, feedback(Flags, M)),
    mistakes(Topic, X, Path, Flags, Mistakes).

%
% List of incorrect results
%
wrongs_paths_results(Topic, Item, WPR) :-
    solution(Topic, Item, Solution, _),
    % All wrong solutions incl. path and results
    findall(wrong(W, P, R, S-R, L), 
        ( wrong(Topic, Item, [Solution], W, P), 
          pairs_keys(P, C), 
          sort(C, S), 
          rod(W, R),
          mistakes(Topic, Item, P, [], M),
          length(M, L)
        ), List),
    % Avoid duplicate results in which only the steps are permuted
    sort(4, @<, List, Unique),
    % Count number of mistakes
    sort(5, @=<, Unique, Ascending),
    % Cleanup
    findall(wrong(Item, W, P, R), member(wrong(W, P, R, _, _), Ascending), WPR).
    
% 
% Extract abbreviations of steps
%
codes([_], []).

codes([A, X | Path], [Code | Codes]) :-
    step(_, Code, A >> X, _, feedback(_)),
    codes([X | Path], Codes).

%
% Find all buggy rules
%
% buggies(Topic, Code_Feedback) :-
%     findall(C-F, buggy(Topic: C, _, [color-auto], F, _), Code_Feedback).
%
buggies(Topic, Code_Bugs) :-
    item(Topic: Item),
    solution(Topic, Item, Solution, _),
    findall(Code_Feed, 
      ( wrong(Topic, Item, Solution, _, Path), 
        mistakes(Topic, Item, Path, [color-auto], Code_Feed, _)
      ), CF_List),
    append(CF_List, CF_All),
    sort(1, @<, CF_All, Code_Bugs).

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

mathex(fix, skip(_, Fn, Arg), Out) :-
    !,
    Expr =.. [Fn, Arg],
    mathex(fix, Expr, Out).

mathex(show, skip(_, _, Expr), Out) :-
    !,
    mathex(show, Expr, Out).

mathex(_, Out, Out) :-
    atomic(Out).

mathex(Mode, In, Out) :-
    compound(In),
    compound_name_arguments(In, Name, Args_in),
    maplist(mathex(Mode), Args_in, Args_out),
    compound_name_arguments(Out, Name, Args_out).

