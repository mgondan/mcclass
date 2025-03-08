:- module(mathml_wrapper, [mmlm//1, mmlm//2, colors/2]).

:- discontiguous mathml/0, math/2, math/3, math/4, current/3, paren/3, prec/3.
:- discontiguous type/3, denoting/3, ml/3, jax/3.

:- use_module(library(http/html_write)).
:- reexport(library(mathml)).
:- reexport(library(pval)).

% Legacy code from mcclass
mmlm(A) -->
    mmlm([], A).

mmlm(Flags, A) -->
    { member(denote(false), Flags),
      pl_mathml(A, M, Flags)
    },
    html(\M).

mmlm(Flags, A) -->
    { colors(A, Colors),
      append(Flags, Colors, Flags1),
      pl_mathml(A, M, Flags1)     % todo: With
    },
    html(\M).

colors(Expr, Flags) :-
    bugs(Expr, Bugs),
    findall(C, color(C), Colors),
    findall(color(B, C), (nth0(N, Bugs, B), N10 is N mod 10, nth0(N10, Colors, C)), Flags0),
    sort(Flags0, Flags).

color("dark").
color("$blue").   % #0d6efd
color("$indigo"). % #6610f2
color("$purple"). % #6f42c1
color("$pink").   % #d63384
color("$red").    % #dc3545
color("$orange"). % #fd7e14
color("$yellow"). % #ffc107
color("$green").  % #198754
color("$teal").   % #20c997
color("$cyan").   % #0dcaf0

% Bugs
bugs(Expr, Bugs) :-
    bugs_(Expr, List),
    sort(List, Bugs).

bugs_(instead(Bug, Wrong, _Correct), List)
 => bugs_(Wrong, Bugs),
    List = [Bug | Bugs].

bugs_(instead(Bug, Wrong, _Correct0, _Correct), List)
 => bugs_(Wrong, Bugs),
    List = [Bug | Bugs].

bugs_(omit_left(Bug, Expr), List)
 => Expr =.. [_Op, _L, R],
    bugs_(R, Bugs),
    List = [Bug | Bugs].

bugs_(omit_right(Bug, Expr), List)
 => Expr =.. [_Op, L, _R],
    bugs_(L, Bugs),
    List = [Bug | Bugs].

bugs_(omit(Bug, Expr), List)
 => bugs_(Expr, Bugs),
    List = [Bug | Bugs].

bugs_(add(Bug, Expr), List)
 => bugs_(Expr, Bugs),
    List = [Bug | Bugs].

bugs_(drop_left(Bug, Expr), List)
 => Expr =.. [_Op, _L, R],
    bugs_(R, Bugs),
    List = [Bug | Bugs].

bugs_(drop_right(Bug, Expr), List)
 => Expr =.. [_Op, L, _R],
    bugs_(L, Bugs),
    List = [Bug | Bugs].

bugs_(add_left(Bug, Expr), List)
 => bugs_(Expr, Bugs),
    List = [Bug | Bugs].

bugs_(add_right(Bug, Expr), List)
 => bugs_(Expr, Bugs),
    List = [Bug | Bugs].

bugs_(color(Col, Expr), List),
    atom(Col)
 => bugs_(Expr, Bugs),
    List = [Col | Bugs].

bugs_(X, Nil),
    atomic(X)
 => Nil = [].

bugs_(X, List),
    compound(X)
 => X =.. [_ | Args],
    maplist(bugs_, Args, Bugs),
    append(Bugs, List).

%
% Formatting numbers
%
math(hdrs(A), X, Flags, Flags1)
 => Flags1 = [digits(1) | Flags],
    A = X.

math(chi2ratio(A), X, Flags, Flags1)
 => Flags1 = [digits(2) | Flags],
    A = X.

% Hyphen
math(hyph(L, R), X)
 => X = list(&('#8209'), [L, R]).

% Box
%
math(boxes([], A), M)
 => A = M.

math(boxes([Col | Boxes], A), M)
 => M = color(Col, box(color("#000000", boxes(Boxes, A)))).

ml(box(A), M)
 => ml(A, X),
    M = menclose(notation(roundedbox), X).

paren(box(A), Paren, Flags)
 => paren(Flags, A, Paren).

prec(box(A), Prec, Flags)
 => prec(Flags, A, Prec).

type(box(A), Type, Flags)
 => type(Flags, A, Type).


% Intervals 
% Rendering intervals as: '1 ... 2'
%
math(ci(L, U), X, Flags, New)
=> New = Flags,
   X = brackets(list(',', [L, U])).

mathml:math_hook('...'(L, U), M) :-
    M = xfx(699, '...', L, U).

math(floor(A), X, Flags, New),
   number(A),
   A < 0
=> select_option(digits(D), Flags, New0, 2),
   New = [ceiling(D) | New0],
   X = A.

math(floor(A), X, Flags, New)
=> select_option(digits(D), Flags, New0, 2),
   New = [floor(D) | New0],
   X = A.

math(ceiling(A), X, Flags, New),
   number(A),
   A < 0
=> select_option(digits(D), Flags, New0, 2),
   New = [floor(D) | New0],
   X = A.

math(ceiling(A), X, Flags, New)
=> select_option(digits(D), Flags, N, 2),
   New = [ceiling(D) | N],
   X = A.

mathml:math_hook(omit_left(_Bug, Expr), M, Flags) :-
    option(error(correct), Flags, fix),
    !,
    M = Expr.

mathml:math_hook(omit_left(_Bug, Expr), M, Flags) :-
    option(error(show), Flags, fix),
    !,
    Expr =.. [_Op, _L, M].

mathml:math_hook(omit_left(Bug, Expr), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    M = list(space, [color(Bug, box(color("#000000", Expr1))), R]).

mathml:math_hook(omit_left(Bug, Expr), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    M = list(space, [color(Bug, cancel(color("#000000", Expr1))), R]).

mathml:math_hook(omit_right(_Bug, Expr), M, Flags) :-
    option(error(correct), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac]),
    !,
    M =.. [Op, Num, Den].

mathml:math_hook(omit_right(_Bug, Expr), M, Flags) :-
    option(error(show), Flags, fix),
    Expr =.. [Op, Num, _Den],
    member(Op, [frac, dfrac]),
    !,
    M = Num.

mathml:math_hook(omit_right(Bug, Expr), M, Flags) :-
    option(error(fix), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac]),
    !,
    M =.. [Op, Num, color(Bug, box(color("#000000", Den)))].

mathml:math_hook(omit_right(Bug, Expr), M, Flags) :-
    option(error(highlight), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac]),
    !,
    M =.. [Op, Num, color(Bug, cancel(color("#000000", Den)))].

% Powers
mathml:math_hook(omit_right(_Bug, Base^Pwr), M, Flags) :-
    option(error(correct), Flags, fix),
    M = Base^Pwr.

mathml:math_hook(omit_right(_Bug, Base^_Pwr), M, Flags) :-
    option(error(show), Flags, fix),
    !,
    M = Base.

mathml:math_hook(omit_right(Bug, Base^Pwr), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    M = Base^color(Bug, box(color("#000000", Pwr))).

mathml:math_hook(omit_right(Bug, Base^Pwr), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    M = Base^color(Bug, cancel(color("#000000", Pwr))).

mathml:math_hook(omit_right(_Bug, Expr), M, Flags) :-
    option(error(correct), Flags, fix),
    !,
    M = Expr.

mathml:math_hook(omit_right(_Bug, Expr), M, Flags) :-
    option(error(show), Flags, fix),
    !,
    Expr =.. [_Op, M, _R].

mathml:math_hook(omit_right(Bug, Expr), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    M = list(space, [L, color(Bug, box(color("#000000", Expr1)))]).

mathml:math_hook(omit_right(Bug, Expr), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    M = list(space, [L, color(Bug, cancel(color("#000000", Expr1)))]).

%
% Expert and buggy rules
%
math(expert(Flags, _, B), X, Flags)
 => X = B.

math(buggy(Flags, _, B), X, Flags)
 => X = B.

math(omit(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
 => M = Expr.

math(omit(_Bug, _Expr), M, Flags),
    option(error(show), Flags, fix)
 => M = "omitted".

math(omit(Bug, Expr), M, Flags),
    option(error(fix), Flags, fix)
 => M = color(Bug, box(color("#000000", Expr))).

math(omit(Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => M = color(Bug, cancel(color("#000000", Expr))).

math(add_left(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
 => Expr =.. [_Op, _L, R],
    M = R.

math(add_left(_Bug, Expr), M, Flags),
    option(error(show), Flags, fix)
 => M = Expr.

prec(add_left(_, Expr), Prec, Flags),
    option(error(show), Flags, fix)
 => prec(Flags, Expr, Prec).

math(add_left(_Bug, Expr), M, Flags),
    option(error(fix), Flags, fix)
 => Expr =.. [_Op, _L, R],
    M = R.

math(add_left(Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    M = list(space, [color(Bug, Expr1), R]).

prec(add_left(_, Expr), Prec, Flags),
    option(error(highlight), Flags, fix)
 => prec(Flags, Expr, Prec).

math(add_right(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
 => Expr =.. [_Op, L, _R],
    M = L.

math(add_right(_Bug, Expr), M, Flags),
    option(error(show), Flags, fix)
 => M = Expr.

prec(add_right(_, Expr), Prec, Flags),
    option(error(show), Flags, fix)
 => prec(Flags, Expr, Prec).

math(add_right(_Bug, Expr), M, Flags),
    option(error(fix), Flags, fix)
 => Expr =.. [_Op, L, _R],
    M = L.

math(add_right(Bug, L^R), M, Flags),
    option(error(highlight), Flags, fix)
 => M = L^color(Bug, R).

math(add_right(Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    M = list(space, [L, color(Bug, Expr1)]).

prec(add_right(_, Expr), Prec, Flags),
    option(error(highlight), Flags, fix)
 => prec(Flags, Expr, Prec).

math(add(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
=>  M = Expr.

math(add(_Bug, _Expr), M, Flags),
    option(error(show), Flags, fix)
 => M = "invented".

math(add(Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => M = color(Bug, box(color("#000000", Expr))). 

math(drop_left(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
 => Expr = M.

math(drop_left(_Bug, Expr), M, Flags),
    option(error(show), Flags, fix)
 => Expr =.. [_Op, _L, M].

math(drop_left(Bug, Expr), M, Flags),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    M = list(space, [color(Bug, Expr1), R]).

math(drop_left(_Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => Expr =.. [_Op, _L, M].

math(drop_right(_Bug, Expr), M, Flags),
    option(error(correct), Flags, fix)
 => M = Expr.

math(drop_right(_Bug, Expr), M, Flags),
    option(error(show), Flags, fix)
 => Expr =.. [_Op, M, _R].

math(drop_right(Bug, Expr), M, Flags),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    M = list(space, [L, color(Bug, Expr1)]).

math(drop_right(_Bug, Expr), M, Flags),
    option(error(highlight), Flags, fix)
 => Expr =.. [_Op, L, _R],
    M = L.

% Show correct alternative
mathml:math_hook(correct(Expr), Expr).
    
% Show error
mathml:math_hook(show(Expr), Expr).

% Fix error (with color)
mathml:math_hook(fix(Expr), Expr).

% Show error (with color)
mathml:math_hook(highlight(Expr), Expr).

mathml:math_hook(instead(Bug, Wrong, Correct), M) :-
    M = instead(Bug, Wrong, Correct, Correct).

mathml:math_hook(instead(_Bug, _Wrong, _Correct0, Correct), M, Flags) :-
    option(error(correct), Flags, fix),
    !,
    M = Correct.

mathml:math_hook(instead(Bug, Wrong, _Correct0, _Correct), M, Flags) :-
    option(error(show), Flags, fix),
    !,
    M = color(Bug, Wrong).

% Nested insteads
mathml:math_hook(instead(_, instead(Bug, Wrong, _), Correct0, Correct), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    M = instead(Bug, Wrong, Correct0, Correct).

mathml:math_hook(instead(_, instead(Bug, Wrong, _, _), Correct0, Correct), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    M = instead(Bug, Wrong, Correct0, Correct).

mathml:math_hook(instead(Bug, _Wrong, _Correct0, Correct), M, Flags) :-
    option(error(fix), Flags, fix),
    !,
    M = color(Bug, Correct).

denoting(instead(_, _Wrong, _Of0, Of), D, Flags),
    option(error(fix), Flags, fix)
 => denoting(Of, D, Flags).

denoting(instead(_, Wrong, _Of0, _Of), D, Flags),
    option(error(highlight), Flags, fix)
 => denoting(Wrong, D, Flags).

% Nested insteads
mathml:math_hook(instead(_, instead(Bug, Wrong, _), Correct0, Correct), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    M = instead(Bug, Wrong, Correct0, Correct).

mathml:math_hook(instead(_, instead(Bug, Wrong, _, _), Correct0, Correct), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    M = instead(Bug, Wrong, Correct0, Correct).

mathml:math_hook(instead(Bug, Wrong, Correct0, _Correct), M, Flags) :-
    option(error(highlight), Flags, fix),
    !,
    M = underbrace(color(Bug, show(Wrong)), list(space, ["instead of", correct(Correct0)])).

%
% t-test
%
mathml:math_hook(var_pool(V1, N1, V2, N2), X) :-
    X = dfrac((N1 - 1)*V1 + (N2 - 1)*V2, N1 + N2 - 2).

