% Translate mathematical expressions to MathML
:- module(mathml, [mml//1, mml//2, mmlm//1, mmlm//2, colors/2]).

:- multifile hook/4.

:- discontiguous mathml/0, current/3, paren/3, prec/3, type/3, denoting/3, ml/3, math/4.
:- use_module(library(http/html_write)).
:- use_module(library(lists)).

%
% mathml/0: Show the examples
% math/4: Translate complex expression to its components
% paren/3: Count level of parentheses
% prec/3: Determine operator precedence (e.g., addition after multiplication)
%

:- op(400, yfx, user:dot).

%
% Interface
%
% 1. Render as MathML
mml(A) -->
    mml([], A).

mml(Flags, A) -->
    { colors(A, Colors),
      append(Flags, Colors, List),
      mathml(List, A, M, _) 
    },
    html(M).

mmlm(A) -->
    mmlm([], A).

mmlm(Flags, {}(Args)) -->
    { colors({}(Args), Colors),
      append(Flags, Colors, List),
      semi(Args, [Arg1, _Arg2]),
      mathml(List, Arg1, M, W)
    },
    !,
    html([M, W]).

mmlm(Flags, {}(Args)) -->
    { colors({}(Args), Colors),
      append(Flags, Colors, List),
      semi(Args, ArgList0),
      reverse(ArgList0, [_ | Reversed]),
      reverse(Reversed, ArgList),
      maplist(mathml(List), ArgList, MList, WList),
      pairs_keys_values(Pairs, MList, WList),
      findall(li([M, W]), member(M-W, Pairs), Items)
    },
    !,
    html(ol(type(a), Items)).

mmlm(Flags, {}(Args) = Res) -->
    { colors({}(Args), Colors),
      append(Flags, Colors, List),
      semi(Args, ArgList0),
      reverse(ArgList0, [H | Reversed]),
      reverse([H = Res | Reversed], ArgList),
      maplist(mathml(List), ArgList, MList, WList),
      pairs_keys_values(Pairs, MList, WList),
      findall(li([M, W]), member(M-W, Pairs), Items)
    },
    !,
    html(ol(type(a), Items)).

mmlm(Flags, A) -->
    { colors(A, Colors),
      append(Flags, Colors, List),
      mathml(List, A, M, With) 
    },
    html([M, With]).

% 2. Show example
mathml :-
    writeln("Mathml examples").

mathml(A) :-
    mathml([], A).

mathml(Flags, A) :-
    mathml(Flags, A, M, With),
    format("A = ~w\n", A),
    format("M = ~w\n", M),
    format("W = ~w\n", With).

%
% Main function
%
mathml(Flags, A, X, With) :-
    ml(Flags, A, M),
    !,
    X = math(M),
    denoting(Flags, A, Denoting),
    ml(Flags, denoting(Denoting), With).

mathml(Flags, A, X, _) :-
    ml(Flags, "Conversion failed: ~w"-A, X).

%
% Try if expression matches with math/4 macro
%
ml(Flags, A, X),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => ml(New, M, X).

%
% Check if expression should be replaced by something else
%
math(Flags, A, New, X),
    member(replace(A, _), Flags)
 => select(replace(A, X), Flags, New).

%
% Check if a macro has been defined in an external module
%
math(Flags, A, New, X),
    option(task(Task), Flags, default),
    hook(Flags, A, New0, X0),
    option(task(Task), New0, Task)
 => New = New0,
    X = X0.

%
% Greek letters
%
math(Flags, A, New, X),
    atom(A),
    memberchk(A, [alpha, beta, gamma, delta, epsilon, varepsilon, zeta,
        eta, theta, vartheta, iota, kappa, lambda, mu, nu, xi, pi, rho,
        sigma, tau, upsilon, phi, varphi, chi, psi, omega, 'Gamma',
        'Delta', 'Theta', 'Lambda', 'Xi', 'Pi', 'Sigma', 'Upsilon',
        'Phi', 'Psi', 'Omega'])
 => New = Flags,
    X = greek(A).

ml(_Flags, greek(A), X) =>
    X = mi(&(A)).

type(_Flags, greek(_), Type)
 => Type = atomic.

denoting(_Flags, greek(_), Den)
 => Den = [].

mathml :- mathml(epsilon).

%
% Verbatim output
%
ml(_Flags, &(A), M),
    atom(A)
 => M = &(A).

type(_Flags, &(A), Type),
    atom(A)
 => Type = atomic.

denoting(_Flags, &(A), Den),
    atom(A)
 => Den = [].

%
% Summation sign
%
math(Flags, sum(A), New, X),
    prec(Flags, sum(A), Outer),
    prec(Flags, A, Inner),
    Outer =< Inner
 => Flags = New,
    X = sum(paren(A)).

ml(Flags, sum(A), M)
 => ml(Flags, A, X),
    M = mrow([mo(&(sum)), X]).

prec(_Flags, sum(_), Prec)
 => current(P, yfx, +),
    Prec is P - 1.

mathml :- mathml(sum(sub(x, i) + sub(y, i))).
mathml :- mathml(sum(sub(x, i)) + sum(sub(y, i))).

%
% Sum over index
%
% This function may be replaced in the future
math(Flags, sum(I, From, To, A), New, M)
 => Flags = New,
    M = underover(sum(A), I=From, To).

mathml :- mathml(sum(i, 1, 10, sub(x, i))).

%
% Absolute value
%
ml(Flags, abs(A), M)
 => ml(Flags, A, X),
    M = mrow([mo(&(vert)), X, mo(&(vert))]).

paren(Flags, abs(A), Paren)
 => paren(Flags, A, Paren).

%
% Integral
%
math(Flags, integrate(F), New, X),
    compound(F)
 => Flags = New,
    F =.. [_, D | _],
    X = integrate(F, D).

math(Flags, integrate(A, D), New, X),
    prec(Flags, integrate(A, D), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => Flags = New,
    X = integrate(paren(A), D).

ml(Flags, integrate(A, D), M)
 => ml(Flags, A, X),
    ml(Flags, space, Space),
    ml(Flags, D, DX),
    M = mrow([mo(&(int)), X, Space, mi(d), DX]).

prec(_Flags, integrate(_, _), Prec)
 => current(Prec, yfx, *).

paren(Flags, integrate(A, _), Paren)
 => paren(Flags, A, Paren).

mathml :- mathml(integrate(sin(x))).
mathml :- mathml(integrate(sin(x)) * integrate(cos(x))).
mathml :- mathml(integrate(sin(x) * cos(x), x)).

%
% Integrate over range
%
% This function may be replaced in the future
math(Flags, integrate(From, To, F, D), New, X),
    prec(Flags, integrate(F, D), Outer),
    prec(Flags, F, Inner),
    Outer < Inner
 => Flags = New,
    X = integrate(From, To, paren(F), D).

ml(Flags, integrate(From, To, F, D), M)
 => ml(Flags, From, FromX),
    ml(Flags, To, ToX),
    ml(Flags, F, FX),
    ml(Flags, space, Space),
    ml(Flags, D, DX),
    M = mrow([munderover([mo(&(int)), FromX, ToX]), FX, Space, mi(d), DX]).

prec(_Flags, integrate(_, _, _, _), Prec)
 => current(Prec, yfx, *).

paren(Flags, integrate(_, _, F, _), Paren)
 => paren(Flags, F, Paren).

mathml :- mathml(integrate(-1.0Inf, 1.0Inf, sin(x), x)).

% hats
ml(Flags, hat(A), M)
 => ml(Flags, A, X),
    M = mover(accent(true), [X, mo(&('Hat'))]).

paren(Flags, hat(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, hat(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, hat(A), Type)
 => type(Flags, A, Type).

mathml :- mathml(hat('K')).
mathml :- mathml(hat('K'^2)).
mathml :- mathml(hat('K')^2).
mathml :- mathml(hat('sigma')^2).

% tilde
ml(Flags, tilde(A), M)
 => ml(Flags, A, X),
    M = mover(accent(true), [X, mo(&(tilde))]).

paren(Flags, tilde(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, tilde(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, tilde(A), Type)
 => type(Flags, A, Type).

mathml :- mathml(tilde('D')).
mathml :- mathml(tilde('X')^2).

%
% Booleans
%
math(Flags, A, New, X),
    atom(A),
    memberchk(A, ['TRUE', 'FALSE'])
 => New = Flags,
    X = boolean(A).

denoting(_Flags, boolean(_), Den)
 => Den = [].

%
% Space
%
math(Flags, space, New, X)
 => New = Flags,
    X = space(thickmathspace).

ml(_Flags, space(Width), M)
 => M = mspace(width(Width), []).

denoting(_Flags, space(_), Den)
 => Den = [].

mathml :- mathml(space).

%
% No space
%
ml(_Flags, nospace, M)
 => M = &('#8288').

denoting(_Flags, nospace, Den)
 => Den = [].

mathml :- mathml(nospace).

%
% Hyphen
%
math(Flags, hyph(L, R), New, X)
 => New = Flags,
    X = list(&('#8209'), [L, R]).

mathml :- mathml(hyph(t, "test")).

%
% Symbols/Identifiers
%
math(Flags, A, New, X),
    atom(A)
 => New = Flags,
    X = ident(A).

ml(_Flags, ident(A), X)
 => X = mi(A).

type(_Flags, ident(_), Type)
 => Type = atomic.

denoting(_Flags, ident(_), Den)
 => Den = [].

mathml :- mathml(i).

%
% Upright text
%
math(Flags, A, New, X),
    string(A)
 => New = Flags,
    X = text(A).

ml(_Flags, text(A), X)
 => X = mtext(A).

type(_Flags, text(_), Type)
 => Type = atomic.

denoting(_Flags, text(_), Den)
 => Den = [].

mathml :- mathml("Text").

%
% Mathematical signs
%
ml(_Flags, sign(=:=), X)
 => X = mo(=).

ml(_Flags, sign(=\=), X)
 => X = mo(&(ne)).

ml(_Flags, sign(dot), X)
 => X = mo(&(sdot)).

ml(_Flags, sign(=<), X)
 => X = mo(&(le)).

ml(_Flags, sign(>=), X)
 => X = mo(&(ge)).

ml(_Flags, sign(':='), X)
 => X = mo(&('Assign')).

ml(_Flags, sign(;), X)
 => X = mo(&(semi)).

ml(_Flags, sign(~), X)
 => X = mo(&('Tilde')).

ml(_Flags, sign(sum), X)
 => X = mo(&(sum)).

current(Prec, fy, sum)
 => current_op(Prec, yfx, +).

ml(_Flags, sign(!), X)
 => X = mo(!).

current(Prec, yf, !)
 => current_op(P, yfx, *),
    Prec is P - 1.

% default
ml(_Flags, sign(A), X)
 => X = mo(A).

% prec(_Flags, sign(A), Prec),
%     current(P, _Fix, A)
%  => Prec = P.

denoting(_Flags, sign(_), Den)
 => Den = [].

mathml :- mathml(sign(dot)).

%
% Indices like s_D: needs operator []/2 for pretty printing
%
math(Flags, [](Idx, A), New, X)
 => math(Flags, sub(A, Idx), New, X).

mathml :- mathml([](i, x)).

%
% Check for sub(sup(A, Power), Index)
%
math(Flags, sub(A, Idx), New, X),
    type(Flags, A, sup(Bas, Pwr))
 => New = [replace(sup(Bas, Pwr), subsup(Bas, Idx, Pwr)) | Flags],
    X = A.

%
% Render
%
math(Flags, sub(A, Idx), New, X),
    prec(Flags, sub(A, Idx), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = sub(paren(A), Idx).

ml(Flags, sub(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = msub([X, Y]).

paren(Flags, sub(A, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, sub(A, _), Prec)
 => prec(Flags, A, Prec).

type(_Flags, sub(A, B), Type)
 => Type = sub(A, B).

mathml :- mathml(sub(s, 'D')).
mathml :- mathml(sub(s^r, 'D')).

%
% Powers like s^D
%
% Check for sup(sub(A, Index), Power)
%
math(Flags, sup(A, Pwr), New, X),
    type(Flags, A, sub(Bas, Idx))
 => New = [replace(sub(Bas, Idx), subsup(Bas, Idx, Pwr)) | Flags],
    X = A.

%
% Render
%
math(Flags, sup(A, Pwr), New, X),
    prec(Flags, sup(A, Pwr), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = sup(paren(A), Pwr).

ml(Flags, sup(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = msup([X, Y]).

paren(Flags, sup(A, _), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, sup(_, _), Prec)
 => current(Prec, xfy, ^).

type(_Flags, sup(A, B), Type)
 => Type = sup(A, B).

mathml :- mathml(sub(s, 'D')).
mathml :- mathml(sub(s^2, 'D')).

%
% Index and Exponent: s_D^2
%
math(Flags, subsup(A, Idx, Pwr), New, X),
    prec(Flags, subsup(A, Idx, Pwr), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = subsup(paren(A), Idx, Pwr).

ml(Flags, subsup(A, B, C), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    ml(Flags, C, Z),
    M = msubsup([X, Y, Z]).

paren(Flags, subsup(A, _, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, subsup(A, _, C), Prec)
 => prec(Flags, sup(A, C), Prec).

type(_Flags, subsup(A, B, C), Type)
 => Type = subsup(A, B, C).

mathml :- mathml(subsup(s, 'D', r)).

%
% Indices like s_D
%
math(Flags, '['(A, Idx), New, X)
 => math(Flags, sub(A, Idx), New, X).

%
% Check for under(over(A, Power), Index)
%
math(Flags, under(A, Idx), New, X),
    type(Flags, A, over(Bas, Pwr))
 => New = [replace(over(Bas, Pwr), underover(Bas, Idx, Pwr)) | Flags],
    X = A.

%
% Render
%
ml(Flags, under(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = munder([X, Y]).

paren(Flags, under(A, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, under(A, _), Prec)
 => prec(Flags, A, Prec).

type(_Flags, under(A, B), Type)
 => Type = under(A, B).

mathml :- mathml(under(sign(&(sum)), i)).
mathml :- mathml(under(over(sign(&(sum)), 'N'), i=1)).

%
% Check for over(under(A, Index), Power)
%
math(Flags, over(A, Pwr), New, X),
    type(Flags, A, under(Bas, Idx))
 => New = [replace(under(Bas, Idx), underover(Bas, Idx, Pwr)) | Flags],
    X = A.

%
% Render
%
ml(Flags, over(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = mover([X, Y]).

paren(Flags, over(A, _), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, over(_, _), Prec)
 => current(Prec, xfy, ^).

type(_Flags, over(A, B), Type)
 => Type = over(A, B).

mathml :- mathml(over(under(sign(&(sum)), i=1), 'N')).

%
% Under and Over
%
ml(Flags, underover(A, B, C), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    ml(Flags, C, Z),
    M = munderover([X, Y, Z]).

paren(Flags, underover(A, _, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, underover(A, _, C), Prec)
 => prec(Flags, over(A, C), Prec).

type(_Flags, underover(A, B, C), Type)
 => Type = underover(A, B, C).

mathml :- mathml(underover(sign(&(sum)), i=1, 'N')).

%
% Numbers
%
math(Flags, A, New, X),
    number(A),
    A < 0
 => New = Flags,
    Abs is abs(A),
    X = -number(Abs).

math(Flags, A, New, X),
    number(A)
 => New = Flags,
    X = number(A).

math(Flags, number(A), New, X),
    integer(A)
 => New = Flags,
    X = integer(A).

ml(_Flags, integer(A), M)
 => M = mn(A).

denoting(_Flags, integer(_), D)
 => D = [].

ml(_Flags, number(1.0Inf), M)
 => M = mi(&('#x221E')).

ml(_Flags, number(1.5NaN), M)
 => M = mtext("?").

ml(Flags, number(A), M),
    option(round(D), Flags, 2),
    D = 0
 => format(string(R), '~0f', [A]),
    M = mn(R).

% This code is a bit nonstandard. Due to sandbox restrictions (format allows
% executing code), I cannot put the desired precision in a string such as 
% format(string(F), '~~~df', D) and then use F in a second format command.
% Therefore, I format the number with 99 decimals and then truncate the result.
ml(Flags, number(A), M),
    option(floor(D), Flags),
    D =< 99
 => format(codes(X), '~99f', [A]),
    % Search for the comma
    nth1(Dot, X, 46),
    N is Dot + D,
    findall(E, (nth1(I, X, E), I =< N), Round),
    string_codes(S, Round),
    M = mn(S).

ml(Flags, number(A), M),
    option(ceiling(D), Flags),
    D =< 99
 => format(codes(X), '~99f', [A + 1.0*10^(-D)]),
    % Search for the comma
    nth1(Dot, X, 46),
    N is Dot + D,
    findall(E, (nth1(I, X, E), I =< N), Round),
    string_codes(S, Round),
    M = mn(S).

ml(Flags, number(A), M),
    option(round(D), Flags, 2),
    D =< 99
 => format(codes(X), '~99f', [A + 0.5*10^(-D)]),
    % Search for the comma
    nth1(Dot, X, 46),
    N is Dot + D,
    findall(E, (nth1(I, X, E), I =< N), Round),
    string_codes(S, Round),
    M = mn(S).

denoting(_Flags, number(_), D)
 => D = [].

mathml :- mathml(3).
mathml :- mathml(-3).
mathml :- mathml(1.0Inf).
mathml :- mathml(-1.0Inf).
mathml :- mathml(3.444).
mathml :- mathml(3.555).
mathml :- mathml(-3.444).
mathml :- mathml(-3.555).

%
% Operators
%
math(Flags, A = B, New, X)
 => New = Flags,
    current_op(Prec, xfx, =),
    X = yfy(Prec, =, A, B).

math(Flags, A < B, New, X)
 => New = Flags,
    current_op(Prec, xfx, <),
    X = yfy(Prec, <, A, B).

math(Flags, A =< B, New, X)
 => New = Flags,
    current_op(Prec, xfx, =<),
    X = yfy(Prec, =<, A, B).

math(Flags, ~(A, B), New, X)
 => New = Flags,
    current_op(Prec, xfx, =),
    X = yfy(Prec, ~, A, B).

math(Flags, A > B, New, X)
 => New = Flags,
    current_op(Prec, xfx, >),
    X = yfy(Prec, >, A, B).

math(Flags, A >= B, New, X)
 => New = Flags,
    current_op(Prec, xfx, >=),
    X = yfy(Prec, >=, A, B).

math(Flags, +A, New, X)
 => New = Flags,
    current_op(Prec, yfx, +),
    X = fy(Prec, +, A).

math(Flags, A + B, New, X)
 => New = Flags,
    current_op(Prec, yfx, +),
    X = yfy(Prec, +, A, B).

math(Flags, -A, New, X)
 => New = Flags,
    current_op(Prec, yfx, -),
    X = fx(Prec, -, A).

math(Flags, A - B, New, X)
 => New = Flags,
    current_op(Prec, yfx, -),
    X = yfx(Prec, -, A, B).

% Use dot or no dot instead of asterisk
math(Flags, A * B, New, X),
    type(Flags, A, atomic),
    type(Flags, B, atomic)
 => New = Flags,
    X = nodot(A, B).

math(Flags, A * B, New, X)
 => New = Flags,
    X = dot(A, B).

math(Flags, dot(A, B), New, X)
 => New = Flags,
    current_op(Prec, yfx, dot),
    X = yfy(Prec, dot, A, B).

math(Flags, nodot(A, B), New, X)
 => New = Flags,
    current_op(Prec, yfx, *),
    X = yfy(Prec, &('#x2062'), A, B).

math(Flags, A / B, New, X)
 => New = Flags,
    current_op(Prec, yfx, /),
    X = yfx(Prec, /, A, B).

math(Flags, (A ; B), New, X)
 => New = Flags,
    current_op(Prec, xfy, ;),
    X = xfy(Prec, ;, A, B).

math(Flags, A^B, New, X)
 => New = Flags,
    X = sup(A, B).

% General case
math(Flags, Binary, New, X),
    compound(Binary),
    compound_name_arguments(Binary, Op, [A, B]),
    current(Prec, Fix, Op),
    Fix = yfx
 => New = Flags,
    X = yfx(Prec, Op, A, B).

math(Flags, Binary, New, X),
    compound(Binary),
    compound_name_arguments(Binary, Op, [A, B]),
    current(Prec, Fix, Op),
    Fix = xfy
 => New = Flags,
    X = xfy(Prec, Op, A, B).

math(Flags, Binary, New, X),
    compound(Binary),
    compound_name_arguments(Binary, Op, [A, B]),
    current(Prec, Fix, Op),
    Fix = xfx
 => New = Flags,
    X = xfx(Prec, Op, A, B).

mathml :- mathml(a * b).
mathml :- mathml(a * (b * c)).
mathml :- mathml((a * b) * c).
mathml :- mathml((2 * b) * c).
mathml :- writeln("Unsure if this can be convinced to omit the parenthesis"), 
          mathml((-2 * b) * c).
mathml :- mathml((-2 * 2) * c).
mathml :- mathml((-2 * -2) * c).

%
% Render
%
ml(Flags, yf(Prec, Op, A), M)
 => ml(Flags, sign(Op), S),
    ml(Flags, left(Prec, A), X),
    M = mrow([X, S]).

ml(Flags, fy(Prec, Op, A), M)
 => ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, A), X),
    M = mrow([S, X]).

ml(Flags, fx(Prec, Op, A), M)
 => ml(Flags, sign(Op), S),
    ml(Flags, right(Prec-1, A), X),
    M = mrow([S, X]).

ml(Flags, xfx(Prec, Op, A, B), M)
 => ml(Flags, left(Prec-1, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec-1, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, yfx(Prec, Op, A, B), M)
 => ml(Flags, left(Prec, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec-1, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, xfy(Prec, Op, A, B), M)
 => ml(Flags, left(Prec-1, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, yfy(Prec, Op, A, B), M)
 => ml(Flags, left(Prec, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, B), Y),
    M = mrow([X, S, Y]).

paren(Flags, fy(_Prec, _Op, A), P)
 => paren(Flags, A, P).

paren(Flags, fx(_Prec, _Op, A), P)
 => paren(Flags, A, P).

paren(Flags, xf(_Prec, _Op, A), P)
 => paren(Flags, A, P).

paren(Flags, yf(_Prec, _Op, A), P)
 => paren(Flags, A, P).

paren(Flags, xfx(Prec, _Op, A, B), P)
 => paren(Flags, left(Prec, A), P1),
    paren(Flags, right(Prec, B), P2),
    P is max(P1, P2).

paren(Flags, xfy(Prec, _Op, A, B), P)
 => paren(Flags, left(Prec, A), P1),
    paren(Flags, right(Prec, B), P2),
    P is max(P1, P2).

paren(Flags, yfx(Prec, _Op, A, B), P)
 => paren(Flags, left(Prec, A), P1),
    paren(Flags, right(Prec, B), P2),
    P is max(P1, P2).

paren(Flags, yfy(Prec, _Op, A, B), P)
 => paren(Flags, left(Prec, A), P1),
    paren(Flags, right(Prec, B), P2),
    P is max(P1, P2).

denoting(Flags, fy(_, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, xfx(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, xfy(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, yfx(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, yfy(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

prec(_Flags, fy(P, _, _), Prec)
 => Prec = P.

prec(_Flags, xfx(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, yfx(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, xfy(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, yfy(P, _, _, _), Prec)
 => Prec = P.

math(Flags, left(Prec, A), New, X),
    prec(Flags, A, P),
    P > Prec
 => New = Flags,
    X = paren(A).

math(Flags, left(_, A), New, X)
 => New = Flags,
    X = A.

math(Flags, right(Prec, A), New, X)
 => New = Flags,
    X = left(Prec, A).

denoting(Flags, left(_, A), Den)
 => denoting(Flags, A, Den).

denoting(Flags, right(_, A), Den)
 => denoting(Flags, A, Den).

mathml :- mathml(a * b).
mathml :- mathml((a + b) * (c + d)).
mathml :- mathml(a * b + c * d).
mathml :- mathml(a + b + c + d).
mathml :- mathml((a - b) - (c + d)).
mathml :- mathml(dot((a - b), (c + d))).
mathml :- mathml(-2 * -2).

%
% Abbreviations
%
% with s^2_pool denoting the pooled variance
%
ml(Flags, abbrev(A, _, _), X)
 => ml(Flags, A, X).

paren(Flags, abbrev(A, _, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, abbrev(A, _, _), Prec)
 => prec(Flags, A, Prec).

type(Flags, abbrev(A, _, _), Type)
 => type(Flags, A, Type).

denoting(Flags, abbrev(A, Expr, Info), Den)
 => denoting(Flags, Expr, T),
    Den = [denoting(A, Expr, Info) | T].

mathml :-
    S2P = abbrev(sub(s, "pool")^2,
                   frac((sub('N', "A") - 1) * sub(s, "A")^2 +
                        (sub('N', "B") - 1) * sub(s, "B")^2,
                        sub('N', "A") + sub('N', "B") - 2),
                   "the pooled variance"),
    mathml(frac(sub(overline('X'), "A") - sub(overline('X'), "B"),
                  sqrt(nodot(S2P, 1/sub('N', "A") + 1/sub('N', "B"))))).

%
% Expand abbreviations
%
ml(Flags, denoting(A, Expr, Info), X)
 => ml(Flags, list(space, [A = Expr, "denoting", Info]), X).

type(Flags, denoting(A, _, _), Type)
 => type(Flags, A, Type).

denoting(_Flags, denoting(_, _, _), Den)
 => Den = [].

%
% Collect abbreviations
%
ml(Flags, denoting(Abbreviations), X)
 => sort(Abbreviations, Sorted), % remove duplicates
    ml(Flags, denoting_(Sorted), X).

ml(_Flags, denoting_([]), W)
 => W = " ".

ml(Flags, denoting_([A]), W)
 => ml(Flags, A, X),
    W = span([", with", &(nbsp), math(X)]).

ml(Flags, denoting_([A, B | T]), W)
 => ml(Flags, A, X),
    ml(Flags, and([B | T]), Y),
    W = span([", with", &(nbsp), math(X) | Y]).

ml(_Flags, and([]), W)
 => W = ".".

ml(Flags, and([A | T]), W)
 => ml(Flags, A, X),
    ml(Flags, and(T), Y),
    W = span([", and", &(nbsp), math(X) | Y]).

%
% Parentheses
%
ml(Flags, paren(A), X),
    paren(Flags, A, P),
    0 is P mod 3
 => ml(Flags, parentheses(A), X).

ml(Flags, paren(A), X),
    paren(Flags, A, P),
    1 is P mod 3
 => ml(Flags, brackets(A), X).

ml(Flags, paren(A), X),
    paren(Flags, A, P),
    2 is P mod 3
 => ml(Flags, braces(A), X).

paren(Flags, paren(A), Paren)
 => paren(Flags, A, P),
    Paren is P + 1.

ml(Flags, parentheses(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('('), X, mo(')')]).

paren(_Flags, parentheses(_), Paren)
 => Paren is 1.

ml(Flags, brackets(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('['), X, mo(']')]).

paren(_Flags, brackets(_), Paren)
 => Paren is 2.

ml(Flags, braces(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('{'), X, mo('}')]).

paren(_Flags, braces(_), Paren)
 => Paren is 3.

mathml :- mathml(paren(paren(paren(paren(i))))).

%
% Matrices
%
ml(Flags, ##(Rows), M)
 => maplist(ml(Flags), Rows, X),
    M = mtable(X).

paren(Flags, ##([Row1]), Paren)
 => paren(Flags, Row1, Paren).

paren(_Flags, ##(_), Paren)
 => Paren is 0.

mathml :- mathml(##([#([1, 0, 0]), #([0, 1, 0]), #([0, 0, 1])])).

%
% Row vectors
%
ml(Flags, #(Cells), M)
 => findall(cell(C), member(C, Cells), CCells),
    maplist(ml(Flags), CCells, X),
    M = mtr(X).

paren(Flags, #(Cells), Paren)
 => mapargs(paren(Flags), Cells, Ps),
    max_list(Ps, Paren).

% with attributes (e.g., mathbackground(blue))
ml(Flags, #(Options, Cells), M)
 => findall(cell(C), member(C, Cells), CCells),
    maplist(ml(Flags), CCells, X), 
    M = mtr(Options, X).

paren(Flags, #(_Options, Cells), Paren)
 => mapargs(paren(Flags), Cells, Ps),
    max_list(Ps, Paren).

%
% Table cell
%
ml(Flags, cell(Cell), M)
 => ml(Flags, Cell, X),
    M = mtd(X).

%
% Lists of things
%
math(Flags, [H | T], New, M)
 => Flags = New, 
    M = list(&('#8288'), [H | T]).

ml(Flags, list(_, [A]), M)
 => ml(Flags, A, M).

ml(Flags, list(Sep, [A, B | T]), M)
 => ml(Flags, A, X),
    ml(Flags, tail(Sep, [B | T]), Y),
    M = mrow([X | Y]).

ml(Flags, tail(Sep, [A]), M)
 => ml(Flags, Sep, S),
    ml(Flags, A, X),
    M = [S, X].

ml(Flags, tail(Sep, [A, B | T]), M)
 => ml(Flags, Sep, S),
    ml(Flags, A, X),
    ml(Flags, tail(Sep, [B | T]), Y),
    M = [S, X | Y].

paren(Flags, list(_, List), Paren)
 => maplist(paren(Flags), List, Parens),
    max_list(Parens, Paren).

prec(Flags, list(_, [A]), Prec)
 => prec(Flags, A, Prec).

prec(Flags, list(Sep, [_, _ | _]), Prec)
 => prec(Flags, Sep, Prec).

denoting(Flags, list(_, L), Den)
 => maplist(denoting(Flags), L, List),
    append(List, Den).

mathml :- mathml(list(space, [i, j, k])).

%
% Steps in an R calculation
%
math(Flags, {}(Args), New, M)
 => Flags = New,
    semi(Args, L),
    M = list(sign(;), L).

mathml :- mathml({odds_a = frac(p_a, 1-p_a); odds_b = odds_a*2; p_b = frac(odds_b, 1 + odds_b)}).

% Translate a;b;c to list
semi(A ; B, X)
 => semi(B, T),
    X = [A | T].

semi(A, X)
 => X = [A].

%
% Fractions
%
ml(Flags, frac(N, D), M)
 => ml(Flags, N, X),
    ml(Flags, D, Y),
    M = mfrac([X, Y]).

paren(_Flags, frac(_, _), Paren)
 => Paren = 0.

prec(_Flags, frac(_, _), Prec)
 => current(P, yfx, /),
    Prec is P - 1.

math(Flags, omit_right(_Bug, Expr), New, M),
    option(error(correct), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac])
 => Flags = New,
    M =.. [Op, Num, Den].

math(Flags, omit_right(_Bug, Expr), New, M),
    option(error(show), Flags, fix),
    Expr =.. [Op, Num, _Den],
    member(Op, [frac, dfrac])
 => Flags = New,
    M = Num.

math(Flags, omit_right(bug(Bug), Expr), New, M),
    option(error(fix), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac])
 => Flags = New,
    M =.. [Op, Num, color(Bug, box(color("#000000", Den)))].

math(Flags, omit_right(bug(Bug), Expr), New, M),
    option(error(highlight), Flags, fix),
    Expr =.. [Op, Num, Den],
    member(Op, [frac, dfrac])
 => M =.. [Op, Num, color(Bug, cancel(color("#000000", Den)))],
    Flags = New.

mathml :- mathml(frac(1, pi)).

%
% Large fraction
%
math(Flags, dfrac(N, D), New, X)
 => New = Flags,
    X = display(frac(N, D)).

mathml :- mathml(dfrac(1, pi)).

%
% Square root
%
ml(Flags, sqrt(A), M)
 => ml(Flags, A, X),
    M = msqrt(X).

prec(_Flags, sqrt(_), Prec)
 => current(P, xfy, ^),
    Prec is P + 1.

mathml :- mathml(sqrt(2)).
mathml :- mathml(sqrt(2)^2).

%
% Large font ("displaystyle")
%
ml(Flags, display(A), M)
 => ml(Flags, A, X),
    M = mstyle(displaystyle(true), X).

type(Flags, display(A), Type)
 => type(Flags, A, Type).

%
% Decorations
%
ml(Flags, overline(A), M)
 => ml(Flags, A, X),
    M = mover(accent(true), [X, mo(&(macr))]).

paren(Flags, overline(A), Paren)
 => paren(Flags, A, Paren).

% Put overline(x)^2 in parentheses
prec(_Flags, overline(_), Prec)
 => current(Prec, yfx, *).

type(Flags, overline(A), Type)
 => type(Flags, A, Type).

mathml :- mathml(overline('D')).

%
% Cancel out
%
ml(Flags, cancel(A), M)
 => ml(Flags, A, X),
    M = menclose(notation(updiagonalstrike), X).

paren(Flags, cancel(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, cancel(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, cancel(A), Type)
 => type(Flags, A, Type).

mathml :- mathml(cancel('D')).

%
% Box
%
ml(Flags, box(A), M)
 => ml(Flags, A, X),
    M = menclose(notation(roundedbox), X).

paren(Flags, box(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, box(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, box(A), Type)
 => type(Flags, A, Type).

test :- test(box('D')).

%
% Underbrace
%
ml(Flags, underbrace(A, U), M)
 => ml(Flags, A, X),
    ml(Flags, U, Y),
    M = munder([munder(accentunder(true),
                  [Y, mo(stretchy(true), &('UnderBrace'))]), X]).

paren(Flags, underbrace(A, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, underbrace(A, _), Prec)
 => prec(Flags, A, Prec).

type(Flags, underbrace(A, _), Type)
 => type(Flags, A, Type).

mathml :- mathml(underbrace('D', u)).

%
% Color palette from Bootstrap CSS
%
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

ml(Flags, color(C, A), M),
    atom(C)
 => member(color(C, S), Flags),
    ml(Flags, color(S, A), M).

ml(Flags, color(C, A), M),
    string(C)
 => ml(Flags, A, X),
    M = mstyle(mathcolor(C), X).

%
% Mistakes
%
% Show correct alternative
math(Flags, correct(Expr), New, M)
 => New = [error(correct) | Flags],
    M = Expr.

% Show error
math(Flags, show(Expr), New, M)
 => New = [error(show) | Flags],
    M = Expr.

% Fix error (with color)
math(Flags, fix(Expr), New, M)
 => New = [error(fix) | Flags],
    M = Expr.

% Show error (with color)
math(Flags, highlight(Expr), New, M)
 => New = [error(highlight) | Flags],
    M = Expr.

math(Flags, omit_left(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Flags = New,
    M = Expr.

math(Flags, omit_left(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => Flags = New,
    Expr =.. [_Op, _L, M].

math(Flags, omit_left(bug(Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    Flags = New,
    M = list(space, [color(Bug, box(color("#000000", Expr1))), R]).

math(Flags, omit_left(bug(Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    Flags = New,
    M = list(space, [color(Bug, cancel(color("#000000", Expr1))), R]).

math(Flags, omit_right(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Flags = New,
    M = Expr.

math(Flags, omit_right(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => Flags = New,
    Expr =.. [_Op, M, _R].

math(Flags, omit_right(bug(Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    Flags = New,
    M = list(space, [L, color(Bug, box(color("#000000", Expr1)))]).

math(Flags, omit_right(bug(Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    Flags = New,
    M = list(space, [L, color(Bug, cancel(color("#000000", Expr1)))]).

math(Flags, drop_left(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Flags = New,
    M = Expr.

math(Flags, drop_left(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => Flags = New,
    Expr =.. [_Op, _L, M].

math(Flags, drop_left(bug(Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    Flags = New,
    M = list(space, [color(Bug, Expr1), R]).

math(Flags, drop_left(bug(_Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [_Op, _L, R],
    Flags = New,
    M = R.

math(Flags, drop_right(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Flags = New,
    M = Expr.

math(Flags, drop_right(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => Flags = New,
    Expr =.. [_Op, M, _R].

math(Flags, drop_right(bug(Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    Flags = New,
    M = list(space, [L, color(Bug, Expr1)]).

math(Flags, drop_right(bug(_Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [_Op, L, _R],
    Flags = New,
    M = L.

math(Flags, instead(_Bug, _Wrong, Correct), New, M),
    option(error(correct), Flags, fix)
 => Flags = New,
    M = Correct.

math(Flags, instead(bug(Bug), Wrong, _Correct), New, M),
    option(error(show), Flags, fix)
 => Flags = New,
    M = color(Bug, Wrong).

math(Flags, instead(bug(Bug), _Wrong, Correct), New, M),
    option(error(fix), Flags, fix)
 => Flags = New,
    M = color(Bug, box(color("#000000", Correct))).

math(Flags, instead(bug(Bug), Wrong, Correct), New, M),
    option(error(highlight), Flags, fix)
 => New = Flags,
    M = underbrace(list(space, ["instead of", correct(Correct)]), color(Bug, show(Wrong))).

math(Flags, invent_left(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Expr =.. [_Op, _L, R],
    Flags = New,
    M = R.

math(Flags, invent_left(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => M = Expr,
    Flags = New.

math(Flags, invent_left(bug(_Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [_Op, _L, R],
    Flags = New,
    M = R.

math(Flags, invent_left(bug(Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, L, " "],
    Flags = New,
    M = list(space, [color(Bug, Expr1), R]).

math(Flags, invent_right(_Bug, Expr), New, M),
    option(error(correct), Flags, fix)
 => Expr =.. [_Op, L, _R],
    Flags = New,
    M = L.

math(Flags, invent_right(_Bug, Expr), New, M),
    option(error(show), Flags, fix)
 => M = Expr,
    Flags = New.

math(Flags, invent_right(bug(_Bug), Expr), New, M),
    option(error(fix), Flags, fix)
 => Expr =.. [_Op, L, _R],
    Flags = New,
    M = L.

math(Flags, invent_right(bug(Bug), L^R), New, M),
    option(error(highlight), Flags, fix)
 => Flags = New,
    M = L^color(Bug, R).

math(Flags, invent_right(bug(Bug), Expr), New, M),
    option(error(highlight), Flags, fix)
 => Expr =.. [Op, L, R],
    Expr1 =.. [Op, " ", R],
    Flags = New,
    M = list(space, [L, color(Bug, Expr1)]).

mathml :- mathml(dfrac(omit_right(bug(bug), overline('D') - mu),
                   sub(s, 'D') / sqrt('N'))).

mathml :- writeln("Same with Flags = error(fix)"),
    mathml([error(fix)], 
           dfrac(omit_right(bug(bug), overline('D') - mu), sub(s, 'D') / sqrt('N'))).

mathml :- writeln("Same with Flags = error(highlight)"),
    mathml([error(highlight)], 
           dfrac(omit_right(bug(bug), overline('D') - mu), sub(s, 'D') / sqrt('N'))).
    
mathml :- writeln("Same with Flags = error(show)"),
    mathml([error(show)], 
           dfrac(omit_right(bug(bug), overline('D') - mu), sub(s, 'D') / sqrt('N'))).

mathml :- writeln("Same with Flags = error(correct)"),
    mathml([error(correct)],
           dfrac(omit_right(bug(bug), overline('D') - mu), sub(s, 'D') / sqrt('N'))).
    
mathml :- mathml(dfrac(overline('D') - mu,
                   sub(s, 'D') / instead(bug(bug), 'N', sqrt('N')))).

mathml :- writeln("Same with Flags = error(fix)"),
    mathml([error(fix)], 
           dfrac(overline('D') - mu, sub(s, 'D') / instead(bug(bug), 'N', sqrt('N')))).

mathml :- writeln("Same with Flags = error(highlight)"),
    mathml([error(highlight)], 
           dfrac(overline('D') - mu, sub(s, 'D') / instead(bug(bug), 'N', sqrt('N')))).

mathml :- writeln("Same with Flags = error(show)"),
    mathml([error(show)], 
           dfrac(overline('D') - mu, sub(s, 'D') / instead(bug(bug), 'N', sqrt('N')))).

%
% Expert and buggy rules
%
math(Flags, expert(Flags, _, B), New, X)
 => New = Flags,
    X = B.

math(Flags, buggy(Flags, _, B), New, X)
 => New = Flags,
    X = B.

%
% t-test
%
math(Flags, var_pool(V1, N1, V2, N2), New, X)
 => New = Flags,
    X = dfrac((N1 - 1)*V1 + (N2 - 1)*V2, N1 + N2 - 2).
    
%
% Binomial coefficient and distribution
%
ml(Flags, choose(N, K), M)
 => ml(Flags, N, X),
    ml(Flags, K, Y),
    M = mrow([mo('('), mfrac([linethickness(0)], [X, Y]), mo(')')]).

paren(_Flags, choose(_, _), Paren)
 => Paren = 1.

prec(_Flags, choose(_, _), Prec)
 => Prec = 0.

type(_Flags, choose(_, _), Type)
 => Type = paren.

mathml :- mathml(choose('N', k)).

mathml :- mathml(choose('N', k) * pi^k * (1 - pi)^('N' - k)).

math(Flags, factorial(N), New, M)
 => Flags = New,
    current(Prec, yf, !),
    M = yf(Prec, !, N).

mathml :- mathml(factorial('N')).

mathml :- mathml(factorial('N'-k)).

mathml :- mathml(factorial('N'*k)).

mathml :- mathml(factorial('N')^2).

% The name is a bit unfortunate
math(Flags, bernoulli(K, N, Pi), New, M)
 => Flags = New,
    M = successes(K, Pi) * failures(N - K, 1 - Pi).

math(Flags, successes(K, Pi), New, M)
 => Flags = New,
    M = Pi^K.

% This may change
math(Flags, failures(K, Pi), New, M)
 => Flags = New,
    M = Pi^K.

% Density, distribution etc.
math(Flags, dbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' = K] ; [N, Pi])).

math(Flags, pbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' =< K] ; [N, Pi])).

% upper tail
math(Flags, upbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' >= K] ; [N, Pi])).

% upper critical value - simple version
math(Flags, uqbinom(Alpha, N, Pi), New, X)
 => New = Flags,
    X = fn(under("argmin", k), [fn(sub('P', "Bi"), ([('X' >= k)] ; [N, Pi])) =< Alpha]).

% critical value - this may not yet work fully
math(Flags, cbinom(Alpha, N, Pi, Tail, Dist), New, X)
 => New = Flags,
    X = fn(Tail, [fn(sub('P', "Bi"), ([Dist] ; [N, Pi])) =< Alpha]).

math(Flags, tail("upper"), New, X)
 => New = Flags,
    X = under("argmin", k).

math(Flags, tail("lower"), New, X)
 => New = Flags,
    X = under("argmax", k).

math(Flags, tail("upperdens"), New, X)
 => New = Flags,
    X = under("argmin", k > 'N' * pi).

math(Flags, tail("lowerdens"), New, X)
 => New = Flags,
    X = under("argmax", k < 'N' * pi).

math(Flags, dist("upper"), New, X)
 => New = Flags,
    X = ('X' >= k).

math(Flags, dist("lower"), New, X)
 => New = Flags,
    X = ('X' =< k).

math(Flags, dist("density"), New, X)
 => New = Flags,
    X = ('X' = k).

mathml :- mathml(dbinom(k, 'N', pi)).
mathml :- mathml(pbinom(k, 'N', pi)).
mathml :- mathml(upbinom(k, 'N', pi)).
mathml :- mathml(cbinom(alpha, 'N', pi, tail("lower"), dist("lower"))).
mathml :- mathml(cbinom(alpha, 'N', pi, tail("upper"), dist("upper"))).
mathml :- 
    writeln("This is silly, but reflects a frequent mistake by my students."),
    mathml(cbinom(alpha, 'N', pi, tail("lowerdens"), dist("density"))).
mathml :- mathml(cbinom(alpha, 'N', pi, tail("upperdens"), dist("density"))).

%
% Trigonometry
%
math(Flags, sin(Alpha), New, X)
 => New = Flags,
    X = fn("sin", [Alpha]).

math(Flags, cos(Alpha), New, X)
 => New = Flags,
    X = fn("cos", [Alpha]).

math(Flags, tan(Alpha), New, X)
 => New = Flags,
    X = fn("tan", [Alpha]).

%
% Probability
%
math(Flags, pnorm(Z), New, X)
 => New = Flags,
    X = fn('Phi', [Z]).
    
math(Flags, qnorm(P), New, X)
 => New = Flags,
    X = fn('Phi' ^(-1), [P]).

%
% Intervals
%
math(Flags, '...'(L, U), New, X)
 => New = Flags,
    X = xfx(699, '...', floor(L), ceiling(U)).

math(Flags, floor(L), New, X)
 => select_option(round(D), Flags, N, 2),
    New = [floor(D) | N],
    X = L.
 
math(Flags, ceiling(L), New, X)
 => select_option(round(D), Flags, N, 2),
    New = [ceiling(D) | N],
    X = L.
 
%
% R assignment
%
math(Flags, '<-'(L, R), New, X)
 => New = Flags,
    X = xfx(700, ':=', L, R).

%
% Functions like f(x) and f(x; a, b)
%
ml(Flags, fn(Name, (Args ; Params)), M)
 => ml(Flags, Name, F),
    ml(Flags, paren(list(sign(;), [list(sign(,), Args), list(sign(,), Params)])), X),
    M = mrow([F, mo(&(af)), X]).

paren(Flags, fn(_Name, (Args ; Params)), Paren)
 => paren(Flags, paren(list(sign(','), Args)), X),
    paren(Flags, paren(list(sign(','), Params)), Y),
    Paren is max(X, Y).

prec(Flags, fn(_Name, (_Args ; _Params)), Prec)
 => prec(Flags, a * b, Prec).

type(_Flags, fn(_Name, (_Args ; _Params)), Type)
 => Type = paren.

ml(Flags, fn(Name, [Arg]), M),
    member(Name, ["sin", "cos", "tan"]),
    prec(Flags, Arg, P),
    P = 0
 => ml(Flags, Name, F),
    ml(Flags, Arg, X),
    M = mrow([F, mo(&(af)), X]).

ml(Flags, fn(Name, [Arg]), M)
 => ml(Flags, Name, F),
    ml(Flags, paren(list(sign(,), [Arg])), X),
    M = mrow([F, mo(&(af)), X]).

ml(Flags, fn(Name, Args), M)
 => ml(Flags, Name, F),
    ml(Flags, paren(list(sign(,), Args)), X),
    M = mrow([F, mo(&(af)), X]).

paren(Flags, fn(_Name, Args), Paren)
 => paren(Flags, paren(list(sign(','), Args)), Paren).

prec(_Flags, fn(_Name, _Args), Prec)
 => current(Prec, yfx, *).

type(_Flags, fn(_Name, _Args), Type)
 => Type = paren.

%
% Defaults
%
math(Flags, A, New, X)
 => A = X,
    New = Flags.

paren(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => paren(New, M, Den).

paren(_, _, P) =>
    P = 0.

prec(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => prec(New, M, Den).

prec(_, _, P) =>
    P = 0.

type(Flags, A, Type),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => type(New, M, Type).

type(_Flags, A, Type),
    compound(A)
 => Type = compound.

denoting(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => denoting(New, M, Den).

denoting(Flags, Expression, Den),
    compound(Expression)
 => compound_name_arguments(Expression, _, Arguments),
    maplist(denoting(Flags), Arguments, List),
    append(List, Den).

% If everything fails, there is no abbreviation
denoting(_Flags, _, Den)
 => Den = [].

% Precedence
current(Prec, Fix, Op),
    atom(Op)
 => current_op(Prec, Fix, Op).

% Bugs
bugs(Expr, Bugs) :-
    bugs_(Expr, List),
    sort(List, Bugs).

bugs_(instead(bug(Bug), Wrong, _Correct), List)
 => bugs_(Wrong, Bugs),
    List = [Bug | Bugs].

bugs_(omit_left(bug(Bug), Expr), List)
 => Expr =.. [_Op, _L, R],
    bugs_(R, Bugs),
    List = [Bug | Bugs].

bugs_(omit_right(bug(Bug), Expr), List)
 => Expr =.. [_Op, L, _R],
    bugs_(L, Bugs),
    List = [Bug | Bugs].

bugs_(drop_left(bug(Bug), Expr), List)
 => Expr =.. [_Op, _L, R],
    bugs_(R, Bugs),
    List = [Bug | Bugs].

bugs_(drop_right(bug(Bug), Expr), List)
 => Expr =.. [_Op, L, _R],
    bugs_(L, Bugs),
    List = [Bug | Bugs].

bugs_(invent_left(bug(Bug), Expr), List)
 => bugs_(Expr, Bugs),
    List = [Bug | Bugs].

bugs_(invent_right(bug(Bug), Expr), List)
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

colors(Expr, Flags) :-
    bugs(Expr, Bugs),
    findall(C, color(C), Colors),
    findall(color(B, C), (nth0(N, Bugs, B), N10 is N mod 10, nth0(N10, Colors, C)), Flags).
