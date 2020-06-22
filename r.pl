% R on demand
:- module(r, [ r_init/0, rod/2 ]).

:- use_module(library(r/r_call)).

% Needed for the web server
r_call:r_console_property(size(25, 80)).

% Use prolog for evaluation
%
% Predefined hooks and custom functions
:- multifile p_hook/2.

p(Expr, Res) :-
    p_hook(Expr, Hook),
    !,
    p(Hook, Res).

% Handle lists
p([], Res) :-
    !,
    Res = [].

p([H | T], Res) :-
    maplist(p, [H | T], Res).

% Switch to R
p(rr(Expr), Res) :-
    !,
    r(rr(Expr), Res).

p(r(Expr), Res) :-
    !,
    r(Expr, Res).

% Stay in prolog
p(p(Expr), Res) :-
    !,
    p(Expr, Res).

p(pp(Expr), Res) :-
    !,
    Res is Expr.

% Evaluate compound
p(Expr, Res) :-
    compound(Expr),
    !,
    compound_name_arguments(Expr, Name, Arguments),
    maplist(p, Arguments, Results),
    compound_name_arguments(New, Name, Results),
    Res is New.

p(Expr, Res) :-
    atom(Expr),
    !,
    Res = Expr.

p(Expr, Res) :-
    string(Expr),
    !,
    Res = Expr.

p(Expr, Res) :-
    Res is Expr.

% Custom functions
p_hook(frac(Num, Den), Res) :-
    p(Num / Den, Res).

p_hook(dfrac(Num, Den), Res) :-
    p(Num / Den, Res).

p_hook(left_landed(_, Expr), Res) :-
    p(Expr, Res).

p_hook(right_landed(_, Expr), Res) :-
    p(Expr, Res).

p_hook(left_elsewhere(_, Expr), Res) :-
    Expr =.. [_, _, R],
    p(R, Res).

p_hook(right_elsewhere(_, Expr), Res) :-
    Expr =.. [_, L, _],
    p(L, Res).

p_hook(instead_of(_, Expr, _), Res) :-
    p(Expr, Res).

p_hook(instead_of(_, Expr, _, _, _), Res) :-
    p(Expr, Res).

p_hook(omit_left(_, Expr), Res) :-
    Expr =.. [_, _, R],
    p(R, Res).

p_hook(omit_right(_, Expr), Res) :-
    Expr =.. [_, L, _],
    p(L, Res).

p_hook(denoting(_, Expr, _), Res) :-
    p(Expr, Res).

p_hook(protect(Expr), Res) :-
    p(Expr, Res).

p_hook(var_pool(Var_A, N_A, Var_B, N_B), Res) :-
    p(frac((N_A - 1) * Var_A + (N_B - 1) * Var_B, N_A + N_B - 2), Res).

p_hook(tratio(T, DF), Res) :-
    r(tratio(p(T), p(DF)), Res).

p_hook(fratio(F), Res) :-
    r(fratio(p(F)), Res).

p_hook(prob(P), Res) :-
    r(prob(p(P)), Res).

p_hook(natural(P), Res) :-
    r(natural(p(P)), Res).

p_hook(qt(P, DF), Res) :-
    r(qt(p(P), p(DF)), Res).

p_hook(uqbinom(Tail, Dist, Alpha, Size, Prob), Res) :-
    r(uqbinom(p(Tail), p(Dist), p(Alpha), p(Size), p(Prob)), Res).

p_hook(tail(Tail), Res) :-
    r(tail(p(Tail)), Res).

p_hook(dist(Dist), Res) :-
    r(dist(p(Dist)), Res).

p_hook(pm(X, PM), [P, M]) :-
    p(X - PM, P),
    p(X + PM, M).

p_hook(confint(CI, D), Res) :-
    r(confint(p(CI), D), Res).

p_hook(anova_f(Model, Effect), Res) :-
    r('`[`'(anova(p(Model)), Effect, "F value"), Res).

p_hook(ancova_ffffff(D, Prim, Cov, Strata, Other, Int, Ex, Main), Res) :-
    r(rr(ancova_ffffff(D, Prim, Cov, Strata, Other, Int, Ex, Main)), Res).

p_hook(choose(N, K), Res) :-
    r(choose(N, K), Res).

% Use R for evaluation
%
% Use R for full expression
r(rr(Expr), Res) :-
    !,
    pl2r(Expr, Fix),
    R <- Fix,
    [Res] = R.

% Ignore
r(r(Expr), Res) :-
    !,
    r(Expr, Res).

% Switch to Prolog
r(p(Expr), Res) :-
    !,
    p(Expr, Res).

% Evaluate compound
r(Expr, Res) :-
    compound(Expr),
    compound_name_arguments(Expr, Name, Arguments),
    maplist(r, Arguments, Results),
    compound_name_arguments(New, Name, Results),
    pl2r(New, Fix),
    R <- Fix,
    [Res] = R.

r(Expr, Res) :-
    atomic(Expr),
    pl2r(Expr, Fix),
    R <- Fix,
    [Res] = R.

% R on demand
rod(Expr, Res) :-
    p(Expr, Res).

% Translate problematic prolog compounds to R
% Multifile hook for extensions
:- multifile pl2r_hook/2.
pl2r(P, R) :-
    pl2r_hook(P, R1),
    !,
    R = R1.

% Prolog lists <-> (flat) R vectors
pl2r([], 'NULL') :-
    !.

pl2r([H | T], R) :-
    !,
    maplist(pl2r, [H | T], List),
    compound_name_arguments(R, c, List).

pl2r(P, R) :-
    atomic(P),
    !, 
    R = P.

pl2r(P, R) :-
    compound(P),
    compound_name_arguments(P, N, PArgs),
    maplist(pl2r, PArgs, RArgs),
    compound_name_arguments(R, N, RArgs).

% Some common functions
r_init :-
    {|r||
        dfrac <- frac <- `/`

        left_landed <- function(err, expr)
        {
            return(expr)
        }

        right_landed <- function(err, expr)
        {
            return(expr)
        }

        left_elsewhere <- omit_left <- function(err, expr)
        {
            eval(substitute(expr)[[3]])
        }

        right_elsewhere <- omit_right <- function(err, expr)
        {
            eval(substitute(expr)[[2]])
        }

        instead_of1 <- function(err, wrong, error, correct, noerror)
        {
            return(wrong)
        }

        denoting <- function(sym, expr, label)
        {
            return(expr)
        }

        tratio <- function(t, df)
        {
            r = sprintf(ifelse(abs(t) >= 10, "%.1f", "%.2f"), t)
            d = sprintf("%g", df)
            sprintf("t(%s) = %s", d, r)
        }

        fratio <- function(f)
        {
            sprintf(ifelse(abs(f) >= 10, "%.1f", "%.2f"), f)
        }

        confint <- function(ci, digits=2)
        {
            mask = sprintf("%%.%if to %%.%if", digits, digits)
            sprintf(mask, ci[1], ci[2])
        }

        prob <- function(p, digits=2)
        {
            mask = sprintf("%%.%if", digits)
            sprintf(mask, p)
        }

        natural <- function(n)
        {
            sprintf("%i", n)
        }

        pm <- function(a, b)
        {
            c(a - b, a + b)
        }

        protect <- identity
    |}.

% Tests
example :-
    rod(p(1 + 1), Res),
    writeln(Res).

example :-
    rod(pp(2 + 2), Res),
    writeln(Res).

example :-
    rod(r(3 + 3), Res),
    writeln(Res).

example :-
    rod(rr(4 + 4), Res),
    writeln(Res).

example :-
    rod(p(5 + r(5 + 5)), Res),
    writeln(Res).

example :-
    rod(r(6 + p(6 + 6)), Res),
    writeln(Res).

example :-
    rod(rr($('t.test'(c(1, 2, 3, 4, 5), mu=0), "p.value")), Res),
    writeln(Res).

