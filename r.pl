% Interaction with the R system
:- module(r, [ sur/1, r/2, r_init/0 ]).

:- use_module(library(r/r_call)).

% Needed for the web server
r_call:r_console_property(size(25, 80)).

% Return a single number
r(Expr, Num) :-
    Res <- Expr,
    [Num] = Res.

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

        instead_of <- function(err, wrong, error, correct, noerror)
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

        pm <- function(a, b)
        {
            c(a - b, a + b)
        }
    |}.

:- discontiguous pl2r/2.
:- discontiguous r2pl/2.
:- discontiguous example/0.

% Evaluate R expression after cleaning
sur(Result <- Expr) :-
    atom(Result),
    !, 
    pl2r(Expr, R),
    Result <- R.

% Evalulate R expression and translate back to Prolog
sur(Prolog <- Expr) :-
    pl2r(Expr, R),
    Result <- R,
    r2pl(Result, [Prolog]).

% Multifile hook for extensions
:- multifile pl2r_hook/2.
pl2r(P, R) :-
    pl2r_hook(P, R),
    !.

:- multifile r2pl_hook/2.
r2pl(R, P) :-
    r2pl_hook(R, P),
    !.

% Prolog lists <-> (flat) R vectors
pl2r([], 'NULL') :- 
    !.

pl2r([H | T], R) :-
    !, maplist(pl2r, [H | T], List),
    compound_name_arguments(R, c, List).

example :-
    pl2r([]).

example :-
    pl2r([1]).

example :-
    pl2r([1, 2]).

example :-
    pl2r([1, 2, 3]).

example :-
    pl2r([[1, 2], [3, 4]]).

% Atomic stuff
pl2r(P, R) :-
    atomic(P),
    !, R = P.

r2pl(R, P) :-
    atomic(R),
    !, P = R.

example :-
    pl2r(1).

example :-
    pl2r(1.5).

example :-
    a <- 1,
    pl2r(a).

example :-
    pl2r("A").

% General compounds
pl2r(P, R) :-
    compound(P),
    compound_name_arguments(P, N, PArgs),
    maplist(pl2r, PArgs, RArgs),
    compound_name_arguments(R, N, RArgs).

r2pl(R, P) :-
    compound(R),
    compound_name_arguments(R, N, RArgs),
    maplist(r2pl, RArgs, PArgs),
    compound_name_arguments(P, N, PArgs).

example :-
    pl2r(sin(pi/2)).

% Bidirectional
pl_r(P, R) :-
    ground(P),
    pl2r(P, R).

pl_r(P, R) :-
    ground(R),
    r2pl(R, P).

% Examples
pl2r(P) :-
    format("P: ~k~n", [P]),
    ( pl_r(P, R)
      -> format("= R: ~k~n", [R]),
         ( RR <- R
           -> format("= RR: ~k~n", [RR]),
              ( pl_r(PP, RR)
                -> format("= PP: ~k~n", [PP])
                 ; writeln("failed.")
              )
            ; writeln("failed.")
         )
       ; writeln("failed.")
    ).

