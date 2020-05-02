% Interaction with the R system
:- module(r, [ r/2, r_init/0 ]).

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

        tratio <- function(t)
        {
            sprintf(ifelse(abs(t) >= 10, "%.1f", "%.2f"), t)
        }
    |}.

