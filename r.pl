:- module(r, [r_init/0, r//1]).

:- reexport(library(r/r_call)).
:- set_prolog_flag(float_overflow, infinity).
r_call:r_console_property(size(25, 80)).

% Evaluate R expression and render it as html
r(Expr) -->
    { R <- Expr },
    html(R).

r_init :- 
    {|r||
        dfrac <- frac <- `/`

        var_pool <- function(v_A, n_A, v_B, n_B)
        {
            frac((n_A-1) * v_A + (n_B-1) * v_B, n_A+n_B-2)
        }

        omit_left <- function(expr)
        {
            # use third element of [-, A, B]
            eval(substitute(expr)[[3]])
        }

        omit_right <- function(expr)
        {
            eval(substitute(expr)[[2]])
        }

        instead <- function(x, of)
        {
            return(x)
        }
    |}.

