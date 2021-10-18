        dfrac <- frac <- `/`

        var_pool <- function(v_A, n_A, v_B, n_B)
        {
            frac((n_A-1) * v_A + (n_B-1) * v_B, n_A+n_B-2)
        }

        omit_left <- function(bug, expr)
        {
            # use third element of [-, A, B]
            eval(substitute(expr)[[3]])
        }

        omit_right <- function(bug, expr)
        {
            eval(substitute(expr)[[2]])
        }

        instead <- function(bug, inst, of)
        {
            return(inst)
        }

        abbrev <- function(s, expr, text)
        {
            return(expr)
        }

        color <- function(col, expr)
        {
            return(expr)
        }
