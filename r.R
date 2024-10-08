dfrac <- frac <- `/`

var_pool <- function(v_A, n_A, v_B, n_B)
{
  frac((n_A - 1) * v_A + (n_B - 1) * v_B, n_A + n_B - 2)
}

omit_left <- drop_left <- function(bug, expr)
{
   # use third element of [-, A, B]
   eval(substitute(expr)[[3]])
}

omit_right <- drop_right <- function(bug, expr)
{
  eval(substitute(expr)[[2]])
}

add_left <- function(bug, expr)
{
  return(expr)
}

add_right <- function(bug, expr)
{
  return(expr)
}
        
instead <- function(bug, inst, of)
{
  return(inst)
}

denote <- function(s, expr, text)
{
  return(expr)
}

color <- function(col, expr)
{
   return(expr)
}

tratio = chi2ratio = identity
