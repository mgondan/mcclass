# Classification of intervals by sign (Hickey, Figure 1)
Mix = function(X)
{
  X[1] < 0 & max(X) > 0
}

Pos = function(X)
{
  X[1] >= 0 & max(X) > 0
}

Pos0 = function(X)
{
  X[1] == 0 & max(X) > 0
}

Pos1 = function(X)
{
  X[1] > 0
}

Neg = function(X)
{
  X[1] < 0 & max(X) <= 0
}

Neg0 = function(X)
{
  X[1] < 0 & max(X) == 0
}

Neg1 = function(X)
{
  max(X) < 0
}

# Typical R functions
i_sqrt = function(X)
{
  l = X[1]
  u = max(X)
  
  list(c(sqrt(l), sqrt(u)))
}

i_dbinom = function(x, size, prob, log=FALSE)
{
  if(max(prob) < x/size)
  {
    l_prob = prob[1]
    u_prob = max(prob)
    return(list(c(dbinom(x, size, l_prob, log), dbinom(x, size, u_prob, log))))
  }
  
  if(prob[1] > x/size)
  {
    l_prob = max(prob)
    u_prob = prob[1]
    return(list(c(dbinom(x, size, l_prob, log), dbinom(x, size, u_prob, log))))
  }
  
  # mixed case
  r = dbinom(x, size, c(l_prob, x/size, u_prob), log)
  list(c(min(r), max(r))) 
}

i_qbinom = function(x, size, prob, lower.tail)
{
    r = qbinom(x, size, prob, lower.tail)
    list(c(min(r), max(r)))
}

i_pnorm = function(z)
{
  l = z[1]
  u = max(z)
  
  list(c(pnorm(l), pnorm(u)))
}

# Hickey: Theorem 5
i_plus = function(X, Y)
{
  a = X[1]
  b = max(X)
  c = Y[1]
  d = max(Y)
  
  list(c(a + c, b + d))
}

# Positive sign
i_pos = function(X)
{
  a = X[1]
  b = max(X)
  
  list(c(a, b))
}

# Hickey: Theorem 5
i_minus = function(X, Y)
{
  a = X[1]
  b = max(X)
  c = Y[1]
  d = max(Y)
  
  list(c(a - d, b - c))
}

# Negation
i_neg = function(X)
{
  a = X[1]
  b = max(X)
  
  list(c(-b, -a))
}

# Hickey: Theorem 6
i_mult = function(X, Y)
{
  a = X[1]
  b = max(X)
  c = Y[1]
  d = max(Y)
  
  cand = c(a * c, a * d, b * c, b * d)
  list(c(min(cand), max(cand)))
}

# Hickey: Figure 4
i_div = function(X, Y)
{
  a = X[1]
  b = max(X)
  c = Y[1]
  d = max(Y)
  
  if(Neg1(X) & Neg0(Y)) # special case of N1 / N
  {
    return(list(c(b / c, Inf)))
  }
  
  if(Neg1(X) & Neg1(Y))
  {
    return(list(c(b / c, a / d)))
  }
  
  if(Neg0(X) & Neg0(Y)) # special case
  {
    return(list(c(0, Inf)))
  }
  
  if(Neg0(X) & Neg1(Y))
  {
    return(list(c(0, a / d)))
  }
  
  if(Mix(X) & Neg0(Y)) # special case
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Neg1(Y))
  {
    return(list(c(b / d, a / d)))
  }
  
  if(Pos0(X) & Neg0(Y)) # special case
  {
    return(list(c(-Inf, 0)))
  }
  
  if(Pos0(X) & Neg1(Y))
  {
    return(list(c(b / d, 0)))
  }
  
  if(Pos1(X) & Neg0(Y)) # special case
  {
    return(list(c(-Inf, a / c)))
  }
  
  if(Pos1(X) & Neg1(Y))
  {
    return(list(c(b / d, a / c)))
  }
  
  # two non-overlapping intervals
  if(Neg1(X) & Mix(Y))
  {
    return(list(c(-Inf, b / d), c(b / c, Inf)))
  }
  
  if(Pos1(X) & Mix(Y))
  {
    return(list(c(-Inf, a / c), c(a / d, Inf)))
  }
  
  if(Neg0(X) & Mix(Y))
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Mix(Y))
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Pos0(X) & Mix(Y))
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Neg1(X) & Pos0(Y)) # special case
  {
    return(list(C(-Inf, b / d)))
  }
  
  if(Neg1(X) & Pos1(Y))
  {
    return(list(c(a / c, b / d)))
  }
  
  if(Neg0(X) & Pos0(Y)) # special case
  {
    return(list(c(-Inf, 0)))
  }
  
  if(Neg0(X) & Pos1(Y))
  {
    return(list(c(a / c, 0)))
  }
  
  if(Mix(X) & Pos0(Y)) # special case
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Pos1(Y))
  {
    return(list(c(a / c, b / c)))
  }
  
  if(Pos0(X) & Pos0(Y)) # special case
  {
    return(list(c(0, Inf)))
  }
  
  if(Pos0(X) & Pos1(Y))
  {
    return(list(c(0, b / c)))
  }
  
  if(Pos1(X) & Pos0(Y)) # special case
  {
    return(list(c(a / d, Inf)))
  }
  
  if(Pos1(X) & Pos1(Y))
  {
    return(list(c(a / d, b / c)))
  }
}

i_pow = function(X, Y)
{
  # This works if the exponent is a single integer
  pw = X^Y
  list(c(min(pw), max(pw)))
}

# Combine everything with everything
outer1 = function(X, FUN, ...)
{
  r = list()
  for(i in X)
    r = c(r, FUN(i, ...))
  r
}

outer2 = function(X, Y, FUN, ...)
{
  r = list()
  for(i in X)
    for(j in Y)
      r = c(r, FUN(i, j, ...))
  r
}

o_sqrt = function(X)
{
  outer1(X, FUN=i_sqrt)
}

o_dbinom = function(x, size, prob, log=FALSE)
{
  outer1(X=prob, FUN=i_dbinom, x=x, size=size, log=log)
}

o_qbinom = function(x, size, prob, lower.tail=TRUE)
{
  outer1(X=prob, FUN=i_qbinom, x=x, size=size, lower.tail=lower.tail)
}

o_pnorm = function(z)
{
  outer1(z, FUN=i_pnorm)
}

o_plus = function(X, Y)
{
  outer2(X, Y, FUN=i_plus)
}

o_pos = function(X)
{
  outer1(X, FUN=i_pos)
}

o_minus = function(X, Y)
{
  outer2(X, Y, FUN=i_minus)
}

o_neg = function(X)
{
  outer1(X, FUN=i_neg)
}

o_mult = function(X, Y)
{
  outer2(X, Y, FUN=i_mult)
}

o_div = function(X, Y)
{
  outer2(X, Y, FUN=i_div)
}

o_pow = function(X, Y)
{
  outer1(X=X, FUN=i_pow, Y=Y)
}

i_name = function(X)
{
  l = X[1]
  u = max(X)
  
  list(c(l, u))
}

o_name = function(X)
{
  outer1(X, FUN=i_name)
}

i_numeric = function(X)
{
  l = X[1]
  u = max(X)
  
  list(c(l, u))
}

o_numeric = function(X)
{
  outer1(X, FUN=i_numeric)
}

# Substitution of o-functions in int(expr)
int = function(expr)
{
  if(is.name(substitute(expr)))
    return(o_name(expr))
  
  if(is.numeric(substitute(expr)))
    return(o_numeric(expr))
  
  L = as.list(substitute(expr))
  L1 = as.character(L[[1]])
  
  if(L1 == '+' & length(L) == 2)
  {
    L[[1]] = quote(o_pos)
    L[[2]] = call("int", L[[2]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == '+')
  {
    L[[1]] = quote(o_plus)
    L[[2]] = call("int", L[[2]])
    L[[3]] = call("int", L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == '-' & length(L) == 2)
  {
    L[[1]] = quote(o_neg)
    L[[2]] = call("int", L[[2]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == '-')
  {
    L[[1]] = quote(o_minus)
    L[[2]] = call("int", L[[2]])
    L[[3]] = call("int", L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == '*')
  {
    L[[1]] = quote(o_mult)
    L[[2]] = call("int", L[[2]])
    L[[3]] = call("int", L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 %in% c('/', 'frac', 'dfrac'))
  {
    L[[1]] = quote(o_div)
    L[[2]] = call("int", L[[2]])
    L[[3]] = call("int", L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 %in% c('^', '**'))
  {
    L[[1]] = quote(o_pow)
    L[[2]] = call("int", L[[2]])
    #    L[[3]] = call("int", L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == '(')
  {
    C = call('int', L[[2]])
    return(eval(as.call(C, envir=parent.frame())))
  }
  
  if(L1 == '{')
  {
    if(length(L) > 2)
      for(i in 2:(length(L)-1))
        eval(call('int', L[[i]]), envir=parent.frame())
    
    return(eval(call('int', L[[length(L)]]), envir=parent.frame()))
  }
  
  if(L1 %in% c('<-', '='))
  {
    L[[3]] = call('int', L[[3]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == ';')
  {
    e = parent.frame(4)
    
    if(length(L) > 2)
      for(i in 2:(length(L)-1))
        eval(call('int', L[[i]]), envir=e)
    
    return(eval(call('int', L[[length(L)]]), envir=e))
  }
  
  if(L1 == 'sqrt')
  {
    L[[1]] = quote(o_sqrt)
    L[[2]] = call('int', L[[2]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == 'dbinom')
  {
    L[[1]] = quote(o_dbinom)
    L$prob = call('int', L$prob)
    return(eval(as.call(L), envir=parent.frame()))
  }
 
  if(L1 == 'qbinom')
  {
    L[[1]] = quote(o_qbinom)
    L$prob = call('int', L$prob)
    return(eval(as.call(L), envir=parent.frame()))
  }

  if(L1 == 'pnorm')
  {
    L[[1]] = quote(o_pnorm)
    L[[2]] = call('int', L[[2]])
    return(eval(as.call(L), envir=parent.frame()))
  }
  
  if(L1 == 'instead')
  {
    C = call('int', L[[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'omit_left')
  {
    C = call('int', L[[3]][[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'omit_right')
  {
    C = call('int', L[[3]][[2]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'drop_left')
  {
    C = call('int', L[[3]][[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'drop_right')
  {
    C = call('int', L[[3]][[2]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'invent_left')
  {
    C = call('int', L[[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'invent_right')
  {
    C = call('int', L[[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  if(L1 == 'color')
  {
    C = call('int', L[[3]])
    return(eval(C, envir=parent.frame()))
  }
  
  eval(expr)
}

# default_session = new.env()
#
# with(default_session,
# {
#   d = list(c(3.3, 3.4), c(3.3, 3.4))
#   mu = list(2.5)
#   s = list(c(1.9, 1.9))
#   n = list(c(20, 20))
# })
#
# with(default_session, int(dfrac(d - mu, s / sqrt(n))))
