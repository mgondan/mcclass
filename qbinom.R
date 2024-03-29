alpha = 0.05
n     = 26L
p0    = 0.6
k     = NA

uqbinom = function(...)
  as.integer(qbinom(..., lower.tail=FALSE) + 1)

lqbinom = function(...)
  as.integer(qbinom(..., lower.tail=TRUE) - 1)

udbinom = function(Alpha, N, Pi)
{
  v = dbinom(round(N*Pi):N, N, Pi)
  N - sum(v <= Alpha) + 1L
}

ldbinom = function(Alpha, N, Pi)
{
  v = dbinom(0L:round(N*Pi), N, Pi)
  sum(v <= Alpha) - 1L
}

cbinom = function(Alpha, N, Pi, Tail, Arg)
{
  if(Tail == "upper" & Arg == "min")
    return(uqbinom(Alpha, N, Pi))

  if(Tail == "lower" & Arg == "max")
    return(lqbinom(Alpha, N, Pi))

  if(Tail == "equal" & Arg == "min")
    return(udbinom(Alpha, N, Pi))

  if(Tail == "equal" & Arg == "max")
    return(ldbinom(Alpha, N, Pi))

  stop("Wrong Tail or Arg")
}

tail = function(Tail, K)
  return(Tail)

arg = function(Arg, K)
  return(Arg)

