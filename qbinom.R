alpha = 0.05
n     = 26L
p0    = 0.6

uqbinom = function(...)
  as.integer(qbinom(..., lower.tail=FALSE) + 1)

lqbinom = function(...)
  as.integer(qbinom(..., lower.tail=TRUE) - 1)

cbinom = function(Alpha, N, Pi, Tail, Arg)
{
  if(Tail == "upper")
    return(uqbinom(Alpha, N, Pi))

  if(Tail == "lower")
    return(lqbinom(Alpha, N, Pi))

  stop("Wrong tail")
}

tail = arg = identity

