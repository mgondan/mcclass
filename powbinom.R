alpha = 0.05
n     = 26L
p0    = 0.6
p1    = 0.8
k     = NA

uqbinom <- function(...)
  as.integer(qbinom(..., lower.tail=FALSE) + 1)

lqbinom <- function(...)
  as.integer(qbinom(..., lower.tail=TRUE) - 1)

udbinom <- function(Alpha, N, Pi)
{
  v = dbinom(round(N*Pi):N, N, Pi)
  N - sum(v <= Alpha) + 1L
}

ldbinom <- function(Alpha, N, Pi)
{
  v = dbinom(0L:round(N*Pi), N, Pi)
  sum(v <= Alpha) - 1L
}

