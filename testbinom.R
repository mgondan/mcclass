repeat
{
  alpha <- 0.05
  n     <- sample(20:30, 1)
  p0    <- sample(c(0.4,0.5,0.6), 1)
  p1    <- p0 + sample(c(0.1,0.2,0.3), 1)
  k     <- as.integer(round(n * p1 - sample(2:5, 1)))

  # trap for the binomial density
  if(dbinom(qbinom(alpha, n, p0, lower.tail=FALSE), n, p0) > alpha)
    next
  if(dbinom(qbinom(alpha, n, p0), n, p0) > alpha)
    next

  break
}

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

