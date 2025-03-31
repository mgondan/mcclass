# Interval: 1.562 -> ...(1.56, 1.57)
int <- function(x, digits=2)
{
  mul <- 10^digits
  call('...', floor(x*mul)/mul, ceiling(x*mul)/mul)
}

# Raw data
repeat
{
  n      <- as.integer(round(runif(1, min=20, max=45)))
  Id     <- 1:n
  T0     <- round(runif(n, min=15, max=40))
  EOT    <- round(T0 + runif(n, min=-10, max=2))
  mu     <- round(runif(1, min=2, max=5), 1)

  data   <- data.frame(Id, T0, EOT)
  t0x    <- mean(data$T0)
  t0     <- int(t0x)

  sx_t0  <- sd(data$T0)
  s_t0   <- int(sx_t0)

  eotx   <- mean(data$EOT)
  eot    <- int(eotx)

  sx_eot <- sd(data$EOT)
  s_eot  <- int(sx_eot)

  dx     <- mean(data$T0 - data$EOT)
  d      <- int(dx)

  sx_d   <- sd(data$T0 - data$EOT)
  s_d    <- int(sx_d)

  tratio <- (dx - mu) / (sx_d / sqrt(n))
  if(abs(tratio) < 0.1)
    next
  if(abs(tratio) > 3.0)
    next

  tails  <- "two-tailed"
  alpha  <- 0.05
  break
}

# Export data for download
download <- function(fname)
  write.csv2(data, fname, row.names=FALSE)

tail <- function(lower)
{
  if(lower == "lower")
    return(TRUE)

  if(lower == "upper")
    return(FALSE)

  stop("tail: argument must be 'upper' or 'lower'.")
}
