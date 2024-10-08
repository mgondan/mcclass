# Raw data
n     <- as.integer(round(runif(1, min=20, max=45)))
Id    <- 1:n
T0    <- round(runif(n, min=15, max=40))
EOT   <- round(T0 + runif(n, min=-10, max=2))
data  <- data.frame(Id, T0, EOT)

# Interval: 1.562 -> ...(1.56, 1.57)
int <- function(x, digits=2)
{
  mul <- 10^digits
  call('...', floor(x*mul)/mul, ceiling(x*mul)/mul)
}

# Summary statistics
mu     <- round(runif(1, min=2, max=5), 1)

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

tails  <- "two-tailed"
alpha  <- 0.05

# Export data for download
download <- function(fname)
  write.csv2(data, fname, row.names=FALSE)
