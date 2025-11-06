# Raw data
n     <- as.integer(round(runif(1, min=30, max=50)))
Id    <- 1:n
T0    <- round(runif(n, min=76, max=83))
EOT   <- round(T0 + runif(n, min=-1, max=6))
EOT   <-ifelse(EOT < 0, 1, EOT)
data  <- data.frame(Id, T0, EOT)

# Summary statistics
mu    <- round(runif(1, min=2, max=3), 1)
t0    <- mean(data$T0)
s_t0  <- sd(data$T0)
eot   <- mean(data$EOT)
s_eot <- sd(data$EOT)

d1    <- mean(data$EOT - data$T0)
# d is a range of possible values
d     <- call('...', floor(d1*10)/10, ceiling(d1*10)/10)

s1_d  <- sd(data$T0 - data$EOT)
s_d   <- call('...', floor(s1_d*10)/10, ceiling(s1_d*10)/10)

tails <- "one-tailed"
alpha <- 0.05

# Export data for download
download <- function(fname)
{
    write.csv2(data, fname, row.names=FALSE)
}

