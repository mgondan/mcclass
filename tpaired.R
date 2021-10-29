# Raw data
n     <- as.integer(round(runif(1, min=20, max=45)))
Id    <- 1:n
T0    <- round(runif(n, min=15, max=40))
EOT   <- round(T0 + runif(n, min=-10, max=2))
data  <- data.frame(Id, T0, EOT)

# Summary statistics
mu    <- round(runif(1, min=2, max=5), 1)
t0    <- mean(data$T0)
s_t0  <- sd(data$T0)
eot   <- mean(data$EOT)
s_eot <- sd(data$EOT)

d1    <- mean(data$T0 - data$EOT)
# d is a range of possible values
d     <- list(c(floor(d1*10)/10, ceiling(d1*10)/10))

s1_d  <- sd(data$T0 - data$EOT)
s_d   <- list(c(floor(s1_d*10)/10, ceiling(s1_d*10)/10))

tails <- "two-tailed"
alpha <- 0.05

# Export data for download
tpaired_data = function(fname)
{
    write.csv2(data, fname, row.names=FALSE)
}

