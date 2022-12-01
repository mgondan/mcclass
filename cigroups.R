n <- as.integer(round(runif(1, min=50, max=100)))
N <- rbinom(n, 1, 0.5)
Id <- 1:n
B0 <- round(runif(n, min=15, max=40))
B1 <- round(runif(n, min=-10, max=2))
AV <- round(B0 + B1 * (1 - N))
data <- data.frame(Id, N, AV)

mc <- mean(data[data$N==1, 1])
s_mc <- sd(data[data$N==1, 1])
rc <- mean(data[data$N==0, 1])
s_rc <- sd(data[data$N==0, 1])
n_mc <- nrow(subset(data, N==1))
n_rc <- nrow(subset(data, N==0))

tails <- "two-tailed"
alpha <- 0.05

download <- function(fname)
{
    write.csv2(data, fname, row.names=FALSE)
}

