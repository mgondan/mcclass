        n <- as.integer(round(runif(1, min=50, max=100)))
        N <- rbinom(n, 1, 0.5)
        Id <- 1:n
        B0 <- round(runif(n, min=15, max=40))
        B1 <- round(runif(n, min=-10, max=2))
        AV <- round(B0 + B1 * (1 - N))
        data <- data.frame(Id, N, AV)

        vr <- mean(data[data$N==1, 1])
        s_vr <- sd(data[data$N==1, 1])
        box <- mean(data[data$N==0, 1])
        s_box <- sd(data[data$N==0, 1])
        n_vr <- nrow(subset(data, N==1))
        n_box <- nrow(subset(data, N==0))

        tails <- "two-tailed"
        alpha <- 0.05


tgroups_data = function(fname)
{
    write.csv2(data, fname, row.names=FALSE)
}

