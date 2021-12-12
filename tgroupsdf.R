    n <- round(runif(1, min=43, max=70))
    n_vr <- round(n * runif(1, min=0.40, max=0.60))
    n_box <- n - n_vr
    vr <- round(runif(1, min=45.0, max=55.0), digits=1)
    s_vr <- round(runif(1, min=10.0, max=14.0), digits=1)
    box <- round(vr + runif(1, min=(-9.0), max=4.0), digits=1)
    s_box <- round(s_vr + runif(1, min=(-2.0), max=6.0), digits=1)

    tails <- "two-tailed"
    alpha <- 0.05
