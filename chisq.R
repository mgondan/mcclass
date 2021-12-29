    s_VR <- sample(15:40, size=1)
    n_VR <- round(s_VR + runif(1, min=15, max=30))
    p_VR <- round(s_VR / n_VR, digits=2)

    s_Box <- s_VR - sample(c(1:10), size=1)
    n_Box <- n_VR - sample(1:10, size=1)
    p_Box <- round(s_Box / n_Box, digits=2)


