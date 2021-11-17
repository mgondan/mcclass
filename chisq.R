    s_VR <- round(runif(1, min=15, max=40))
    n_VR <- round(s_VR + runif(1, min=5, max=20))
    p_VR <- s_VR / n_VR
    s_Box <- round(s_VR - runif(1, min=-1, max=7))
    n_Box <- round(n_VR + runif(1, min=3, max=8))
    p_Box <-  s_Box / n_Box
