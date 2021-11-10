    odds_A <- runif(1, min=0.33, max=5.66)
    or <- runif(1, min=0.5, max=6)
    odds_B <- odds_A * or
    pi_B <- odds_B/(1 + odds_B)
