	      pi_A <- runif(1, min=0.25, max=0.85)
        pi_B <- runif(1, min=0.25, max=0.85)

        odds <- function(p)
        {
          p / (1 - p)
        }

        prob <- function(odds)
        {
          odds / (1 + odds)
        }
