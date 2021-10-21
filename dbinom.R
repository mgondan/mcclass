        k  <- 14L
        n  <- 26L
        p0 <- 0.6

        bernoulli <- function(k, n, p0)
        {
          successes(k, p0) * failures(n - k, 1 - p0)
        }

        successes <- function(k, p0)
        {
          p0^k
        }

        # this may change
        failures <- function(nk, q0)
        {
          q0^nk
        }

