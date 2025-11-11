k  <- sample(6L:14L, size=1)
n  <- sample(8L:14L, size=1)
p0 <- round(runif(1, min=0.2, max=0.8), 2)
k  <- as.integer(pmax(2, pmin(n - 2, n*p0 + sample(-3:3, size=1))))

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


# exactseq loop
repeat {
  n1 <- sample(4:5, 1)
  k1 <- sample(2:(n1-1), 1)
  p0_1 <- round(runif(1, 0.4, 0.6), 2)
  
  exactseq <- p0_1^k1 * (1 - p0_1)^(n1 - k1)
  if (exactseq >= 0.05) break
}

# succrun loop
repeat {
  k2  <- 3L
  n2  <- 5L
  p0_2 <-round(runif(1, 0.4, 0.6), 2)
  
  succrun <- p0_2^k2
  if(succrun >= 0.05) break
}

