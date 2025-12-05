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
