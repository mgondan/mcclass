alpha = 0.05
n     = 26L
p0    = 0.6

uqbinom = function(...)
{
    qbinom(..., lower.tail=FALSE)
}

lqbinom = function(...)
{
    qbinom(..., lower.tail=TRUE)
}

