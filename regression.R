x <- "Sleep"
y <- "Depression"
n = sample(100:150, size=1)

generate.data = function(seed)
{
  set.seed(seed)
  sleep = round(rnorm(n, mean=5.5, sd=1.5), 1)
  dep = round(rnorm(n, mean=30 - 1.8 * sleep + rnorm(n, mean=1, sd=8), sd=6), 1)
  d <- setNames(data.frame(sleep, dep), c(x, y))
  
  #Inclusion criteria
  d = d[d[[y]] > 0 & d[[y]] < 42, ]
  return(d)
}

get.models <- function(d)
{
  correct <- sprintf("%s ~ %s", y, x)
  switched <- sprintf("%s ~ %s", x, y)
  setNames(list(lm(correct, data=d),lm(switched, d=data)),
  c(correct, switched))
}

data <- generate.data(seed = sample(1:10000, 1))

models <- get.models(data)

# Extract value from model 
lm0 <- function(y, x, index)
{
  formula <- sprintf("%s ~ %s", y, x)
  m <- models[[formula]]
  
  if (index == "intercept") {
    return(coef(m)[1])
  }
  if (index == "coef") {
    return(coef(m)[2])
  }
  if (index == "pval:coef") {
    return( summary(m)$coefficients[x, "Pr(>|t|)"])
  }
  if (index == "pval:intercept") {
    return( summary(m)$coefficients["(Intercept)", "Pr(>|t|)"])
  }
}

download <- function(fname) {
  write.csv2(data, fname, row.names=FALSE)
}