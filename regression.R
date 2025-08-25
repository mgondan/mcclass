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

# Calculate models once and store them
get.models <- function(d)
{
  correct <- sprintf("%s ~ %s", y, x)
  switched <- sprintf("%s ~ %s", x, y)
  setNames(list(lm(correct, data=d),lm(switched, data=d)),
  c(correct, switched))
}

data <- generate.data(seed = sample(1:10000, 1))

models <- get.models(data)

lm0 <- function(y, x)
{
  sprintf("%s ~ %s", y, x)
}

# For coefficients
extract0 <- function(formula, index)
{ 
  m <- models[[formula]]
  if (index == "intercept") {
    coef(m)[1]
  } else if (index == "predictor") {
    coef(m)[2]
  }
}

# For p-values
extract1 <- function(formula, index)
{ 
  x <- strsplit(formula, " ~ ", fixed = TRUE)[[1]][2]
  m <- models[[formula]]
  if (index == "predictor") {
    summary(m)$coefficients[x, "Pr(>|t|)"]
  } else if (index == "intercept") {
    summary(m)$coefficients["(Intercept)", "Pr(>|t|)"]
  }
}

download <- function(fname) {
  write.csv2(data, fname, row.names=FALSE)
}