regression_data = function(seed)
{
  set.seed(seed)
  N = sample(100:150, size=1)
  Sleep = round(rnorm(N, mean=5.5, sd=1.5), 1)
  Dep = round(rnorm(N, mean=30 - 1.8 * Sleep, sd=6), 1)
  d = data.frame(ID=1:N, Sleep, Dep)
  
  #Inclusion criteria
  d = d[d$Dep > 15 & d$Dep < 45, ]

  return(d)
}


data        <- regression_data(seed = sample(1:10000, 1))
n           <- nrow(data)
sleep       <- data$Sleep
mean_Sleep  <- mean(data$Sleep)
sd_Sleep    <- sd(data$Sleep)
dep       <- data$Dep
mean_Dep    <- mean(data$Dep)
sd_Dep      <- sd(data$Dep)

# Coefficients

bcoef <- function(Dep, Sleep)
{

    m <- lm(Dep ~ Sleep, data=data)
    coef(m)[2]

}


intercept <- function(DV, IV)
{

    m <- lm(DV ~ IV, data=data)

    coef(m)[1]

}

# Correlation
#cor_value   <- cor(data$Sleep, data$Dep)

# p-value
#p_value     <- summary(lm_model)$coefficients["sleep", "Pr(>|t|)"]

download <- function(fname) {
  write.csv2(data, fname, row.names=FALSE)
}