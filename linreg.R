linreg_data = function(seed)
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


data        <- linreg_data(seed = sample(1:10000, 1))
n           <- nrow(data)
mean_Sleep  <- mean(data$Sleep)
sd_Sleep    <- sd(data$Sleep)
mean_Dep    <- mean(data$Dep)
sd_Dep      <- sd(data$Dep)

# Linear regression
lm_model    <- lm(Dep ~ Sleep, data=data)

# Coefficients
b_coef      <- coef(lm_model)["Sleep"]
intercept   <- coef(lm_model)["(Intercept)"]

# Correlation
cor_value   <- cor(data$Sleep, data$Dep)

# p-value
p_value     <- summary(lm_model)$coefficients["Sleep", "Pr(>|t|)"]

download <- function(fname) {
  write.csv2(data, fname, row.names=FALSE)
}