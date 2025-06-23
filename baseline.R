data = function(seed)
{
  set.seed(seed)
  n <- sample(100:150, size=1)
  p_fem <- 0.3
  sex <- factor(rbinom(n, size=1, prob=p_fem), levels=c(0, 1), labels=c("M", "F"))
  age <- round(runif(n, min=30, max=80))
  inclusion <- rnorm(n, mean=6 - as.numeric(sex) + 0.05*age, sd=2)
  T0 <- round(rnorm(n, mean=inclusion, sd=2), 1)
  d <- data.frame(ID=1:n, sex, age, T0)
  
  # Inclusion criteria
  inclusion <- inclusion[d$T0 > 2 & d$T0 < 10]
  d <- d[d$T0 > 2 & d$T0 < 10, ]
  n <- nrow(d)
  
  # Randomization
  d$Therapy <- factor(rbinom(n, size=1, prob=0.5), levels=c(0, 1), labels=c("Lidcombe", "TAU"))
  
  # Treatment fidelity
  d$fidel <- pmin(100, pmax(20, round(rnorm(n, mean=70, sd=15))))
  
  # EOT depends on therapy and fidelity
  d$EOT = pmax(0, pmin(10, round(digits=1, rnorm(n,
      mean=inclusion-5.1-d$fidel*0.03+1.2*as.numeric(d$Therapy), sd=2))))
  d$FU <- pmax(0, pmin(10, round(digits=1, rnorm(n,
      mean=inclusion-6.3-d$fidel*0.02+1.5*as.numeric(d$Therapy), sd=2))))
  
  d$Therapy <- as.character(d$Therapy)
  return(d)
}

d     <- data(seed=4711)
n     <- nrow(d)
m_T0  <- by(d$T0, d$Therapy, mean)
s_T0  <- by(d$T0, d$Therapy, sd)
m_EOT <- by(d$EOT, d$Therapy, mean)
s_EOT <- by(d$EOT, d$Therapy, sd)
m_FU  <- by(d$FU, d$Therapy, mean)
s_FU  <- by(d$FU, d$Therapy, sd)
alpha <- 0.05
tails <- "two-tailed"

Lidcombe_T0_Mean  <- m_T0["Lidcombe"]
Lidcombe_T0_SD    <- s_T0["Lidcombe"]
Lidcombe_T0       <- sprintf("%.1f (%.1f)", Lidcombe_T0_Mean, Lidcombe_T0_SD)
TAU_T0_Mean       <- m_T0["TAU"]
TAU_T0_SD         <- s_T0["TAU"]
TAU_T0            <- sprintf("%.1f (%.1f)", TAU_T0_Mean, TAU_T0_SD)
Lidcombe_EOT_Mean <- m_EOT["Lidcombe"]
Lidcombe_EOT_SD   <- s_EOT["Lidcombe"]
Lidcombe_EOT      <- sprintf("%.1f (%.1f)", Lidcombe_EOT_Mean, Lidcombe_EOT_SD)
TAU_EOT_Mean      <- m_EOT["TAU"]
TAU_EOT_SD        <- s_EOT["TAU"]
TAU_EOT           <- sprintf("%.1f (%.1f)", TAU_EOT_Mean, TAU_EOT_SD)

# Export data for download
download <- function(fname)
  write.csv2(d, fname, row.names=FALSE)

ancova_f <- function(outcome, cov, strata, other, interaction, exclude, Therapy)
{
  predictors <- paste(c(cov, strata, list(Therapy)), collapse="+")
  formula <- sprintf("%s ~ %s", outcome, predictors)
  m <- lm(formula, data=d)
  anova(m)[Therapy, "F value"]
}

ancova_p <- function(outcome, cov, strata, other, interaction, exclude, Therapy)
{
  predictors <- paste(c(cov, strata, list(Therapy)), collapse="+")
  formula <- sprintf("%s ~ %s", outcome, predictors)
  m <- lm(formula, data=d)
  anova(m)[Therapy, "Pr(>F)"]
}

library(emmeans)
ancova_ci <- function(outcome, cov, strata, other, interaction, exclude, Therapy)
{
  predictors <- paste(c(cov, strata, list(Therapy)), collapse="+")
  formula <- sprintf("%s ~ %s", outcome, predictors)
  m <- lm(formula, data=d)
  emm <- emmeans(m, Therapy, contr="trt.vs.ctrl1")
  ci <- confint(emm)$contrasts
  lower <- ci[1, "lower.CL"]
  upper <- ci[1, "upper.CL"]
  substitute(call("ci", lower, upper), list(lower=lower, upper=upper))
}
