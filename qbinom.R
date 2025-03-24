alpha = 0.05
n     = 26L
p0    = 0.6
k     = NA

uqbinom = function(...)
  as.integer(qbinom(..., lower.tail=FALSE) + 1)

lqbinom = function(...)
  as.integer(qbinom(..., lower.tail=TRUE) - 1)

udbinom = function(Alpha, N, Pi)
{
  v = dbinom(round(N*Pi):N, N, Pi)
  N - sum(v <= Alpha) + 1L
}

ldbinom = function(Alpha, N, Pi)
{
  v = dbinom(0L:round(N*Pi), N, Pi)
  sum(v <= Alpha) - 1L
}

cbinom = function(Alpha, N, Pi, Tail, Arg)
{
  if(Tail == "upper" & Arg == "min")
    return(uqbinom(Alpha, N, Pi))

  if(Tail == "lower" & Arg == "max")
    return(lqbinom(Alpha, N, Pi))

  if(Tail == "equal" & Arg == "min")
    return(udbinom(Alpha, N, Pi))

  if(Tail == "equal" & Arg == "max")
    return(ldbinom(Alpha, N, Pi))

  stop("Wrong Tail or Arg")
}

library(ggplot2)
library(svglite)

rendered_probs <- 0.5
  
k_values <- 0:n

probs <- pbinom(k_values, n, p0, lower.tail = FALSE)

d <- data.frame(k = k_values, Probability = probs)

d <- d[d$Probability < rendered_probs, ]

plt <- ggplot(d, aes(x = k, y = Probability)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = paste("Cumulated Binomial Distribution (N =", n, ", p =", p0, ")"),
       x = "Number of Successes",
       y = "Cumulated Probability") +
  scale_x_continuous(breaks = seq(0, n, by = 1)) + 
  scale_y_continuous(breaks = seq(0, rendered_probs, by = 0.05)) +
  geom_hline(yintercept = alpha, linetype = "dashed", color = "red") + 
  theme(plot.background = element_rect(color = "black", size = 1))

temp_file <- tempfile(fileext = ".svg")
svglite(temp_file, width = 6, height = 4)
print(plt)
dev.off()
svg <- paste(readLines(temp_file), collapse = "\n")
unlink(temp_file)