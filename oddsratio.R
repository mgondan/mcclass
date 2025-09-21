odds <- function(p)
{
  p / (1 - p)
}

#Task 1 oddsratio
repeat
{
  pi_A <- runif(1, min=0.25, max=0.85)
  pi_B <- runif(1, min=0.25, max=0.85)
  odds_ratio <- odds(pi_B) / odds(pi_A)
  if(odds_ratio > 0.9 && odds_ratio < 1.1)
      next

  break
}

# Task 2 success probability of therapy B
repeat
{
  or <- runif(1, min=0.5, max=6)
  if(or > 0.9 && or < 1.1)
    next

  break
}
