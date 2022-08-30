odds <- function(p)
{
  p / (1 - p)
}

repeat
{
  pi_A <- runif(1, min=0.25, max=0.85)
  pi_B <- runif(1, min=0.25, max=0.85)

  odds_A <- odds(pi_A)
  odds_B <- odds(pi_B)
  or <- odds_B / odds_A  
  if(or > 0.9 & or < 1.1)
    continue

  break
}
