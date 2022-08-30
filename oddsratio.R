repeat
{
  pi_A <- runif(1, min=0.25, max=0.85)
  or <- runif(1, min=0.5, max=6)
  if(or > 0.9 & or < 1.1)
    continue

  break
}
