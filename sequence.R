
# exactseq/succrun/failrun loop
repeat {
  n <- sample(4:6, 1)
  k <- sample(2:(n-1), 1)
  p0 <- sample(c(seq(0.40, 0.49, 0.01), seq(0.51, 0.80, 0.01)), 1)
  
  exactseq <- p0^k * (1 - p0)^(n - k)
  if (exactseq >= 0.05) 
    break
}

# partseq loop
repeat {
  ks <- sample(1:(n-1), 1)
  kf <- sample(1:(n-1), 1)
  
  if (ks + kf < n && (ks + kf) > 1 && ks != kf) {
    partseq <- p0^ks * (1 - p0)^kf
    if (partseq >= 0.05)
      break
  }
}

