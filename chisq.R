# Interval: 1.562 -> ...(1.56, 1.57)
int <- function(x, digits=2)
{
  mul <- 10^digits
  call('...', floor(x * mul) / mul, ceiling(x * mul) / mul)
}

s_VR   <- sample(15:40, size=1)
n_VR   <- s_VR + sample(15:30, size=1)
p_VRx  <- s_VR / n_VR
p_VR   <- int(p_VRx)

s_Box  <- s_VR - sample(1:10, size=1)
n_Box  <- n_VR - sample(1:10, size=1)
p_Boxx <- s_Box / n_Box
p_Box  <- int(p_Boxx)
