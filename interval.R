# Classification of intervals by sign
# Hickey: Figure 1

Mix = function(X) 
{
  X[1] < 0 & X[2] > 0
}

Pos = function(X)
{
  X[1] >= 0 & X[2] > 0
}


Pos0 = function(X) 
{
  X[1] == 0 & X[2] > 0
}

Pos1 = function(X) 
{
  X[1] > 0 & X[2] > 0
}

Neg = function(X) 
{
  X[1] < 0 & X[2] <= 0
}

Neg0 = function(X) 
{
  X[1] < 0 & X[2] == 0
}

Neg1 = function(X) 
{
  X[1] < 0 & X[2] < 0
}

# 

# Addition
# Hickey: Theorem 5

iplus = function(X, Y)
{
  a = X[1]
  b = X[2]
  c = Y[1]
  d = Y[2]
  
  return(list(c(a+c, b+d )))
  
}

# Substraction
# Hickey: Theorem 5

iminus = function(X, Y)
{
  a = X[1]
  b = X[2]
  c = Y[1]
  d = Y[2]
  
  return(list(c(a-d, b-c)))
}

# Multiplication
# Hickey: Theorem 6 

imult = function(X,Y)
{
  a = X[1]
  b = X[2]
  c = Y[1]
  d = Y[2]
  
  list(c(min(a*c, a*d, b*c, b*d), max(a*c, a*d, b*c, b*d)))
  
}

# Division
# Hickey: Figure 4
# Case analysis for functional division of real intervals 

idiv = function(X, Y)
{ 
  a = X[1]
  b = X[2]
  c = Y[1]
  d = Y[2]
  
  if(Neg1(X) & Neg0(Y)) #exception case for N1,N
  {
    return(list(c(b/c, Inf)))
  }
  
  if(Neg1(X) & Neg1(Y)) #general formula for N1,N
  {
    return(list(c(b/c, a/d)))
  }

  if(Neg0(X) & Neg0(Y)) #exception case for N0,N
  {
    return(list(c(0, Inf)))
  }
  
  if(Neg0(X) & Neg1(Y)) #general formula for N0,N
  {
    return(list(c(0, a/d)))
  }
  
  if(Mix(X) & Neg0(Y)) #exception case for M,N
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Neg1(Y)) #general formula for M,N
  {
    return(list(c(b/d, a/d)))
  }
  
  if(Pos0(X) & Neg0(Y)) #exception case for P0,N
  {
    return(list(c(-Inf, 0)))
  }
  
  if(Pos0(X) & Neg1(Y)) #general formula for P0,N
  {
    return(list(c(b/d, 0)))
  }
  
  if(Pos1(X) & Neg0(Y)) #exception case for P1,N
  {
    return(list(c(-Inf, a/c)))
  }
  
  if(Pos1(X) & Neg1(Y)) #general formula for P1,N
  {
    return(list(c(b/d, a/c)))
  }
  
  if(Neg1(X) & Mix(Y)) #general formula for N1,M
  {
    return(list(c(-Inf, b/d), c(b/c, Inf)))
  }
  
  if(Pos1(X) & Mix(Y)) #general formula for P1,M
  {
    return(list(c(-Inf, a/c), c(a/d, Inf)))
  }
  
  if(Neg0(X) & Mix(Y)) #general formula for N0,M
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Mix(Y)) #general formula for M,M
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Pos0(X) & Mix(Y)) #general formula for P0,M
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Neg1(X) & Pos0(Y)) #exception case for N1,P
  {
    return(list(C(-Inf, b/d)))
  }
  
  if(Neg1(X) & Pos1(Y)) #general formula for N1,P
  {
    return(list(c(a/c, b/d)))
  }
  
  if(Neg0(X) & Pos0(Y)) #exception case for N0,P
  {
    return(list(c(-Inf, 0)))
  }
  
  if(Neg0(X) & Pos1(Y)) #general formula for N0,P
  {
    return(list(c(a/c, 0)))
  }
  
  if(Mix(X) & Pos0(Y)) #exception case for M,P
  {
    return(list(c(-Inf, Inf)))
  }
  
  if(Mix(X) & Pos1(Y)) #general formula for M,P
  {
    return(list(c(a/c, b/c)))
  }
  
  if(Pos0(X) & Pos0(Y)) #exception case for P0,P
  {
    return(list(c(0, Inf)))
  }
  
  if(Pos0(X) & Pos1(Y)) #general formular for P0,P
  {
    return(list(c(0, b/c)))
  }
  
  if(Pos1(X) & Pos0(Y) ) #exception case for P1,P
  {
    return(list(c(a/d, Inf)))
  } 
  
  if(Pos1(X) & Pos1(Y)) #general formular for P1,P
  {
    return(list(c(a/d, b/c)))
  }
  
}

# Test i-functions
Pzero = c(0, 1.4)
Nzero = c(-1.4, 0)
Mixed = c(-1.5, 1.5)
Positive = c(1, 1.5)
Negative = c(-2, -1.5)

iplus(Nzero, Negative)
iminus(Negative, Pzero)
imult(Mixed, Negative)

idiv(Mixed, Nzero)
idiv(Positive, Mixed)

# O-function 

outerl = function(X,Y, FUN)
{
  r = list()
  for(i in X)
    for(j in Y)
      r = c(r, FUN(i,j))
  r
}

oplus = function(X, Y)
{
  outerl(X, Y, FUN=`iplus`)
}

ominus = function(X, Y)
{
  outerl(X, Y, FUN=`iminus`)
}

omult = function(X, Y)
{
  outerl(X, Y, FUN=`imult`)
}

odiv = function(X, Y)
{
  outerl(X, Y, FUN=`idiv`)
}


# Test o-functions
X = list(c(0.6, 0.7), c(-0.8, 0.9))
Y = list(c(-2.5, 2.6), c(0.8, 0.9))

A = odiv(X,Y)
B = list(c(1.1, 1.2))
C = omult(A,B)
odiv(C,A)