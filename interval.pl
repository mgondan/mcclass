% Interval arithmetics in R
:- module(interval, [r_int/2]).

:- use_module(r).

r_int(Expr, Res) :-
    Res <- int(Expr).

test :-
    r_init,
    {|r||
        # Classification of intervals by sign (Hickey, Figure 1)
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

        # Typical R functions
        i_sqrt = function(X)
        {
            l = X[1]
            u = X[2]
  
            list(c(sqrt(l), sqrt(u)))
        }

        i_dbinom = function(x, size, prob, log=FALSE)
        {
            l_prob = prob[1]
            u_prob = prob[2]
  
            list(c(dbinom(x, size, l_prob, log), dbinom(x, size, u_prob, log)))
        }

        # Hickey: Theorem 5
        i_plus = function(X, Y)
        {
            a = X[1]
            b = X[2]
            c = Y[1]
            d = Y[2]
  
            list(c(a + c, b + d))
        }

        # Positive sign
        i_pos = function(X)
        {
            a = X[1]
            b = X[2]
  
            list(c(a, b))
        }

        # Hickey: Theorem 5
        i_minus = function(X, Y)
        {
            a = X[1]
            b = X[2]
            c = Y[1]
            d = Y[2]
  
            list(c(a - d, b - c))
        }

        # Negation
        i_neg = function(X)
        {
            a = X[1]
            b = X[2]
  
            list(c(-b, -a))
        }

        # Hickey: Theorem 6
        i_mult = function(X, Y)
        {
            a = X[1]
            b = X[2]
            c = Y[1]
            d = Y[2]
  
            candidates = c(a * c, a * d, b * c, b * d)
            list(c(min(candidates), max(candidates)))
        }

        # Hickey: Figure 4
        i_div = function(X, Y)
        {
            a = X[1]
            b = X[2]
            c = Y[1]
            d = Y[2]
  
            if(Neg1(X) & Neg0(Y)) # special case of N1 / N
            {
                return(list(c(b / c, Inf)))
            }
  
            if(Neg1(X) & Neg1(Y))
            {
                return(list(c(b / c, a / d)))
            }
  
            if(Neg0(X) & Neg0(Y)) # special case
            {
                return(list(c(0, Inf)))
            }
  
            if(Neg0(X) & Neg1(Y))
            {
                return(list(c(0, a / d)))
            }
  
            if(Mix(X) & Neg0(Y)) # special case
            {
                return(list(c(-Inf, Inf)))
            }
  
            if(Mix(X) & Neg1(Y))
            {
                return(list(c(b / d, a / d)))
            }
  
            if(Pos0(X) & Neg0(Y)) # special case
            {
                return(list(c(-Inf, 0)))
            }
  
            if(Pos0(X) & Neg1(Y))
            {
                return(list(c(b / d, 0)))
            }
  
            if(Pos1(X) & Neg0(Y)) # special case
            {
                return(list(c(-Inf, a / c)))
            }
  
            if(Pos1(X) & Neg1(Y))
            {
                return(list(c(b / d, a / c)))
            }
  
            # Result can be in two non-overlapping intervals
            if(Neg1(X) & Mix(Y))
            {
                return(list(c(-Inf, b / d), c(b / c, Inf)))
            }
  
            if(Pos1(X) & Mix(Y))
            {
                return(list(c(-Inf, a / c), c(a / d, Inf)))
            }
  
            if(Neg0(X) & Mix(Y))
            {
                return(list(c(-Inf, Inf)))
            }
  
            if(Mix(X) & Mix(Y))
            {
                return(list(c(-Inf, Inf)))
            }
  
            if(Pos0(X) & Mix(Y))
            {
                return(list(c(-Inf, Inf)))
            }
  
            if(Neg1(X) & Pos0(Y)) # special case
            {
                return(list(C(-Inf, b / d)))
            }
  
            if(Neg1(X) & Pos1(Y))
            {
                return(list(c(a / c, b / d)))
            }
  
            if(Neg0(X) & Pos0(Y)) # special case
            {
                return(list(c(-Inf, 0)))
            }
  
            if(Neg0(X) & Pos1(Y))
            {
                return(list(c(a / c, 0)))
            }
  
            if(Mix(X) & Pos0(Y)) # special case
            {
                return(list(c(-Inf, Inf)))
            }
  
            if(Mix(X) & Pos1(Y))
            {
                return(list(c(a / c, b / c)))
            }
  
            if(Pos0(X) & Pos0(Y)) # special case
            {
                return(list(c(0, Inf)))
            }
  
            if(Pos0(X) & Pos1(Y))
            {
                return(list(c(0, b / c)))
            }
  
            if(Pos1(X) & Pos0(Y) ) # special case
            {
                return(list(c(a / d, Inf)))
            }
  
            if(Pos1(X) & Pos1(Y))
            {
                return(list(c(a / d, b / c)))
            }
        }

        # Combine everything with everything
        outer1 = function(X, FUN, ...)
        {
            r = list()
            for(i in X)
                r = c(r, FUN(i, ...))
            r
        }

        outer2 = function(X, Y, FUN, ...)
        {
            r = list()
            for(i in X)
                for(j in Y)
                   r = c(r, FUN(i, j, ...))
            r
        }

        o_sqrt = function(X)
        {
            outer1(X, FUN=i_sqrt)
        }

        o_dbinom = function(x, size, prob, log=FALSE)
        {
            outer1(X=prob, FUN=i_dbinom, x=x, size=size, log=log)
        }

        o_plus = function(X, Y)
        {
            outer2(X, Y, FUN=i_plus)
        }

        o_pos = function(X)
        {
            outer1(X, FUN=i_pos)
        }

        o_minus = function(X, Y)
        {
            outer2(X, Y, FUN=i_minus)
        }

        o_neg = function(X)
        {
            outer1(X, FUN=i_neg)
        }

        o_mult = function(X, Y)
        {
            outer2(X, Y, FUN=i_mult)
        }

        o_div = function(X, Y)
        {
            outer2(X, Y, FUN=i_div)
        }

        # Substitution of o-functions in int(expr)
        int = function(expr)
        {
            L = as.list(substitute(expr))
            L1 = as.character(L[[1]])
  
            if(L1 == '+' & length(L) == 2)
            {
                L[[1]] = quote(o_pos)
                L[[2]] = call("int", L[[2]])
                return(eval(as.call(L)))
            }
  
            if(L1 == '+')
            {
                L[[1]] = quote(o_plus)
                L[[2]] = call("int", L[[2]])
                L[[3]] = call("int", L[[3]])
                return(eval(as.call(L)))
            }
  
            if(L1 == '-' & length(L) == 2)
            {
                L[[1]] = quote(o_neg)
                L[[2]] = call("int", L[[2]])
                return(eval(as.call(L)))
            }
  
            if(L1 == '-')
            {
                L[[1]] = quote(o_minus)
                L[[2]] = call("int", L[[2]])
                L[[3]] = call("int", L[[3]])
                return(eval(as.call(L)))
            }
  
            if(L1 == '*')
            {
                L[[1]] = quote(o_mult)
                L[[2]] = call("int", L[[2]])
                L[[3]] = call("int", L[[3]])
                return(eval(as.call(L)))
            }
  
            if(L1 %in% c('/', 'frac', 'dfrac'))
            {
                L[[1]] = quote(o_div)
                L[[2]] = call("int", L[[2]])
                L[[3]] = call("int", L[[3]])
                return(eval(as.call(L)))
            }
  
            if(L1 == '(')
            {
                C = call("int", L[[2]])
                return(eval(C))
            }
  
            if(L1 == 'sqrt')
            {
                L[[1]] = quote(o_sqrt)
                L[[2]] = call("int", L[[2]])
                return(eval(as.call(L)))
            }
  
            if(L1 == 'dbinom')
            {
                L[[1]] = quote(o_dbinom)
                L$prob = call("int", L$prob)
                return(eval(as.call(L)))
            }
  
            return(expr)
        }
    |}.

test :-
    d <- list(c(3.3, 3.4)),
    mu <- list(c(2.5, 2.5)),
    s <- list(c(1.9, 1.9)),
    n <- list(c(20, 20)),
    r_int(dfrac(d - mu, s / sqrt(n)), Res),
    writeln(Res).
