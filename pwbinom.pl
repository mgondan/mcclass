% Power of binomial test 
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

mathml:math_hook(Flags, pi_0, Flags, sub(pi, 0)).
mathml:math_hook(Flags, pi_1, Flags, sub(pi, 1)).

r:pl_hook(pi_0, r(pi_0)).
r:pl_hook(pi_1, r(pi_1)).
r:pl_hook(alpha, r(alpha)).
r:pl_hook('N', r('N')).

%    
% Binomial density
%
:- multifile item/1.
item(pwbinom: binom_power(alpha, 'N', pi_0, pi_1)).

:- multifile intermediate/1.
intermediate(pwbinom: binom_power/4).

:- multifile item//2.
item(pwbinom, Response) -->
    { Alpha <- alpha,
      NN <- 'N', [N] = NN,
      Pi_0 <- pi_0,
      Pi_1 <- pi_1,
      C = [k, dbinom(k, 'N', pi_0 = Pi_0), dbinom(k, 'N', pi_1 = Pi_1)], 
      maplist(mathml, C, Cols),
      findall(R, 
        ( between(0, N, K), 
	  P0 <- prob3(dbinom(K, 'N', pi_0)), 
	  P1 <- prob3(dbinom(K, 'N', pi_1)),
          maplist(mathml, [K, P0, P1], R)
        ), Rows)
    }, 
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'), 
              [ "Consider a clinical study with ", \mml('N' = round0(N)), " ",
                "patients. The variable ", \mml('X'), " represents the number ",
                "of therapeutic successes in the sample. We assume that the ",
                "successes occur independently, and under the null ",
                "hypothesis, the success probability is ", \mml(Pi_0), " in ",
                "all patients. The binomial probabilities are given in the ",
                "table below."
	          ]),
	        \table(Cols, Rows)
	      ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
                \question(question, 
                    response, 
                    [ "The significance level is set ",
                      "to ", \mml(alpha = '100%'(Alpha)), " one-tailed. Assuming ",
                      "that the success probability ",
                      "is ", \nowrap([\mml(Pi_1), ","]), " what is the power of ",
                      "the test?"
                    ], 
                    Response)
	      ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(pwbinom: binom, From >> To, _Flags, Feed, Hint) :-
    From = binom_power(Alpha, N, Pi_0, Pi_1),
    To   = prob(binom_power_2(Alpha, N, Pi_0, Pi_1)),
    Feed = "Correctly identified the problem as a binomial test.",
    Hint = "This is a binomial test.".

intermediate(pwbinom: binom_power_2/4).

% Choose upper critical value
expert(pwbinom: tail, From >> To, _Flags, Feed, Hint) :-
    From = binom_power_2(Alpha, N, Pi_0, Pi_1),
    To   = binom_power_3(tail("upper"), dist("upper"), Alpha, N, Pi_0, Pi_1),
    Feed = "Correctly determined the upper critical value.",
    Hint = "The upper critical value should be determined.".

intermediate(pwbinom: binom_power_3/6).

:- multifile buggy/5.
buggy(pwbinom: tail, From >> To, _Flags, Feed, Trap) :-
    From = binom_power_2(Alpha, N, Pi_0, Pi_1),
    To   = binom_power_3(instead_of(tail, tail("lower"), tail("upper")), instead_of(tail, dist("lower"), dist("upper")), Alpha, N, Pi_0, Pi_1),
    Feed = [ "The result matches the lower critical value. Please determine the ",
             "upper critical value."
           ],
    Trap = "The lower critical value is determined instead of the upper one.".

% Critical value based on distribution
expert(pwbinom: cumul, From >> To, _Flags, Feed, Hint) :-
    From = binom_power_3(Tail, Dist, Alpha, N, Pi_0, Pi_1),
    To   = binom_power_4(Tail, Dist, Alpha, N, Pi_0, Pi_1),
    Feed = [ "Correctly determined the critical value using the cumulative ",
             "distribution."
           ],
    Hint = [ "The critical value is determined using the cumulative ",
             "distribution."
	   ].

intermediate(pwbinom: binom_power_4/6).

buggy(pwbinom: cumul, From >> To, _Flags, Feed, Trap) :-
    From = binom_power_3(Tail, _Dist, Alpha, N, Pi_0, Pi_1),
    To   = binom_power_4(Tail, instead_of(cumul, dist("density"), dist("upper")), Alpha, N, Pi_0, Pi_1),
    Feed = [ "The result matches the critical value based on the probability ",
             "density. Please determine the critical value using the ",
             "cumulative distribution function."
           ],
    Trap = [ "The critical value is determined using the density instead of ",
             "the cumulative distribution." 
           ].

% Upper tail
expert(pwbinom: ubinom, From >> To, _Flags, Feed, Hint) :-
    From = binom_power_4(Tail, Dist, Alpha, N, Pi_0, Pi_1),
    C    = uqbinom(Tail, Dist, Alpha, N, Pi_0),
    To   = ubinom(denoting(c, C, "the crititcal value"), N, Pi_1),
    Feed = "Correctly determined the power from the upper tail.",
    Hint = "The power is the upper tail probability at the critical value.".

:- multifile r_init/1.
r_init(pwbinom) :-
    r_init,
    {|r||
        uqbinom = function(tail, dist, alpha, size, prob)
        {
	        if(tail == "upper" & dist == "upper")
	            return(ucbinom(alpha, size, prob))
	    
            if(tail == "lower" & dist == "lower")
	            return(lcbinom(alpha, size, prob))

	        if(tail == "upper" & dist == "density")
	            return(udbinom(alpha, size, prob))

            if(tail == "lower" & dist == "density")
	            return(ldbinom(alpha, size, prob))

	        stop("error")
	    }

        lcbinom = function(alpha, size, prob)
	    {
	        qbinom(alpha, size, prob, lower.tail=TRUE) - 1
	    }

	    ucbinom = function(alpha, size, prob)
	    {
            qbinom(alpha, size, prob, lower.tail=FALSE) + 1
	    }

	    ldbinom = function(alpha, size, prob)
	    {
	        k = 0:floor(size*prob)
	        p = dbinom(k, size=size, prob=prob)
            k = k[p <= alpha]
	        k[length(k)]
        }

	    udbinom = function(alpha, size, prob)
	    {
	        k = ceiling(size*prob):size
	        p = dbinom(k, size=size, prob=prob)
	        k = k[p <= alpha]
	        k[1]
	    }

        ubinom = function(c, size, prob)
        {
            dbinom(c, size, prob) + pbinom(c, size, prob, lower.tail=FALSE)
        }

	    tail = dist = identity

	    lower = function(alpha)
	    {
	        return(alpha)
	    }

	    upper = function(alpha)
	    {
	        return(1 - alpha)
	    }

	    prob3 = function(P)
	    {
	        sprintf("%.3f", P)
	    }

        alpha = 0.05
	    N     = 26
	    pi_0  = 0.6
	    pi_1  = 0.75
    |}.

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = pwbinom,
    r_init(Topic),
    item(Topic: Item),
    solution(Topic, Item, Solution, Path),
    writeln(solution: Solution),
    rod(Solution, Result),
    writeln(result: Result),
    writeln(path: Path),
    mathml(Solution = quantity(Result), Mathml),
    writeln(mathml: Mathml),
    hints(Topic, Item, Path, _, Hints),
    writeln(hints: Hints),
    traps(Topic, Item, Path, _, Traps),
    writeln(traps: Traps),
    praise(Topic, Item, Path, _, Praise),
    writeln(praise: Praise).

example :-
    Topic = pwbinom,
    r_init(Topic),
    item(Topic: Item),
    wrong(Topic, Item, Wrong, Woodden),
    writeln(wrong: Wrong),
    mathex(fix, Wrong, Fix),
    writeln(fix: Fix),
    mathex(show, Wrong, Show),
    writeln(show: Show),
    rod(Wrong, Result),
    writeln(result: Result),
    writeln(path: Woodden),
    palette(Wrong, Flags),
    mathml([highlight(all) | Flags], Wrong \= Result, Mathml),
    writeln(mathml: Mathml),
    feedback(Topic, Item, Woodden, Flags, _, Feedback),
    writeln(feedback: Feedback),
    praise(Topic, Item, Woodden, Code_Praise, Praise),
    writeln(praise: Praise),
    mistakes(Topic, Item, Woodden, Flags, Code_Mistakes, Mistakes),
    writeln(mistakes: Mistakes),
    solution(Topic, Item, _, Path),
    hints(Topic, Item, Path, Code_Hints, _),
    relevant(Code_Praise, Code_Hints, Rel_Praise, Irrel_Praise),
    writeln(relevant_praise: Rel_Praise),
    writeln(irrelevant_praise: Irrel_Praise),
    traps(Topic, Item, Path, Code_Traps, _),
    relevant(Code_Mistakes, Code_Traps, Rel_Mistakes, Irrel_Mistakes),
    writeln(relevant_mistakes: Rel_Mistakes),
    writeln(irrelevant_mistakes: Irrel_Mistakes).

example :-
    Topic = pwbinom,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = pwbinom,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

