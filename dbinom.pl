% Binomial density
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

mathml:math_hook(Flags, pi_00, Flags, pi).

r:p_hook(k, r(k)).
r:p_hook(pi_00, r(pi_00)).

%    
% Binomial density
%
:- multifile item/1.
item(dbinom: binary_dbinom(k, 'N', pi_00)).

:- multifile intermediate/1.
intermediate(dbinom: binary_dbinom/3).

:- multifile item//2.
item(dbinom, Response) -->
    { K <- k,
      N <- 'N',
      Pi_0 <- pi_00
    }, 
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Binary outcomes"),
          p(class('card-text'), 
            [ "Consider a clinical study with ", \mml(round0(N)), " patients. ",
	      "We assume that the success probability is ", \mml(Pi_0), " in ",
	      "all patients, and successes occur independently."
	    ])
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            \question(question, 
                response, 
                [ "What is the probability for exactly ", \mml(K), " successes?" ], 
                Response)
	  ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(dbinom: binary_dbinom, From >> To, _Flags, Feed, Hint) :-
    From = binary_dbinom(K, N, Pi),
    To   = binary_dbinom_2(K, N, Pi),
    Feed = "Correctly identified the problem as a binomial probability.",
    Hint = "This is a binomial probability.".

intermediate(dbinom: binary_dbinom_2/3).

expert(dbinom: dbinom_prod, From >> To, Flags, Feed, Hint) :-
    From = binary_dbinom_2(K, N, Pi),
    To   = prob(choose(N, K) * bernoulli(K, N, Pi)),
    Feed = [ "Correctly identified the expression for the binomial ",
             "probability."
	   ],
    Hint = [ "The expression for the binomidal probability is ", 
             \nowrap([\mml(Flags, choose(K, N) * bernoulli(K, N, Pi)), "."]) 
	   ].

intermediate(dbinom: bernoulli/3).

expert(dbinom: dbinom_bernoulli, From >> To, Flags, Feed, Hint) :-
    From = bernoulli(K, N, Pi),
    To   = successes(K, Pi) * failures(N-K, 1-Pi),
    Feed = [ "Correctly determined the probability for a sequence of ",
             "successes and failures."
	   ],
    Hint = [ "Determine the probability for a sequence of ",
             \mml(Flags, K), " successes and ", \mml(Flags, N-K), "failures."
           ].

intermediate(dbinom: successes/2).

expert(dbinom: successes, From >> To, Flags, Feed, Hint) :-
    From = successes(K, Pi),
    To   = Pi^K,
    Feed = [ "Correctly determined the probability for ", \mml(Flags, K), " ",
             "independent successes."
	   ],
    Hint = [ "Determine the probability for ", \mml(Flags, K), " independent ",
             "successes."
           ].

intermediate(dbinom: failures/2).

expert(dbinom: failures, From >> To, Flags, Feed, Hint) :-
    From = failures(K, Pi),
    To   = Pi^K,
    Feed = [ "Correctly determined the probability for ", \mml(Flags, K), " ",
             "independent failures."
	   ],
    Hint = [ "Determine the probability for ", \mml(Flags, K), " independent ",
             "failures."
           ].

:- multifile r_init/1.
r_init(dbinom) :-
    r_init,
    {|r||
        k     = 14
	N     = 26
	pi_00  = 0.6
    |}.

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = dbinom,
    r_init(Topic),
    item(Topic: Item),
    solution(Topic, Item, Solution, Path),
    writeln(solution: Solution),
    r(Solution, Result),
    writeln(result: Result),
    writeln(path: Path),
    mathml(Solution = number(Result), Mathml),
    writeln(mathml: Mathml),
    hints(Topic, Item, Path, _, Hints),
    writeln(hints: Hints),
    traps(Topic, Item, Path, _, Traps),
    writeln(traps: Traps),
    praise(Topic, Item, Path, _, Praise),
    writeln(praise: Praise).

example :-
    Topic = dbinom,
    r_init(Topic),
    item(Topic: Item),
    wrong(Topic, Item, Wrong, Woodden),
    writeln(wrong: Wrong),
    mathex(fix, Wrong, Fix),
    writeln(fix: Fix),
    mathex(show, Wrong, Show),
    writeln(show: Show),
    r(Wrong, Result),
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
    Topic = dbinom,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = dbinom,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

