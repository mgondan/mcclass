% Binomial density

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).
:- use_module(interval).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

% Render symbols
mathml:hook(Flags, n, Flags, 'N').
mathml:hook(Flags, p0, Flags, pi).

% R definitions
interval:hook(pl, n, r(n)).
interval:hook(pl, k, r(k)).
interval:hook(pl, p0, r(p0)).

render(dbinom, item(K, N, P0), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Binary outcomes"),
            p(class("card-text"),
              [ "Consider a clinical study with ", \mmlm(r(N)), " patients. ",
                "We assume that the success probability ",
                "is ", \mmlm(r(P0)), " in all patients, and that the ",
                "successes occur independently."
	      ])
          ])),
        \htmlform([ "What is the probability for exactly ", \mmlm(r(K)), " ",
                    "successes?" ], "#dbinom", R)
      ]).

intermediate(dbinom, item).
start(dbinom, item(k, n, p0)).

% Recognise as a binomial density
intermediate(dbinom, dbinom).
expert(dbinom, stage(2), From, To, [step(expert, dens, [])]) :-
    From = item(K, N, P0),
    To   = dbinom(K, N, P0).

feedback(dbinom, dens, [], _Col, Feed) :-
    Feed = [ "Correctly recognised the problem as a binomial probability." ].

hint(dbinom, dens, [], _Col, Hint) :-
    Hint = [ "This is a binomial probability." ].

% Convert to product
intermediate(dbinom, bernoulli).
expert(dbinom, stage(2), From, To, [step(expert, prod, [K, N, P0])]) :-
    From = dbinom(K, N, P0),
    To   = choose(N, K) * bernoulli(K, N, P0).

feedback(dbinom, prod, [_K, _N, _P0], _Col, Feed) :-
    Feed = [ "Correctly identified the formula for the binomial probability." ].

hint(dbinom, prod, [K, N, P0], Col, Hint) :-
    Hint = [ "The formula for the binomial probability ",
             "is ", \mmlm(Col, choose(N, K) * bernoulli(K, N, P0)), "."
           ].

% Successes and failures
intermediate(dbinom, successes).
intermediate(dbinom, failures).
expert(dbinom, stage(2), From, To, [step(expert, bern, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = successes(K, P0) * failures(N - K, 1 - P0).

feedback(dbinom, bern, [K, N, _P0], Col, Feed) :-
    Feed = [ "Correctly determined the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ", 
             "and ", \mmlm(Col, N - K), " failures."
           ].

hint(dbinom, bern, [K, N, _P0], Col, Hint) :-
    Hint = [ "Determine the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ",
             "and ", \mmlm(Col, N - K), "failures."
           ].

% Successes
expert(dbinom, stage(2), From, To, [step(expert, success, [K, P0])]) :-
    From = successes(K, P0),
    To   = P0^K.

feedback(dbinom, success, [K, _P0], Col, Feed) :-
    Feed = [ "Correctly determined the probability for ", \mml(Col, K), " ",
             "independent successes."
           ].

hint(dbinom, success, [K, _P0], Col, Hint) :-
    Hint = [ "Determine the probability for ", \mml(Col, K), " independent ",
             "successes."
           ].

% Failures - same as successes (this may change)
expert(dbinom, stage(2), From, To, [step(expert, failure, [K, P0])]) :-
    From = failures(K, P0),
    To   = P0^K.

feedback(dbinom, failure, [K, _P0], Col, Feed) :-
    Feed = [ "Correctly determined the probability for ", \mml(Col, K), " ",
             "independent failures."
           ].

hint(dbinom, failure, [K, _P0], Col, Hint) :-
    Hint = [ "Determine the probability for ", \mml(Col, K), " independent ",
             "failures."
           ].

% Forget binomial coefficient
buggy(dbinom, stage(2), From, To, [step(buggy, nochoose, [K, N])]) :-
    From = dbinom(K, N, P0),
    To   = omit_left(bug(nochoose), choose(N, K) * bernoulli(K, N, P0)).

feedback(dbinom, nochoose, [_K, _N], _Col, Feed) :-
    Feed = [ "The binomial coefficient with the number of permutations was ",
             "omitted." ].

hint(dbinom, nochoose, [K, N], Col, Hint) :-
    Hint = [ "Do not forget to multiply everything with the number of ",
             "permutations ", \mmlm(Col, choose(N, K)), "."
           ].

% Treat binomial coefficient like a fraction
buggy(dbinom, stage(2), From, To, [step(buggy, choosefrac, [K, N])]) :-
    From = choose(N, K),
    To   = instead(bug(choosefrac), dfrac(N, K), choose(N, K)).

feedback(dbinom, choosefrac, [K, N], Col, Feed) :-
    Feed = [ "Please determine the number of permutations using the ",
             "binomial coefficient ", 
             \mmlm(Col, choose(N, K) = 
               dfrac(factorial(N), factorial(K)*factorial(N-K))), "." ].

hint(dbinom, choosefrac, [K, N], Col, Hint) :-
    Hint = [ "The number of permutations is determined using the binomial ",
             "coefficient ", \mmlm(Col, choose(N, K)), "."
           ].

% Confuse successes and failures
buggy(dbinom, stage(2), From, To, [step(buggy, succfail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = instead(bug(succfail), successes(K, 1 - P0), successes(K, P0)) * 
           instead(bug(succfail), failures(N - K, P0), failures(N - K, 1 - P0)).

feedback(dbinom, succfail, [_K, _N, P0], Col, Feed) :-
    Feed = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
             "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
             "failure were confused."
           ].

hint(dbinom, succfail, [_K, _N, P0], Col, Hint) :-
    Hint = [ "Make sure not to confuse the ",
             "probabilities ", \mmlm(Col, P0), " for success ",
             "and ", \mmlm(Col, 1 - P0), " for failure."
           ]. 

% Forget failures
buggy(dbinom, stage(2), From, To, [step(buggy, nofail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = omit_right(bug(nofail), successes(K, P0) * failures(N - K, 1 - P0)).

feedback(dbinom, nofail, [K, N, _P0], Col, Feed) :-
    Feed = [ "The probability for the ", \mmlm(Col, color(nofail, N - K)), " ",
             "failures was omitted."
           ].

hint(dbinom, nofail, [K, N, _P0], Col, Hint) :-
    Hint = [ "Make sure not to forget the ",
             "probability for the ", \mmlm(Col, N - K), " failures."
           ].

