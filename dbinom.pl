:- module(dbinom, []).

% Binomial density
:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).
:- use_module(interval).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Render symbols
mathml:hook(Flags, n, [task(dbinom) | Flags], 'N').
mathml:hook(Flags, p0, [task(dbinom) | Flags], pi).

% R definitions
interval:r_hook(n).
interval:r_hook(k).
interval:r_hook(p0).
interval:r_hook(factorial(_N)).

render(item(K, N, P0), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Binary outcomes"),
            p(class("card-text"),
              [ "Consider a clinical study with ", \mmlm([task(dbinom)], r(N)), " patients. ",
                "We assume that the success probability ",
                "is ", \mmlm([task(dbinom)], r(P0)), " in all patients, and that the ",
                "successes occur independently."
              ])
          ])),
        \htmlform([ "What is the probability for exactly ", \mmlm([task(dbinom)], r(K)), " ",
                    "successes?" ], "#dbinom", R)
      ]).

intermediate(item).
start(item(k, n, p0)).

% Recognise as a binomial density
intermediate(dbinom).
expert(stage(2), From, To, [step(expert, dens, [])]) :-
    From = item(K, N, P0),
    To   = dbinom(K, N, P0).

feedback(dens, [], _Col, Feed) =>
    Feed = [ "Correctly recognised the problem as a binomial probability." ].

hint(dens, [], _Col, Hint) =>
    Hint = [ "This is a binomial probability." ].

% Convert to product
intermediate(bernoulli).
expert(stage(2), From, To, [step(expert, prod, [K, N, P0])]) :-
    From = dbinom(K, N, P0),
    To   = choose(N, K) * bernoulli(K, N, P0).

feedback(prod, [_K, _N, _P0], _Col, Feed) =>
    Feed = [ "Correctly identified the formula for the binomial probability." ].

hint(prod, [K, N, P0], Col, Hint) =>
    Hint = [ "The formula for the binomial probability ",
             "is ", \mmlm(Col, choose(N, K) * bernoulli(K, N, P0)), "."
           ].

% Successes and failures
intermediate(successes).
intermediate(failures).
expert(stage(2), From, To, [step(expert, bern, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = successes(K, P0) * failures(N - K, 1 - P0).

feedback(bern, [K, N, _P0], Col, Feed) =>
    Feed = [ "Correctly determined the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ", 
             "and ", \mmlm(Col, N - K), " failures."
           ].

hint(bern, [K, N, _P0], Col, Hint) =>
    Hint = [ "Determine the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ",
             "and ", \mmlm(Col, N - K), "failures."
           ].

% Successes
expert(stage(2), From, To, [step(expert, success, [K, P0])]) :-
    From = successes(K, P0),
    To   = P0^K.

feedback(success, [K, _P0], Col, Feed) =>
    Feed = [ "Correctly determined the probability for ", \mml(Col, K), " ",
             "independent successes."
           ].

hint(success, [K, _P0], Col, Hint) =>
    Hint = [ "Determine the probability for ", \mml(Col, K), " independent ",
             "successes."
           ].

% Failures - same as successes (this may change)
expert(stage(2), From, To, [step(expert, failure, [K, P0])]) :-
    From = failures(K, P0),
    To   = P0^K.

feedback(failure, [K, _P0], Col, Feed) =>
    Feed = [ "Correctly determined the probability for ", \mml(Col, K), " ",
             "independent failures."
           ].

hint(failure, [K, _P0], Col, Hint) =>
    Hint = [ "Determine the probability for ", \mml(Col, K), " independent ",
             "failures."
           ].

% Forget binomial coefficient
buggy(stage(2), From, To, [step(buggy, nochoose, [K, N])]) :-
    From = dbinom(K, N, P0),
    To   = omit_left(bug(nochoose), choose(N, K) * bernoulli(K, N, P0)).

feedback(nochoose, [_K, _N], _Col, Feed) =>
    Feed = [ "The binomial coefficient with the number of permutations was ",
             "omitted." ].

hint(nochoose, [K, N], Col, Hint) =>
    Hint = [ "Do not forget to multiply everything with the number of ",
             "permutations ", \mmlm(Col, choose(N, K)), "."
           ].

% Treat binomial coefficient like a fraction
buggy(stage(2), From, To, [step(buggy, choosefrac, [K, N])]) :-
    From = choose(N, K),
    To   = instead(bug(choosefrac), dfrac(N, K), choose(N, K)).

feedback(choosefrac, [K, N], Col, Feed) =>
    Feed = [ "Please determine the number of permutations using the ",
             "binomial coefficient ", 
             \mmlm(Col, choose(N, K) = 
               dfrac(factorial(N), factorial(K)*factorial(N-K))), "." ].

hint(choosefrac, [K, N], Col, Hint) =>
    Hint = [ "The number of permutations is determined using the binomial ",
             "coefficient ", \mmlm(Col, choose(N, K)), "."
           ].

% Omit (N-k)! in the denominator of the binomial coefficient
buggy(stage(2), From, To, [step(buggy, choosefail, [K, N])]) :-
    From = choose(N, K),
    To   = instead(bug(choosefail), dfrac(factorial(N), factorial(K)), choose(N, K)).

feedback(choosefail, [K, N], Col, Feed) =>
    Feed = [ "Please determine the number of permutations using the ",
             "binomial coefficient ",
             \mmlm(Col, choose(N, K) =
               dfrac(factorial(N), factorial(K) * color(choosefail, factorial(N-K)))), "." ].

hint(choosefail, [K, N], Col, Hint) =>
    Hint = [ "The number of permutations is determined using the binomial ",
             "coefficient ", \mmlm(Col, choose(N, K) =
               dfrac(factorial(N), factorial(K)*factorial(N-K))), "."
           ].

% Confuse successes and failures
buggy(stage(2), From, To, [step(buggy, succfail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = instead(bug(succfail), successes(K, 1 - P0), successes(K, P0)) * 
           instead(bug(succfail), failures(N - K, P0), failures(N - K, 1 - P0)).

feedback(succfail, [_K, _N, P0], Col, Feed) =>
    Feed = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
             "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
             "failure were confused."
           ].

hint(succfail, [_K, _N, P0], Col, Hint) =>
    Hint = [ "Make sure not to confuse the ",
             "probabilities ", \mmlm(Col, P0), " for success ",
             "and ", \mmlm(Col, 1 - P0), " for failure."
           ]. 

% Forget failures
buggy(stage(2), From, To, [step(buggy, nofail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = omit_right(bug(nofail), successes(K, P0) * failures(N - K, 1 - P0)).

feedback(nofail, [K, N, _P0], Col, Feed) =>
    Feed = [ "The probability for the ", \mmlm(Col, color(nofail, N - K)), " ",
             "failures was omitted."
           ].

hint(nofail, [K, N, _P0], Col, Hint) =>
    Hint = [ "Make sure not to forget the ",
             "probability for the ", \mmlm(Col, N - K), " failures."
           ].

