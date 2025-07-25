:- module(dbinom, []).

% Binomial density
:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).


:- use_module(navbar).
navbar:page(dbinom, "Binomial probability").
task(exactprob).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

math_hook(n, 'N').
math_hook(p0, pi).
math_hook(successes(K, P0), P0^K).
math_hook(failures(K, P0), P0^K).

r_hook(p).
r_hook(n).
r_hook(k).
r_hook(p0).

r_hook(factorial/1).
r_hook(choose/2).
mono((choose)/2, [+, +]).
mono((factorial)/1, [+]).

render(Flags)
--> {start(item(_K, N, P0)) },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Binary outcomes"),
            p(class("card-text"),
              [ "Consider a clinical study with ", \mmlm(Flags, r(N)), " patients. ",
                "We assume that the success probability ",
                "is ", \mmlm(Flags, r(P0)), " in all patients, and that the ",
                "successes occur independently."
              ])
          ]))
      ]).
        

task(Flags, exactprob)
--> { start(item(K, _N, _P0)), 
      session_data(resp(dbinom, exactprob, Resp), resp(dbinom, exactprob, '#.##'))
    },
    html(\htmlform([ "What is the probability for exactly ", \mmlm(Flags, r(K)), " ",
        "successes?" ], exactprob, Resp)).

intermediate(exactprob, item).
start(item(k, n, p0)).

% Recognise as a binomial density
intermediate(exactprob, dbinom).
expert(exactprob, stage(2), From, To, [step(expert, dbinom, [])]) :-
    From = item(K, N, P0),
    To   = { dbinom(K, N, P0) }.

feedback(dbinom, [], _Col, F)
 => F = [ "Correctly recognised the problem as a binomial probability." ].

hint(dbinom, _Col, H)
 => H = "This is a binomial probability.".

% Convert to product
intermediate(exactprob, bernoulli).
expert(exactprob, stage(2), From, To, [step(expert, prod, [K, N, P0])]) :-
    From = dbinom(K, N, P0),
    To   = choose(N, K) * bernoulli(K, N, P0).

feedback(prod, [_K, _N, _P0], _Col, F)
 => F = [ "Correctly identified the formula for the binomial probability." ].

hint(prod, Col, H)
 => H = [ "The formula for the binomial probability ",
          "is ", \mmlm(Col, choose(n, k) * p0^k * (1 - p0)^(n - k)), "."
        ].

% Successes and failures
intermediate(exactprob, successes).
intermediate(exactprob, failures).
expert(exactprob, stage(2), From, To, [step(expert, bern, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = successes(K, P0) * failures(N - K, 1 - P0).

feedback(bern, [K, N, _P0], Col, F)
 => F = [ "Correctly determined the probability for a sequence ",
          "of ", \mmlm(Col, K), " successes ", 
          "and ", \mmlm(Col, N - K), " failures."
        ].

hint(bern, Col, H)
 => H = [ "Determine the probability for a sequence ",
          "of ", \mmlm(Col, k), " successes ",
          "and ", \mmlm(Col, n - k), " failures."
        ].

% Successes
expert(exactprob, stage(2), From, To, [step(expert, success, [K, P0])]) :-
    From = successes(K, P0),
    To   = P0^K.

feedback(success, [K, _P0], Col, F)
 => F = [ "Correctly determined the probability for ", \mmlm(Col, K), " ",
          "independent successes."
        ].

hint(success, Col, H)
 => H = [ "Determine the probability for ", \mmlm(Col, k), " independent ",
          "successes."
        ].

% Failures - same as successes (this may change)
expert(exactprob, stage(2), From, To, [step(expert, failure, [K, P0])]) :-
    From = failures(K, P0),
    To   = P0^K.

feedback(failure, [K, _P0], Col, F)
 => F = [ "Correctly determined the probability for ", \mmlm(Col, K), " ",
          "independent failures."
        ].

hint(failure, Col, H)
 => H = [ "Determine the probability for ", \mmlm(Col, k), " independent ",
          "failures."
        ].

% Forget binomial coefficient
buggy(exactprob, stage(2), From, To, [step(buggy, nochoose, [K, N])]) :-
    From = dbinom(K, N, P0),
    To   = omit_left(nochoose, choose(N, K) * bernoulli(K, N, P0)).

feedback(nochoose, [_K, _N], _Col, F)
 => F = [ "The binomial coefficient with the number of permutations was ",
          "omitted."
        ].

hint(nochoose, Col, H)
 => H = [ "Do not forget to multiply everything with the number of ",
          "permutations ", \mmlm(Col, choose(n, k)), "."
        ].

% Treat binomial coefficient like a fraction
buggy(exactprob, stage(2), From, To, [step(buggy, choosefrac, [K, N])]) :-
    From = choose(N, K),
    To   = instead(choosefrac, dfrac(N, K), choose(N, K)).

feedback(choosefrac, [K, N], Col, F)
 => F = [ "Please determine the number of permutations using the ",
          "binomial coefficient ", 
          \mmlm(Col, choose(N, K) = 
            dfrac(factorial(N), factorial(K)*factorial(N-K))), "." 
	].

hint(choosefrac, Col, H)
 => H = [ "The number of permutations is determined using the binomial ",
          "coefficient ", \mmlm(Col, choose(n, k) =
          dfrac(factorial(n), factorial(k) * factorial(n - k))), "."
        ].

% Omit (N-k)! in the denominator of the binomial coefficient
buggy(exactprob, stage(2), From, To, [step(buggy, choosefail, [K, N])]) :-
    From = choose(N, K),
    To   = instead(choosefail, dfrac(factorial(N), factorial(K)), choose(N, K)).

feedback(choosefail, [K, N], Col, F)
 => F = [ "Please determine the number of permutations using the ",
          "binomial coefficient ",
          \mmlm(Col, choose(N, K) =
            dfrac(factorial(N), factorial(K) * color(choosefail, factorial(N - K)))), "."
        ].

hint(choosefail, Col, H)
 => H = [ "The number of permutations is determined using the binomial ",
          "coefficient ", \mmlm(Col, choose(n, k) =
          dfrac(factorial(n), factorial(k) * factorial(n - k))), "."
        ].

% Confuse successes and failures
buggy(exactprob, stage(2), From, To, [step(buggy, succfail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = instead(succfail, successes(K, 1 - P0), successes(K, P0)) * 
           instead(succfail, failures(N - K, P0), failures(N - K, 1 - P0)).

feedback(succfail, [_K, _N, P0], Col, F)
 => F = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
          "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
          "failure were confused."
        ].

hint(succfail, Col, H)
 => H = [ "Make sure not to confuse the probabilities ", \mmlm(Col, p0), " ",
          "for success and ", \mmlm(Col, 1 - p0), " for failure."
        ]. 

% Forget failures
buggy(exactprob, stage(2), From, To, [step(buggy, nofail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = omit_right(nofail, successes(K, P0) * failures(N - K, 1 - P0)).

feedback(nofail, [K, N, _P0], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nofail, N - K)), " ",
          "failures was omitted."
        ].

hint(nofail, Col, H)
 => H = [ "Make sure not to forget the ",
          "probability for the ", \mmlm(Col, n - k), " failures."
        ].

