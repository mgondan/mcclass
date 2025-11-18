:- module(sequence, []).

% Binomial density
:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(sequence, "binomial probability").
label(exactprob, "Exact Probability").
label(exactseq, "Exact sequence").
label(succrun, "Success run").

task(exactprob).
task(exactseq).
task(succrun).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

math_hook(n, 'N').
math_hook(p0, pi).
math_hook(ns, 'N').
math_hook(ks, 'k').
math_hook(ps0, pi).
math_hook(successes(K, P0), P0^K).
math_hook(failures(K, P0), P0^K).

macro(p).
macro(n).
macro(k).
macro(p0).
macro(ns).
macro(ks).
macro(ps0).
macro(choose/2, all, [+, +]).
macro(factorial/1, all, [+]).


render(Flags)
--> {start(item(_K, _N, _P0, _KS, NS, PS0)) },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Binary outcomes"),
            p(class("card-text"),
              [ "Consider a clinical study with ", \mmlm(Flags, r(NS)), " patients. ",
                "Assume that the success probability ",
                "is ", \mmlm(Flags, r(PS0)), " in all patients, and that the ",
                "successes occur independently."
              ])
          ]))
      ]).


% Question for the Exact probability
task(Flags, exactprob)
--> { start(item(K, _N, _P0, _KS, _NS, _PS0)), 
      session_data(resp(sequence, exactprob, Resp), resp(sequence, exactprob, '#.##'))
    },
    html(\htmlform([ "What is the probability of exactly ", \mmlm(Flags, r(K)), " ",
        "successes?" ], exactprob, Resp)).


% Question for the Exact sequence
task(Flags, exactseq)
--> { start(item(_K, _N, _P0, KS, NS, _PS0)),
      session_data(resp(sequence, exactseq, Resp), resp(sequence, exactseq, '#.##'))
    },
    html(\htmlform([ "What is the probability ",
         "for the specific sequence with ", \mmlm(Flags, r(KS)),
         " successes followed by ", \mmlm(Flags, r(NS - KS)),
         " failures?"], exactseq, Resp)).


% Question for the Success run
task(Flags, succrun)
--> { start(item(_K, _N, _P0, KS, NS, _PS0)),
      session_data(resp(sequence, succrun, Resp), resp(sequence, succrun, '#.##'))
    },
    html(\htmlform([ "What is the probability ",
         "for the specific sequence with ", \mmlm(Flags, r(KS)),
         " successes followed by ", \mmlm(Flags, r(NS - KS)),
         " successes ", u("or"), " failures?"], succrun, Resp)).

intermediate(exactprob, item).
start(item(k, n, p0, ks, ns, ps0)).


%
% Expert rules for the exactprob task
%
% First step: recognise as a binomial density
intermediate(exactprob, dbinom).
expert(exactprob, stage(1), From, To, [step(expert, sequence, [])]) :-
    From = item(K, N, P0, _KS, _NS, _PS0),
    To   = dbinom(K, N, P0).

feedback(sequence, [], _Col, F)
 => F = [ "Correctly recognised the problem as a binomial probability." ].

hint(sequence, _Col, H)
 => H = "This is a binomial probability.".

% Second step: convert to product
intermediate(exactprob, bernoulli).
expert(exactprob, stage(1), From, To, [step(expert, prod, [K, N, P0])]) :-
    From = dbinom(K, N, P0),
    To   = choose(N, K) * bernoulli(K, N, P0).

feedback(prod, [_K, _N, _P0], _Col, F)
 => F = [ "Correctly identified the formula for the binomial probability." ].

hint(prod, Col, H)
 => H = [ "The formula for the binomial probability ",
          "is ", \mmlm(Col, choose(n, k) * p0^k * (1 - p0)^(n - k)), "."
        ].

% Third step: successes and failures
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

% Fourth step: successes
expert(exactprob, stage(3), From, To, [step(expert, success, [K, P0])]) :-
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

% Fifth step: failures - same as successes (this may change)
expert(exactprob, stage(3), From, To, [step(expert, failure, [K, P0])]) :-
    From = failures(K, P0),
    To   = P0^K.

feedback(failure, [K, _P0], Col, F)
 => F = [ "Correctly determined the probability for ", \mmlm(Col, K),
          " independent failures."
        ].

hint(failure, Col, H)
 => H = [ "Determine the probability for ", \mmlm(Col, k), " independent ",
          "failures."
        ].

%
% Buggy rules for the exactprob task
%
% Buggy rule: forget binomial coefficient
buggy(exactprob, stage(1), From, To, [step(buggy, nochoose, [K, N])]) :-
    From = dbinom(K, N, P0),
    To   = omit_left(nochoose, choose(N, K) * bernoulli(K, N, P0)).

feedback(nochoose, [_K, _N], _Col, F)
 => F = [ "The binomial coefficient for the number of permutations was ",
          "omitted."
        ].

hint(nochoose, Col, H)
 => H = [ "Do not forget to multiply by the number of ",
          "permutations ", \mmlm(Col, choose(n, k)), "."
        ].


% Buggy rule: treat binomial coefficient like a fraction
buggy(exactprob, stage(1), From, To, [step(buggy, choosefrac, [K, N])]) :-
    From = choose(N, K),
    To   = instead(choosefrac, dfrac(N, K), choose(N, K)).

feedback(choosefrac, [K, N], Col, F)
 => F = [ "The number of permutations was mistakenly calculated by dividing the number of trials ",
          \mmlm(Col, N), " by the number of successes ", \mmlm(Col, [K, "."])
        ].

hint(choosefrac, _Col, H)
 => H = [ "The binomial coefficient includes factorials." ].


% Buggy rule: omit (N-k)! in the denominator of the binomial coefficient
buggy(exactprob, stage(1), From, To, [step(buggy, choosefail, [K, N])]) :-
    From = choose(N, K),
    To   = instead(choosefail, dfrac(factorial(N), factorial(K)), choose(N, K)).

feedback(choosefail, [K, N], Col, F)
 => F = [ \mmlm(Col, color(choosefail, factorial(N - K))), " is missing in the formula for the binomial coefficient." 
        ].

hint(choosefail, Col, H)
 => H = [ "Do not forget ", \mmlm(Col, color(choosefail, factorial(n - k))), 
          " in the formula for the binomial coefficient."
        ].


% Buggy rule: confuse successes and failures
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
 => H = [ "Do not confuse the probability of success ", \mmlm(Col, p0), " ",
          "with the probability of failure ", \mmlm(Col, 1 - p0)]. 


% Buggy rule: forget failures
buggy(exactprob, stage(2), From, To, [step(buggy, nofail, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = omit_right(nofail, successes(K, P0) * failures(N - K, 1 - P0)).

feedback(nofail, [K, N, _P0], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nofail, N - K)), " ",
          "failures was omitted."
        ].

hint(nofail, Col, H)
 => H = [ "Do not forget to include the ",
          "probability for the ", \mmlm(Col, n - k), " failures."
        ].


%
% Expert rules for the Exact sequence task
%
intermediate(exactseq, item).
expert(exactseq, stage(1), From, To, [step(expert, exactseq, [])]) :-
    From = item(_K, _N, _P0, KS, NS, PS0),
    To = PS0^KS * (1 - PS0)^(NS - KS).

feedback(exactseq, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a specific sequence of successes and failures (permutation)." ].

hint(exactseq, _Col, H)
 => H = [ "Use the formula P = p0^K * (1 - p0)^(N - K) to calculate the probability for a specific sequence of successes and failures (permutation)." ].


%
% Expert rules for the Success run task
%
intermediate(succrun, item).
expert(succrun, stage(1), From, To, [step(expert, succrun, [])]) :-
    From = item(_K, _N, _P0, KS, _NS, PS0),
    To = PS0^KS.

feedback(succrun, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a specific run of successes." ].

hint(succrun, _Col, H)
 => H = [ "Use the formula P = p0^K to calculate the probability for a specific run of successes." ].

