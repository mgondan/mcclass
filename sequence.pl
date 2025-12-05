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
label(exactseq, "Exact sequence").
label(partseq, "Partial sequence").
label(succrun, "Success run").
label(failrun, "Failure run").

task(exactseq).
task(partseq).
task(succrun).
task(failrun).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

math_hook(n, 'N').
math_hook(p0, pi).
math_hook(successes(K, P0), P0^K).
math_hook(failures(K, P0), P0^K).

macro(p).
macro(n).
macro(k).
macro(p0).
macro(ks).
macro(kf).
macro(choose/2, all, [+, +]).
macro(factorial/1, all, [+]).


render(Flags)
--> {start(item(_K, N, P0, _Ks, _Kf)) },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Binary outcomes"),
            p(class("card-text"),
              [ "Consider a clinical study with ", \mmlm(Flags, r(N)), " patients. ",
                "Assume that the success probability ",
                "is ", \mmlm(Flags, r(P0)), " in all patients, and that the ",
                "successes occur independently."
              ])
          ]))
      ]).


% Question for the Exact sequence
task(Flags, exactseq)
--> { start(item(K, N, _P0, _Ks, _Kf)),
      session_data(resp(sequence, exactseq, Resp), resp(sequence, exactseq, '#.##'))
    },
    html(\htmlform([ "What is the probability ",
         "for the specific sequence with ", \mmlm(Flags, r(K)),
         " successes followed by ", \mmlm(Flags, r(N - K)),
         " failure(s)?"], exactseq, Resp)).


% Question for the Partial sequence
task(Flags, partseq)
--> { start(item(_K, _N, _P0, Ks, Kf)),
      session_data(resp(sequence, partseq, Resp), resp(sequence, partseq, '#.##'))
    },
    html(\htmlform([ "What is the probability for the specific sequence with ",
         \mmlm(Flags, r(Ks)), " success(es) followed by ",
         \mmlm(Flags, r(Kf)), " failure(s)?"], partseq, Resp)).


% Question for the Success run
task(Flags, succrun)
--> { start(item(K, N, _P0, _Ks, _Kf)),
      session_data(resp(sequence, succrun, Resp), resp(sequence, succrun, '#.##'))
    },
    html(\htmlform([ "What is the probability ",
         "for the specific sequence with ", \mmlm(Flags, r(K)),
         " successes followed by ", \mmlm(Flags, r(N - K)),
         " success(es) ", u("or"), " failure(s)?"], succrun, Resp)).


% Question for the Failure run
task(Flags, failrun)
--> { start(item(K, N, _P0, _Ks, _Kf)),
      session_data(resp(sequence, failrun, Resp), resp(sequence, failrun, '#.##'))
    },
    html(\htmlform([ "What is the probability ",
         "for the specific sequence with ", \mmlm(Flags, r(N - K)),
         " failure(s) followed by ", \mmlm(Flags, r(K)),
         " success(es) ", u("or"), " failure(s)?"], failrun, Resp)).

start(item(k, n, p0, ks, kf)).


%
% Expert rules for the Exact sequence task
%
intermediate(exactseq, item).
expert(exactseq, stage(1), From, To, [step(expert, exactseq, [])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To = P0^K * (1 - P0)^(N - K).

feedback(exactseq, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a specific sequence of successes and failures (permutation)." ].

hint(exactseq, Col, H)
 => H = [ "Use the formula ", 
        \mmlm(Col, p0^k * (1 - p0)^(n - k)), " to calculate the probability for a ", 
        "specific sequence of successes and failures (permutation)." ].

%
% Buggy rules for the Exact sequence task
%
% Buggy rule: confuse probability of success and failure
buggy(exactseq, stage(1), From, To, [step(buggy, succfail, [P0])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = instead(succfail, (1 - P0)^K, P0^K) * 
           instead(succfail, P0^(N - K), (1 - P0)^(N - K)).

feedback(succfail, [P0], Col, F)
 => F = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
          "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
          "failure were confused."
        ].

hint(succfail, Col, H)
 => H = [ "Do not confuse the probability of success ", \mmlm(Col, p0), " ",
          "with the probability of failure ", \mmlm(Col, 1 - p0), "." ].

% Buggy rule: forget failures
buggy(exactseq, stage(1), From, To, [step(buggy, nofail, [K, N])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = omit_right(nofail, P0^K * (1 - P0)^(N - K)).

feedback(nofail, [K, N], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nofail, N - K)), " ",
          "failures was omitted."
        ].

hint(nofail, Col, H)
 => H = [ "Do not forget to include the ",
          "probability for the ", \mmlm(Col, n - k), " failures."
        ].

% Buggy rule: forget successes
buggy(exactseq, stage(1), From, To, [step(buggy, nosucc, [K])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = omit_left(nosucc, P0^K * (1 - P0)^(N - K)).

feedback(nosucc, [K], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nosucc, K)), " ",
          "successes was omitted."
        ].

hint(nosucc, Col, H)
 => H = [ "Do not forget to include the ",
          "probability for the ", \mmlm(Col, k), " successes."
        ].

%
% Expert rules for the Partial sequence task
%
intermediate(partseq, item).
expert(partseq, stage(1), From, To, [step(expert, partseq, [])]) :-
    From = item(_K, _N, P0, Ks, Kf),
    To = P0^Ks * (1 - P0)^Kf.

feedback(partseq, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a partial sequence." ].

hint(partseq, Col, H)
 => H = [ "Use the formula ", \mmlm(Col, p0^ks * (1-p0)^kf), " to calculate the probability", 
        " for a partial sequence." ].

%
% Buggy rules for the Partial sequence task
%
% Buggy rule: wrong failures
buggy(partseq, stage(1), From, To, [step(buggy, wrongfail, [Ks, Kf, N])]) :-
    From = item(_K, N, P0, Ks, Kf),
    To   = P0^Ks * instead(wrongfail, (1 - P0)^(N - Ks), (1 - P0)^Kf).

feedback(wrongfail, [Ks, Kf, N], Col, F)
 => F = [ "The number of failures ",
        \mmlm(Col, color(wrongfail, Kf)), " were confused with ",
        \mmlm(Col, color(wrongfail, N - Ks)), "." ].

hint(wrongfail, Col, H)
 => H = [ "In this case, the number of failures is not equal to ", 
        \mmlm(Col, n - ks), "." ].

% Buggy rule: forget failures (2)
buggy(partseq, stage(1), From, To, [step(buggy, nofail2, [Kf])]) :-
    From = item(_K, _N, P0, Ks, Kf),
    To   = omit_right(nofail2, P0^Ks * (1 - P0)^Kf).

feedback(nofail2, [Kf], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nofail2, Kf)), " ",
          "failures was omitted."
        ].

hint(nofail2, Col, H)
 => H = [ "Do not forget to include the ",
          "probability for the ", \mmlm(Col, kf), " failures."
        ].

% Buggy rule: forget successes (2)
buggy(partseq, stage(1), From, To, [step(buggy, nosucc2, [Ks])]) :-
    From = item(_K, _N, P0, Ks, Kf),
    To   = omit_left(nosucc2, P0^Ks * (1 - P0)^Kf).

feedback(nosucc2, [Ks], Col, F)
 => F = [ "The probability for the ", \mmlm(Col, color(nosucc2, Ks)), " ",
          "successes was omitted."
        ].

hint(nosucc2, Col, H)
 => H = [ "Do not forget to include the ",
          "probability for the ", \mmlm(Col, ks), " successes."
        ].

% Buggy rule: confuse probability of success and failure
buggy(partseq, stage(1), From, To, [step(buggy, succfail, [P0])]) :-
    From = item(_K, _N, P0, Ks, Kf),
    To   = instead(succfail, (1 - P0)^Ks, P0^Ks) * 
           instead(succfail, P0^Kf, (1 - P0)^Kf).

feedback(succfail, [P0], Col, F)
 => F = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
          "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
          "failure were confused."
        ].

hint(succfail, Col, H)
 => H = [ "Do not confuse the probability of success ", \mmlm(Col, p0), " ",
          "with the probability of failure ", \mmlm(Col, 1 - p0), "." ].

%
% Expert rules for the Success run task
%
intermediate(succrun, item).
expert(succrun, stage(1), From, To, [step(expert, succrun, [])]) :-
    From = item(K, _N, P0, _Ks, _Kf),
    To = P0^K.

feedback(succrun, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a specific run of successes." ].

hint(succrun, Col, H)
 => H = [ "Use the formula ", \mmlm(Col, p0^k), " to calculate the probability for a specific run of successes." ].

%
% Buggy rules for the Success run task
%
% Buggy rule: confuse probability of success and failure
buggy(succrun, stage(1), From, To, [step(buggy, succfail, [P0])]) :-
    From = item(K, _N, P0, _Ks, _Kf),
    To   = instead(succfail, (1 - P0)^K, P0^K).

feedback(succfail, [P0], Col, F)
 => F = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
          "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
          "failure were confused."
        ].

hint(succfail, Col, H)
 => H = [ "Do not confuse the probability of success ", \mmlm(Col, p0), " ",
          "with the probability of failure ", \mmlm(Col, 1 - p0), "." ].

% Buggy rule: confuse number of successes and failures
buggy(succrun, stage(1), From, To, [step(buggy, numbfail, [K, N])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = instead(numbfail, P0^(N - K), P0^K).

feedback(numbfail, [K, N], Col, F)
 => F = [ "The number of successes ", \mmlm(Col, color(numbfail, K)), " and ",
          "the number of failures ",
          \mmlm(Col, color(numbfail, N - K)),
          " were confused."
        ].

hint(numbfail, Col, H)
 => H = [ "Do not confuse the number of successes ", \mmlm(Col, k), " ",
          "with the number of failures ", \mmlm(Col, n - k), "." ].

% Buggy rule: added failures
buggy(succrun, stage(1), From, To, [step(buggy, addfail, [K, N, P0])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To = add_right(addfail, P0^K * (1 - P0)^(N - K)).

feedback(addfail, [K, N, P0], Col, F)
 => F = [ "The probability for failure ", \mmlm(Col, color(addfail, (1 - P0)^(N - K))),
          " was incorrectly added to the expression." ].

hint(addfail, Col, H)
 => H = [ "Do not add the probability for failure ", \mmlm(Col, (1 - p0)^(n - k)), " to the expression." ].

%
% Expert rules for the Failure run task
%
intermediate(failrun, item).
expert(failrun, stage(1), From, To, [step(expert, failrun, [])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To = (1 - P0)^(N - K).

feedback(failrun, [], _Col, F)
 => F = [ "Correctly applied the formula to calculate the probability for a specific run of failures." ].

hint(failrun, Col, H)
 => H = [ "Use the formula ", \mmlm(Col, (1 - p0)^(n - k)), " to calculate the probability for a specific run of failures." ].

%
% Buggy rules for the Failure run task
%
% Buggy rule: confuse probability of success and failure
buggy(failrun, stage(1), From, To, [step(buggy, succfail, [P0])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = instead(succfail, P0^(N - K), (1 - P0)^(N - K)).

feedback(succfail, [P0], Col, F)
 => F = [ "The probabilities ", \mmlm(Col, color(succfail, P0)), " for ",
          "success and ", \mmlm(Col, color(succfail, 1 - P0)), " for ",
          "failure were confused."
        ].

hint(succfail, Col, H)
 => H = [ "Do not confuse the probability of success ", \mmlm(Col, p0), " ",
          "with the probability of failure ", \mmlm(Col, 1 - p0), "." ].

% Buggy rule: confuse number of successes and failures
buggy(failrun, stage(1), From, To, [step(buggy, numbfail, [K, N])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To   = instead(numbfail, (1 - P0)^K, (1 - P0)^(N - K)).

feedback(numbfail, [K, N], Col, F)
 => F = [ "The number of successes ", \mmlm(Col, color(numbfail, K)), " and ",
          "the number of failures ",
          \mmlm(Col, color(numbfail, N - K)),
          " were confused."
        ].

hint(numbfail, Col, H)
 => H = [ "Do not confuse the number of successes ", \mmlm(Col, k), " ",
          "with the number of failures ", \mmlm(Col, n - k), "." ].

% Buggy rule: added successes
buggy(failrun, stage(1), From, To, [step(buggy, addsucc, [K, P0])]) :-
    From = item(K, N, P0, _Ks, _Kf),
    To = add_right(addsucc, (1 - P0)^(N - K) * P0^K).

feedback(addsucc, [K, P0], Col, F)
 => F = [ "The probability for success ", \mmlm(Col, color(addsucc, P0^K)),
          " was incorrectly added to the expression." ].

hint(addsucc, Col, H)
 => H = [ "Do not add the probability for success ", \mmlm(Col, p0^k), " to the expression." ].

