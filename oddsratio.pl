:- module(oddsratio, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(oddsratio, "odds ratio").

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

label(oratio, [math(mi('OR'))]).
label(successprob, [math("Therapy B")]).

% Prettier symbols for mathematical rendering
math_hook(pi_A, subscript(pi, "A")).
math_hook(odds_A, subscript(odds, "A")).
math_hook(pi_B, subscript(pi, "B")). % oratio task
math_hook(pi_B_2, pi_B). % successprob task
math_hook(odds_B, subscript(odds, "B")).
math_hook(or, 'OR'). % successprob task
math_hook(or_1, 'OR'). % oratio task

% R constants
macro(odds_A).
macro(pi_A).
macro(pi_B).
macro(pi_B).
macro(or).
macro(odds_B).

render(Flags)
--> { start(item(Pi_A, _Pi_B, _OR)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Odds ratio"),
		      p(class('card-text'),
                [ "We consider two therapies A and B. The success probability of therapy A ",
                  "is ", \mmlm(Flags, [r(Pi_A), "."])
                ])]))]).

task(oratio).
task(successprob). 
 
% Question for the odds ratio task
task(Flags, oratio)
--> { start(item(_Pi_A, Pi_B, _OR)),
      session_data(resp(oddsratio2, oratio, Resp), resp(oddsratio2, oratio, '#.##'))
	}, 
    html(\htmlform(["What is the odds ratio relative to therapy A if the success probability for therapy B is ", 
                     \mmlm(Flags, r(Pi_B)), "?"], oratio, Resp)).

% Question for the success probability task  
task(Flags, successprob)
--> { start(item(_Pi_A, _Pi_B, OR)),
      session_data(resp(oddsratio, successprob, Resp), resp(oddsratio, successprob, '#.##'))
	},
	html(\htmlform(["What is the success probability of therapy B 
                   if the odds ratio relative to therapy A is ", \mmlm(Flags, r(OR)), "?"], successprob, Resp)).

 
start(item(pi_A, pi_B, or)).

%
% Expert rules for the odds ratio task
%
% Step 1: Recognize the problem
intermediate(oratio, item).
expert(oratio, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A, Pi_B, _OR),
    To = 
      { '<-'(odds_A, odds(Pi_A)) ;
        '<-'(odds_B, odds(Pi_B)) ; 
        '<-'(or_1, odds_ratio(odds_B, odds_A))
      }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, _Col, F)
 => F = "This is an odds ratio.".

% Step 2: Determine the odds for A and B
intermediate(oratio, odds).
expert(oratio, stage(1), From, To, [step(expert, odds, [Pi, Odds])]) :-
    From = '<-'(Odds, odds(Pi)),
    To = '<-'(Odds, dfrac(Pi, 1 - Pi)).

feedback(odds, [Pi, _], Col, FB)
 => FB = [ "Correctly determined the odds ", "from ", \mmlm(Col, [Pi, "."]) ].

hint(odds, Col, F)
 => F = [ "Convert ", \mmlm(Col, pi), " to the respective ",
          "odds, ", \mmlm(Col, [odds = dfrac(pi, 1 - pi), "."])
        ].

% Step 3: Calculate OR
intermediate(oratio, odds_ratio).
expert(oratio, stage(2), From, To, [step(expert, odds_ratio, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = Odds_B / Odds_A.

feedback(odds_ratio, [Odds_B, Odds_A], Col, FB)
 => FB = [ "Correctly determined the odds ratio ", \mmlm(Col, [Odds_B / Odds_A, "."]) ].

hint(odds_ratio, Col, F)
 => F = [ "Divide ", \mmlm(Col, odds_b), " by ", \mmlm(Col, odds_a), " to ",
          "determine the OR."
        ].

%
% Buggy rules for the oratio task
%
% Buggy-Rule: Forgot conversion of pi to odds
buggy(oratio, stage(1), From, To, [step(buggy, forget_odds, [Pi, Odds])]) :-
    From = '<-'(Odds, odds(Pi)),
    To = '<-'(Odds, omit_right(forget_odds, dfrac(Pi, 1 - Pi))).

feedback(forget_odds, [Pi, Odds], Col, FB)
 => FB = [ "Forgot to convert ", \mmlm(Col, color(forget_odds, Pi)),
            " to ", \mmlm(Col, [color(forget_odds, Odds = frac(Pi, 1 - Pi)), "."])
         ].

hint(forget_odds, Col, F)
 => F = [ "Do not forget to convert ", 
          \mmlm(Col, color(forget_odds, pi)), 
          " to the respective odds."
        ].

% Buggy-Rule: Confuse odds_A and odds_B
buggy(oratio, stage(2), From, To, [step(buggy, inverse, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = instead(inverse, odds_A / odds_B, odds_B / odds_A).

feedback(inverse, [Odds_B, Odds_A], Col, FB)
 => FB = [ "The result matches the inverse, ",
           "with ", \mmlm(Col, color(inverse, Odds_A)), " being divided ",
           "by ", \mmlm(Col, color(inverse, Odds_B)), " instead ",
           "of ", \mmlm(Col, [color(inverse, Odds_B / Odds_A), "."]) 
         ].

hint(inverse, _Col, F)
 => F = "Make sure to put the correct odds into the denominator.".

% Buggy-Rule: Product instead of ratio
buggy(oratio, stage(2), From, To, [step(buggy, product, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = instead(product, dot(odds_A, odds_B), odds_B / odds_A).

feedback(product, [Odds_B, Odds_A], Col, FB)
 => FB = [ "The result matches the product of the two odds instead ",
           "of the ratio, ", \mmlm(Col, [color(product, Odds_B / Odds_A), "."])
         ].

hint(product, _Col, F)
 => F = "Make sure to calculate the ratio (not the product).".



% Expert rules for the success probability task
%
% Step 1: Recognize the problem
intermediate(successprob, item).
expert(successprob, stage(1), From, To, [step(expert, problem2, [])]) :-
    From = item(Pi_A, _Pi_B, OR),
    To = { '<-'(odds_A, odds(Pi_A)) ;
           '<-'(odds_B, odds_ratio(odds_A, OR)) ; 
           '<-'(pi_B_2, inv_odds(odds_B))
         }.

feedback(problem2, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem2, _Col, F)
 => F = "This is an odds ratio.".

% Step 2: Determine the odds for A
intermediate(successprob, odds).
expert(successprob, stage(1), From, To, [step(expert, odds2, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(odds2, [Pi_A, _], Col, FB)
 => FB = [ "Correctly determined the odds ",
           "from ", \mmlm(Col, [Pi_A, "."])
         ].

hint(odds2, Col, F)
 => F = [ "Convert the success probability of therapy A, ", 
          \mmlm(Col, [pi_A, ","]), " to the respective ",
          "odds, ", \mmlm(Col, [odds_A = dfrac(pi_A, 1 - pi_A), "."])
        ].

% Step 3: Calculate the odds for B
intermediate(successprob, odds_ratio).
expert(successprob, stage(2), From, To, [step(expert, odds_ratio2, [Odds_A, odds_B])]) :-
    From = odds_ratio(Odds_A, OR),
    To = dot(Odds_A, OR).

feedback(odds_ratio2, [_Odds_A, Odds_B], Col, FB)
 => FB = [ "Correctly determined ", \mmlm(Col, [Odds_B, "."])
         ].

hint(odds_ratio2, Col, F)
 => F = [ "Multiply ", \mmlm(Col, odds_A), " by the odds ratio to ",
          "obtain ", \mmlm(Col, [odds_B, "."])
        ].

% Step 4: Determine the probability for B
intermediate(successprob, inv_odds).
expert(successprob, stage(3), From, To, [step(expert, inv_odds, [Odds_B, pi_B])]) :-
    From = inv_odds(Odds_B),
    To = dfrac(Odds_B, 1 + Odds_B).

feedback(inv_odds, [Odds_B, _], Col, FB)
 => FB = ["Correctly determined the success probability from ", \mmlm(Col, [Odds_B, "."])].

hint(inv_odds, Col, F)
 => F = [ "Back-transform ", \mmlm(Col, odds_B), " to the respective success ",
          "probability, ", \mmlm(Col, [pi_B = dfrac(odds_B, 1 + odds_B), "."])
        ].

%
% Buggy rules for the success probability task
%
% Buggy-Rule: Forgot conversion of pi_A to odds_A
buggy(successprob, stage(1), From, To, [step(buggy, forget_odds2, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = omit_right(forget_odds, dfrac(Pi_A, 1 - Pi_A)).

feedback(forget_odds2, [Pi_A, Odds_A], Col, FB)
 => FB = [ "Forgot to convert ", \mmlm(Col, color(forget_odds, Pi_A)), 
           " to ", \mmlm(Col, [color(forget_odds, Odds_A = frac(Pi_A, 1 - Pi_A)), "."])
         ].

hint(forget_odds2, Col, F)
 => F = [ "Do not forget to ",
          "convert ", \mmlm(Col, color(forget_odds, pi_A)), " to the ",
          "respective odds."
        ].

% Buggy-Rule: Divided odds_A and OR rather then multiplying them.
buggy(successprob, stage(2), From, To, [step(buggy, divide, [Odds_A, OR])]) :-
    From = odds_ratio(Odds_A, OR),
    To = instead(divide, Odds_A / OR, dot(Odds_A, OR)).

feedback(divide, [Odds_A, OR], Col, FB)
 => FB = [ "The response matches the inverse result in ",
           "which ", \mmlm(Col, color(divide, Odds_A)), " was divided by ", 
           "the ", \mmlm(Col, color(divide, OR)), " rather than multiplied."
         ].

hint(divide, Col, F)
 => F = [ "Make sure to multiply by ", \mmlm(Col, color(divide, or)), " ",
          "instead of dividing by it."
        ].

% Buggy-Rule: Forgot to convert odds_B to pi_B
buggy(successprob, stage(3), From, To, [step(buggy, forget_prob, [Odds_B, pi_B])]) :-
    From = inv_odds(Odds_B),
    To = omit_right(forget_prob, dfrac(Odds_B, 1 + Odds_B)).

feedback(forget_prob, [Odds_B, Pi_B], Col, FB)
 => FB = [ "Forgot to ",
           "back-transform ", \mmlm(Col, color(forget_prob, Odds_B)), " to ",
           \mmlm(Col, [color(forget_prob, Pi_B = frac(Odds_B, 1 + Odds_B)), "."])
         ].

hint(forget_prob, Col, F)
 => F = [ "Do not forget to ",
          "back-transform ", \mmlm(Col, color(forget_prob, odds_B)), " to ",
          "the respective probability."
        ].

% Buggy-Rule: Used OR instead of Pi_A
buggy(successprob, stage(1), From, To, [step(buggy, wrong_pi_A, [or, odds_A])]) :-
    From = odds(Pi_A),
    To = dfrac(instead(wrong_pi_A, or, Pi_A), 1 - instead(wrong_pi_A, or, Pi_A)).

feedback(wrong_pi_A, [OR, Odds_A], Col, FB)
 => FB = [ "The odds ratio ", \mmlm(Col, color(wrong_pi_A, OR)), 
           " was mistakenly used to calculate ", 
           \mmlm(Col, [color(wrong_pi_A, Odds_A), "."])
         ].

hint(wrong_pi_A, Col, F)
 => F = [ "Do not use the odds ratio ", \mmlm(Col, color(wrong_pi_A, or)),
          " to calculate ", \mmlm(Col, [color(wrong_pi_A, odds_A), "."]),
          " Use the the success probability ", \mmlm(Col, color(wrong_pi_A, pi_A)),
          " instead."
        ].

