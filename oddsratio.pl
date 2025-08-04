:- module(oddsratio, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(oddsratio, "OR (1)").
task(oratio).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(pi_A, subscript(pi, "A")).
math_hook(odds_A, subscript(odds, "A")).
math_hook(pi_B, subscript(pi, "B")).
math_hook(odds_B, subscript(odds, "B")).
math_hook(or, 'OR').

% R constants
macro(odds_A).
macro(pi_A).
macro(pi_B).
macro(or).
macro(odds_B).

render(Flags)
--> { start(item(Pi_A, OR)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Odds ratio"),
		      p(class('card-text'),
                [ "We consider two therapies A and B. The success probability of therapy A ",
                  "is ", \mmlm(Flags, [r(Pi_A), "."]), " The odds ratio is ", \mmlm(Flags, r(OR)), "relative ",
                  "to therapy A."
                ])]))]).

task(_Flags, oratio)
--> { start(item(_Pi_A, _OR)),
      session_data(resp(oddsratio, oratio, Resp), resp(oddsratio, oratio, '#.##'))
	},
	html(\htmlform(["What is the success probability of therapy B?"], oratio, Resp)).
      

intermediate(oratio, item).
start(item(pi_A, or)).

% Recognized the problem
expert(oratio, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A, OR),
    To = { '<-'(odds_A, odds(Pi_A)) ;
           '<-'(odds_B, odds_ratio(odds_A, OR)) ; 
           '<-'(pi_B, inv_odds(odds_B))
         }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, _Col, F)
 => F = "This is an odds ratio.".

% Determine the odds for A
intermediate(oratio, odds).
expert(oratio, stage(1), From, To, [step(expert, odds, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(odds, [Pi_A, _], Col, FB)
 => FB = [ "Correctly determined the odds ",
           "from ", \mmlm(Col, [Pi_A, "."])
         ].

hint(odds, Col, F)
 => F = [ "Convert the success probability of therapy A, ", 
          \mmlm(Col, [pi_A, ","]), " to the respective ",
          "odds, ", \mmlm(Col, [odds_A = dfrac(pi_A, 1 - pi_A), "."])
        ].

% Calculate the odds for B
intermediate(oratio, odds_ratio).
expert(oratio, stage(2), From, To, [step(expert, odds_ratio, [Odds_A, odds_B])]) :-
    From = odds_ratio(Odds_A, OR),
    To = Odds_A * OR.

feedback(odds_ratio, [_Odds_A, Odds_B], Col, FB)
 => FB = [ "Sucessfully ",
           "calculated ", \mmlm(Col, [Odds_B, "."])
         ].

hint(odds_ratio, Col, F)
 => F = [ "Multiply ", \mmlm(Col, odds_A), " by the odds ratio to ",
          "obtain ", \mmlm(Col, [odds_B, "."])
        ].

% Determine the probability for B
intermediate(oratio, inv_odds).
expert(oratio, stage(3), From, To, [step(expert, inv_odds, [Odds_B, pi_B])]) :-
    From = inv_odds(Odds_B),
    To = dfrac(Odds_B, 1 + Odds_B).

feedback(inv_odds, [Odds_B, _], Col, FB)
 => FB = ["Correctly determined the success probability from ", \mmlm(Col, [Odds_B, "."])].

hint(inv_odds, Col, F)
 => F = [ "Back-transform ", \mmlm(Col, odds_B), " to the respective success ",
          "probability, ", \mmlm(Col, [pi_B = dfrac(odds_B, 1 + odds_B), "."])
        ].

%%Buggy rules
% Forgot conversion of pi_A to odds_A
buggy(oratio, stage(1), From, To, [step(buggy, forget_odds, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = omit_right(forget_odds, dfrac(Pi_A, 1 - Pi_A)).

feedback(forget_odds, [Pi_A, Odds_A], Col, FB)
 => FB = [ "Please remember to ",
           "convert ", \mmlm(Col, color(forget_odds, Pi_A)), " ",
           "to ", \mmlm(Col, [color(forget_odds, Odds_A = frac(Pi_A, 1 - Pi_A)), "."])
         ].

hint(forget_odds, Col, F)
 => F = [ "Do not forget to ",
          "convert ", \mmlm(Col, color(forget_odds, pi_A)), " to the ",
          "respective odds."
        ].

% Divided odds_A and OR rather then multiplying them.
buggy(oratio, stage(2), From, To, [step(buggy, divide, [Odds_A, OR])]) :-
    From = odds_ratio(Odds_A, OR),
    To = instead(divide, Odds_A / OR, Odds_A * OR).

feedback(divide, [Odds_A, OR], Col, FB)
 => FB = [ "The response matches the inverse result in ",
           "which ", \mmlm(Col, color(divide, Odds_A)), " was divided by ", 
           "the ", \mmlm(Col, color(divide, OR)), " rather than multiplied."
         ].

hint(divide, Col, F)
 => F = [ "Make sure to multiply by ", \mmlm(Col, color(divide, or)), " ",
          "instead of dividing by it."
        ].

% Forgot to convert odds_B to pi_B
buggy(oratio, stage(3), From, To, [step(buggy, forget_prob, [Odds_B, pi_B])]) :-
    From = inv_odds(Odds_B),
    To = omit_right(forget_prob, dfrac(Odds_B, 1 + Odds_B)).

feedback(forget_prob, [Odds_B, Pi_B], Col, FB)
 => FB = [ "Please remember to ",
           "back-transform ", \mmlm(Col, color(forget_prob, Odds_B)), " to ",
           \mmlm(Col, [color(forget_prob, Pi_B = frac(Odds_B, 1 + Odds_B)), "."])
         ].

hint(forget_prob, Col, F)
 => F = [ "Do not forget to ",
          "back-transform ", \mmlm(Col, color(forget_prob, odds_B)), " to ",
          "the respective probability."
        ].

% Used OR instead of Pi_A: Needs to be fixed, because causes stack overflow
/* buggy(oratio, stage(1), From, To, [step(buggy, wrong_pi_A, [Pi_A, OR])]) :-
    From = odds(Pi_A),
    To = instead(wrong_pi_A, Pi_A, OR).

feedback(wrong_pi_A, [Pi_A, OR, Odds_A], Col, FB)
 => FB = [ "Please use the success probability ",
           \mmlm(Col, color(wrong_pi_A, Pi_A)), " to calculate the respective ",
          \mmlm(Col, color(wrong_pi_A, Odds_A)), " instead of the odds ratio ",
           \mmlm(Col, [color(wrong_pi_A, OR), "."])
         ].

hint(wrong_pi_A, Col, F)
 => F = [ "Don't use the odds ratio ", \mmlm(Col, color(wrong_pi_A, or)),
          "to calculate ", \mmlm(Col, [color(wrong_pi_A, odds_A), "."]),
          " Use the the success probability ", \mmlm(Col, color(wrong_pi_A, pi_A)),
          " instead."
        ].
*/
