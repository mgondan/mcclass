:- module(oddsratio, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(oddsratio, "OR (1)").

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(pi_A, sub(pi, "A")).
mathml_hook(odds_A, sub(odds, "A")).
mathml_hook(pi_B, sub(pi, "B")).
mathml_hook(odds_B, sub(odds, "B")).
mathml_hook(or, 'OR').

% R constants
interval:r_hook(odds_A).
interval:r_hook(pi_A).
interval:r_hook(pi_B).
interval:r_hook(or).
interval:r_hook(odds_B).

render(item(Pi_A, OR), Form) -->
    { option(resp(R), Form, '#.##') },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Odds ratio"),
		      p(class('card-text'),
                [ "We consider two therapies A and B. The success probability of Therapy A ",
                  "is ", \mmlm([r(Pi_A), "."]), " The odds ratio is ", \mmlm(r(OR)), "relative ",
                  "to Therapy A."
                ])
            ])),
        \htmlform("What is the success probability of Therapy B?", "#oddsratio", R)
      ]).

intermediate(item).
start(item(pi_A, or)).

% Recognized the problem
expert(stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A, OR),
    To = { '<-'(odds_A, odds(Pi_A)) ;
           '<-'(odds_B, odds_ratio(odds_A, OR)) ; 
           '<-'(pi_B, inv_odds(odds_B)) ; 
           pi_B
         }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is an odds ratio.".

% Determine the odds for A
intermediate(odds).
expert(stage(1), From, To, [step(expert, odds, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(odds, [Pi_A, _], Col, FB)
 => FB = ["Correctly determined the odds from ", \mmlm(Col, Pi_A)].

hint(odds, [Pi_A, Odds_A], Col, FB)
 => FB = ["Convert ", \mmlm(Col, Pi_A), " to the respective ",
          "odds, ", \mmlm(Col, Odds_A = dfrac(Pi_A, 1 - Pi_A))].

% Calculate the odds for B
intermediate(odds_ratio).
expert(stage(2), From, To, [step(expert, odds_ratio, [Odds_A, odds_B])]) :-
    From = odds_ratio(Odds_A, OR),
    To = Odds_A * OR.

feedback(odds_ratio, [_Odds_A, Odds_B], Col, FB)
 => FB = ["Sucessfully calculated ", \mmlm(Col, Odds_B)].

hint(odds_ratio, [Odds_A, Odds_B], Col, FB)
 => FB = ["Multiply ", \mmlm(Col, Odds_A), " by the odds ratio to ",
          "obtain ", \mmlm(Col, Odds_B)].

% Determine the probability for B
intermediate(inv_odds).
expert(stage(3), From, To, [step(expert, inv_odds, [Odds_B, pi_B])]) :-
    From = inv_odds(Odds_B),
    To = dfrac(Odds_B, 1 + Odds_B).

feedback(inv_odds, [Odds_B, _], Col, FB)
 => FB = ["Correctly determined the success probability from ", \mmlm(Col, Odds_B)].

hint(inv_odds, [Odds_B, Pi_B], Col, FB)
 => FB = ["Back-transform ", \mmlm(Col, Odds_B), " to the respective ",
          "success probability, ", \mmlm(Col, Pi_B = dfrac(Odds_B, 1 + Odds_B))].

%% Forgot conversion  of pi_a to odds.
%buggy(stage(2), From, To, [step(buggy, cona, [Pi_A])]) :-
%    From = dfrac(Pi_A, (1 - Pi_A)),
%    To = omit_right(cona, dfrac(Pi_A, 1 - Pi_A)).
%
%feedback(cona, [Pi_A], Col, FB) =>
%    FB = [ "Please remember to convert ", \mmlm(Col, color(cona, Pi_A)), " to ",
%	   \mmlm(Col, color(cona, odds_A)), ", with ", \mmlm(Col, color(cona, odds_A = frac(Pi_A, 1 - Pi_A)))  ].
%
%hint(cona, [Pi_A], Col, FB) =>
%    FB = [ "You should try converting ", \mmlm(Col, color(cona, Pi_A)), 
%	   " to odds before continuing." ].
%
%% Forgot to multiply odds_a and or.
%buggy(stage(2), From, To, [step(buggy, mult, [OR])]) :-
%    From = odds_A * OR,
%    To = omit_right(mult, odds_A * OR).
%
%feedback(mult, [OR], Col, FB) =>
%    FB = [ "You forgot to multiply ", \mmlm(Col, color(mult, odds_A)), " with ",
%	   \mmlm(Col, color(mult, OR)) ].
%
%hint(mult, [OR], Col, FB) =>
%    FB = [ "Please remember to multiply ",\mmlm(Col, color(mult, odds_A)), " with ",
%	   \mmlm(Col, color(mult, OR)) ].
%
%% Divided odds_A and or rather then multiplying them.
%buggy(stage(2), From, To, [step(buggy, divi, [OR])]) :-
%    From = odds_A * OR,
%    To = instead(divi, odds_A / OR, odds_A * OR).
%
%feedback(divi, [OR], Col, FB) =>
%    FB = [ "You divided ", \mmlm(Col, color(divi, odds_A)), " by ", 
%	   \mmlm(Col, color(divi, OR)), " rather than multiplying them." ].
%
%hint(divi, [OR], Col, FB) =>
%    FB = [ "Please remember to multiply ", \mmlm(Col, color(divi, odds_A)), " with ",
%	   \mmlm(Col, color(divi, OR)), " rather then dividing them." ].
%
%% Forgot to convert odds_B to pi_B.
%buggy(stage(2), From, To, [step(buggy, nopi, [])]) :-
%    From = dfrac(odds_B, 1 + odds_B),
%    To = omit_right(nopi, dfrac(odds_B, 1 + odds_B)).
%
%feedback(nopi, [], Col, FB) =>
%    FB = [ "You forgot to convert ", 
%	   \mmlm(Col, color(nopi, odds_B)),
%           " back into a probability ", \mmlm(Col, color(nopi, pi_B)), 
%	   ", with ", \mmlm(Col, color(nopi, pi_B = frac(odds_B, 1 + odds_B))) ].
%
%hint(nopi, [], Col, FB) =>
%    FB = [ "Remember that your end result should be a probability ",
%	   "rather than ", \mmlm(Col, color(nopi, odds_B)), "." ].

