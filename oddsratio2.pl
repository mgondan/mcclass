:- module(oddsratio2, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(interval/interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(oddsratio2, "OR (2)").
task(oratio).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering
mathml:math_hook(pi_A, subscript(pi, "A")).
mathml:math_hook(odds_A, subscript(odds, "A")).
mathml:math_hook(pi_B, subscript(pi, "B")).
mathml:math_hook(odds_B, subscript(odds, "B")).
mathml:math_hook(or, 'OR').

% R constants
r_hook(odds_A).
r_hook(pi_A).
r_hook(pi_B).
r_hook(or).
r_hook(odds_B).

render
--> { start(item(Pi_A, Pi_B)) },
    
    html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Odds ratio"),
              p(class('card-text'),
                [ "The success probability of therapy A ",
                  "is ", \mmlm([r(Pi_A), "."]), " Therapy B has a success ",
                  "probability of ", \mmlm([r(Pi_B), "."])
                ])]))]).

task(oratio)
--> { start(item(_Pi_A, _Pi_B)),
      session_data(resp(oddsratio2, oratio, Resp), resp(oddsratio2, oratio, '#.##'))
	}, 
    html(\htmlform(["What is the odds ratio relative to therapy A?"], oratio, Resp)).


% Odds ratio with two probabilities
intermediate(oratio, item).
start(item(pi_A, pi_B)).

expert(oratio, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A, Pi_B),
    To = 
      { '<-'(odds_A, odds(Pi_A)) ;
        '<-'(odds_B, odds(Pi_B)) ; 
        '<-'(or, odds_ratio(odds_B, odds_A))
      }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is an odds ratio.".

% Determine the odds for A and B
intermediate(oratio, odds).
expert(oratio, stage(1), From, To, [step(expert, odds, [Pi, Odds])]) :-
    From = '<-'(Odds, odds(Pi)),
    To = '<-'(Odds, dfrac(Pi, 1 - Pi)).

feedback(odds, [Pi, _], Col, FB)
 => FB = [ "Correctly determined the odds ", "from ", \mmlm(Col, [Pi, "."]) ].

hint(odds, [Pi, Odds], Col, FB)
 => FB = [ "Convert ", \mmlm(Col, Pi), " to the respective ",
           "odds, ", \mmlm(Col, [Odds = dfrac(Pi, 1 - Pi), "."])
         ].

% Calculate OR
intermediate(oratio, odds_ratio).
expert(oratio, stage(2), From, To, [step(expert, odds_ratio, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = Odds_B / Odds_A.

feedback(odds_ratio, [Odds_B, Odds_A], Col, FB)
 => FB = [ "Sucessfully calculated ", \mmlm(Col, [Odds_B / Odds_A, "."]) ].

hint(odds_ratio, [Odds_B, Odds_A], Col, FB)
 => FB = [ "Divide ", \mmlm(Col, Odds_B), " by ", \mmlm(Col, Odds_A), " to ",
           "determine the OR."
         ].

% Forgot conversion of pi to odds
buggy(oratio, stage(1), From, To, [step(buggy, forget_odds, [Pi, Odds])]) :-
    From = '<-'(Odds, odds(Pi)),
    To = '<-'(Odds, omit_right(forget_odds, dfrac(Pi, 1 - Pi))).

feedback(forget_odds, [Pi, Odds], Col, FB)
 => FB = [ "Please remember to ",
           "convert ", \mmlm(Col, color(forget_odds, Pi)), " ",
           "to ", \mmlm(Col, [color(forget_odds, Odds = frac(Pi, 1 - Pi)), "."])
         ].

hint(forget_odds, [Pi, _Odds], Col, FB)
 => FB = [ "Do not forget to ",
           "convert ", \mmlm(Col, color(forget_odds, Pi)), " to the ",
           "respective odds."
         ].

% Confuse odds_A and odds_B
buggy(oratio, stage(2), From, To, [step(buggy, inverse, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = instead(inverse, odds_A / odds_B, odds_B / odds_A).

feedback(inverse, [Odds_B, Odds_A], Col, FB)
 => FB = [ "The result matches the inverse, ",
           "with ", \mmlm(Col, color(inverse, Odds_A)), " being divided ",
           "by ", \mmlm(Col, color(inverse, Odds_B)), " instead ",
           "of ", \mmlm(Col, [color(inverse, Odds_B / Odds_A), "."]) 
         ].

hint(inverse, [_Odds_B, _Odds_A], _Col, FB)
 => FB = "Make sure to put the correct odds into the denominator.".

% Product instead of ratio
buggy(oratio, stage(2), From, To, [step(buggy, product, [Odds_B, Odds_A])]) :-
    From = odds_ratio(Odds_B, Odds_A),
    To = instead(product, odds_A * odds_B, odds_B / odds_A).

feedback(product, [Odds_B, Odds_A], Col, FB)
 => FB = [ "The result matches the product of the two odds instead ",
           "of the ratio, ", \mmlm(Col, [color(product, Odds_B / Odds_A), "."])
         ].

hint(product, [_Odds_B, _Odds_A], _Col, FB)
 => FB = "Make sure to calculate the ratio (not the product).".

