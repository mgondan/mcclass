:- module(easyodds, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(easyodds, "OR (3)").
task(oratio).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(pi_A, subscript(pi, "A")).
math_hook(odds_A, subscript(odds, "A")).
math_hook(pi_B, subscript(pi, "B")).
math_hook(or, 'OR').

% R constants
macro(odds_A).
macro(pi_A).
macro(pi_B).
macro(or).

render(Flags)
--> {start(item(Odds_A, Pi_B, OR)) }, 
	
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Odds ratio"),
		      p(class('card-text'),
		       [ "The odds for succeding with treatment A are ", 
			 \mmlm(Flags, [r(Odds_A), ","]), " while treatment B has a success probability of ",
			 \mmlm(Flags, [r(Pi_B), "."]), " The odds ratio between both treatments is ",
		         \mmlm(Flags, [r(OR), "."])
		       ])]))]).

task(_Flags, oratio)
--> { start(item(_Odds_A, _Pi_B, _OR)),
      session_data(resp(easyodds, oratio, Resp), resp(easyodds, oratio, '#.##'))
	}, 
	html(\htmlform(["What is the success probability for treatment A?"], oratio, Resp)).

% Odds ratio with two probabilities. 
intermediate(oratio, item).
start(item(odds_A, pi_B, or)).

expert(oratio, stage(2), From, To, [step(expert, odd, [])]) :-
    From = item(Odds_A, _Pi_B, _OR),
    To = { '<-'(pi_A, dfrac(Odds_A, 1 + Odds_A))
	 }.

feedback(odd, [], _Col, FB) =>
    FB = [ "Correctly calculated the success probability from ", \mmlm([odds_A, "."]) ].
           

hint(odd, _Col, F)
 => F = "This is an odds ratio.".

% 1) Tried conversion from odds to odds, as if starting with 
%    a probability.
buggy(oratio, stage(2), From, To, [step(buggy, subscript, [Odds_A])]) :-
    From = 1 + Odds_A,
    To = instead(subscript, 1 - Odds_A, 1 + Odds_A).

feedback(subscript, [Odds_A], Col, FB) =>
    FB = [ "Please use the correct formula to convert ", \mmlm(Col, color(subscript, Odds_A)), " to ", 
	   \mmlm(Col, [color(subscript, pi_A), "."]) ].

hint(subscript, _Col, F)
 => F = "Do not apply the formula to calculate the odds from the success probability.".

% 2) Used pi_B rather than odds_A.
buggy(oratio, stage(1), From, To, [step(buggy, pi, [])]) :-
    From = odds_A,
    To = instead(pi, pi_B, odds_A).

feedback(pi, [], Col, FB) =>
    FB = [ "Please use ", \mmlm(Col, color(pi, odds_A)), " instead of ", \mmlm(Col, [color(pi, pi_B), "."]) ].

hint(pi, Col, F)
 => F = [ "Do not use the success probability for therapy B ", \mmlm(Col, [color(pi,  pi_B), "."]) ].

% 3) Used OR rather than odds_A.
buggy(oratio, stage(1), From, To, [step(buggy, ratio, [])]) :-
    From = odds_A,
    To = instead(ratio, or, odds_A).

feedback(ratio, [], Col, FB) =>
    FB = [ "Please use ", \mmlm(Col, color(ratio, odds_A)), " instead of ", \mmlm(Col, [color(ratio, or), "."]) ].

hint(ratio, Col, F)
 => F = [ "Do not use the odds ratio ", \mmlm(Col, [color(ratio, or), "."]) ].

