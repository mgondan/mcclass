:- module(easyodds, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(r).
:- use_module(rint).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(easyodds, "OR (3)").
task(oratio).
:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(pi_A, sub(pi, "A")).
mathml_hook(odds_A, sub(odds, "A")).
mathml_hook(pi_B, sub(pi, "B")).
mathml_hook(or, 'OR').

% R constants
rint:r_hook(odds_A).
rint:r_hook(pi_A).
rint:r_hook(pi_B).
rint:r_hook(or).

render
--> {start(item(Odds_A, Pi_B, OR)) }, 
	
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "The Odds for sucess with treatment A are ", 
			 \mmlm([r(Odds_A), ","]), " treatment B has a probability of success of ",
			 \mmlm([r(Pi_B), ","]), " and the Odds Ratio between both treatments is ",
		         \mmlm([r(OR), "."])
		       ])]))]).

task(oratio, Form)
	--> {start(item(_Odds_A, _Pi_B, _OR)),
	option(resp(Resp), Form, '#.##')
	}, 
	html(\htmlform(["What is the probability of sucess with treatment A?"], "oratio", 
	Resp)).

% Odds ratio with two probabilities. 
intermediate(oratio, item).
start(item(odds_A, pi_B, or)).

expert(oratio, stage(2), From, To, [step(expert, odd, [])]) :-
    From = item(Odds_A, _Pi_B, _OR),
    To = { '<-'(pi_A, dfrac(Odds_A, 1 + Odds_A)) ;
	   pi_A
	 }.

feedback(odd, [], Col, FB) =>
    FB = [ "Correctly recognised the problem as an ", 
           \mmlm(Col, hyph(odds, "ratio")), "."].

hint(odd, [], Col, FB) =>
    FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

% 1) Tried conversion from odds to odds, as if starting with 
%    a probability.
buggy(oratio, stage(2), From, To, [step(buggy, sub, [Odds_A])]) :-
    From = 1 + Odds_A,
    To = instead(sub, 1 - Odds_A, 1 + Odds_A).

feedback(sub, [Odds_A], Col, FB) =>
    FB = [ "Please use the formula converting, ", \mmlm(Col, color(sub, Odds_A)), " to ", 
	   \mmlm(Col, color(sub, pi_A)) ].

hint(sub, [Odds_A], Col, FB) =>
    FB = [ "Do not try to further convert ", \mmlm(Col, color(sub, Odds_A)), " to odds." ].

% 2) Used pi_B rather than odds_A.
buggy(oratio, stage(1), From, To, [step(buggy, pi, [])]) :-
    From = odds_A,
    To = instead(pi, pi_B, odds_A).

feedback(pi, [], Col, FB) =>
    FB = [ "Please extract and use the value for ", \mmlm(Col, color(pi, odds_A)), " instead." ].

hint(pi, [], Col, FB) =>
    FB = [ "Do not execute your calculations using ", \mmlm(Col, color(pi,  pi_B)) ].

% 3) Used or rather than odds_A.
buggy(oratio, stage(1), From, To, [step(buggy, ratio, [])]) :-
    From = odds_A,
    To = instead(ratio, or, odds_A).

feedback(ratio, [], Col, FB) =>
    FB = [ "Please extract and use the value for ", \mmlm(Col, color(ratio, odds_A)), " instead." ].

hint(ratio, [], Col, FB) =>
    FB = [ "Do not execute your calculations using the ", \mmlm(Col, color(ratio,  "OR")) ].

