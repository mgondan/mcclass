:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, pi_A, Flags, sub(pi, "A")).
mathml:hook(Flags, odds_A, Flags, sub(odds, "A")).
mathml:hook(Flags, pi_B, Flags, sub(pi, "B")).
mathml:hook(Flags, odds_B, Flags, sub(odds, "B")).
mathml:hook(Flags, or, Flags, 'OR').

%
% R constants
%
interval:hook(pl, odds_A, r(odds_A)).
interval:hook(pl, pi_A, r(pi_A)).
interval:hook(pl, pi_B, r(pi_B)).
interval:hook(pl, or, r(or)).
interval:hook(pl, odds_B, r(odds_B)).

render(oddsratio2, item(Pi_A, Pi_B), Form) -->
	{ option(resp(R), Form, '#.##') },
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "Compare the effectiveness of ",
		         "two therapies. Therapy A has a probability of success of ",
		          \mmlm(Pi_A = r(pi_A)), ". Therapy B one of ", \mmlm(Pi_B = r(pi_B))
		       ])
		    ])),
	          div(class(card), div(class('card-body'),
		     [ h4(class('card-title'), [a(id(question), []), "Question"]),
		       p(class('card-text'),
		        [ "What is the Odds Ratio in favor of treatment A?"
		        ]),
		       form([class(form), method('POST'), action('#oddsratio2-reverse')],
		        [ div(class("input-group mb-3"),
		           [ div(class("input-group-prepend"), 
		               span(class("input-group-text"), "Response")),
		             input([class("form-control"), type(text), name(resp), value(R)]),
		                div(class("input-group-append"),
		                   button([class('btn btn-primary'), type(submit)], "Submit"))
		        ])])
		     ]))
		]).

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% Odds ratio with two probabilities. 
intermediate(_, item).
start(oddsratio2, item(pi_A, pi_B)) :-
    init(oddsratio2).

expert(oddsratio2, stage(2), From, To, [step(expert, odd, [])]) :-
    From = item(Pi_A, Pi_B),
    To = { '<-'(odds_A, dfrac(Pi_A, 1 - Pi_A)) ;
	   '<-'(odds_B, dfrac(Pi_B, 1 - Pi_B)) ; 
	   '<-'(or, odds_A / odds_B) ; 
	   or
	 }.

feedback(oddsratio2, odd, [], Col, FB) :-
    FB = [ "Correctly recognised the problem as an ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(oddsratio2, odd, [], Col, FB) :-
    FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

% 1) Forgot conversion  of pi_a to odds.
buggy(oddsratio2, stage(2), From, To, [step(buggy, cona, [pi_A]), depends(conb)]) :-
    From = dfrac(pi_A, (1 - pi_A)),
    To = omit_right(bug(cona), dfrac(pi_A, 1 - pi_A)).

feedback(oddsratio2, cona, [pi_A], Col, FB) :-
    FB = [ "Please remember to convert ", \mmlm(Col, color(cona, pi_A)), " to ",
	   \mmlm(Col, color(cona, odds_A)), ", with ", \mmlm(Col, color(cona, odds_A = frac(pi_A, 1 - pi_A)))  ].

% 2) Forgot conversion  of pi_b to odds.
buggy(oddsratio2, stage(2), From, To, [step(buggy, conb, [pi_B]), depends(cona)]) :-
    From = dfrac(pi_B, (1 - pi_B)),
    To = omit_right(bug(conb), dfrac(pi_B, 1 - pi_B)).

feedback(oddsratio2, conb, [pi_B], Col, FB) :-
    FB = [ "Please remember to convert ", \mmlm(Col, color(conb, pi_B)), " to ",
	   \mmlm(Col, color(conb, odds_B)), ", with ", \mmlm(Col, color(conb, odds_B = frac(pi_B, 1 - pi_B)))  ].

hint(oddsratio2, conb, [pi_B], Col, FB) :-
    FB = [ "Do not forget to convert ", \mmlm(Col, color(conb, pi_A)), " and ",
	   \mmlm(Col, color(conb, pi_B)), " to odds before continuing." ].

% 3) Flipped odds_A and odds_B.
buggy(oddsratio2, stage(2), From, To, [step(buggy, flip, [])]) :-
    From = odds_A / odds_B,
    To = instead(bug(flip), odds_B / odds_A, odds_A / odds_B).

feedback(oddsratio2, flip, [], Col, FB) :-
    FB = [ "Divide ", \mmlm(Col, color(flip, odds_A)), " by ",
	   \mmlm(Col, color(flip, odds_B)), " instead of ", 
	   \mmlm(Col, color(flip, odds_B / odds_A)) ].

hint(oddsratio2, flip, [], Col, FB) :-
    FB = [ "Try using ", \mmlm(Col, color(flip, odds_A)), " and ",
	   \mmlm(Col, color(flip, odds_B)), " in a different configuration."  ].
	   
% 4) Multiplied odds_A and odds_B.
buggy(oddsratio2, stage(2), From, To, [step(buggy, mult, [])]) :-
    From = odds_A / odds_B,
    To = instead(bug(mult), odds_A * odds_B, odds_A / odds_B).

feedback(oddsratio2, mult, [], Col, FB) :-
    FB = [ "Divide ", \mmlm(Col, color(mult, odds_A)), " by ",
	   \mmlm(Col, color(mult, odds_B)), " instead of multiplying them." ].

hint(oddsratio2, mult, [], Col, FB) :-
    FB = [ "Do not multiply ", \mmlm(Col, color(mult, odds_A)), " and ",
	   \mmlm(Col, color(mult, odds_B)) ].
