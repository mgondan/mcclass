:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

% Prettier symbols for mathematical rendering
mathml:hook(Flags, pi_A, [task(oddsratio) | Flags], sub(pi, "A")).
mathml:hook(Flags, odds_A, [task(oddsratio) | Flags], sub(odds, "A")).
mathml:hook(Flags, pi_B, [task(oddsratio) | Flags], sub(pi, "B")).
mathml:hook(Flags, odds_B, [task(oddsratio) | Flags], sub(odds, "B")).
mathml:hook(Flags, or, [task(oddsratio) | Flags], 'OR').

% R constants
interval:r_hook(odds_A).
interval:r_hook(pi_A).
interval:r_hook(pi_B).
interval:r_hook(or).
interval:r_hook(odds_B).

render(oddsratio, item(Pi_A, OR), Form) -->
	{ option(resp(R), Form, '#.##') },
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "Compare the effectiveness of ",
		         "two therapies. Therapy A has a probability of success of ",
		          \mmlm([task(oddsratio)], Pi_A = r(pi_A)), ". The Odds Ratio is ",
		          \mmlm([task(oddsratio)], OR = r(or)), " in favor of Therapy A." 
		       ])
		    ])),
	          div(class(card), div(class('card-body'),
		     [ h4(class('card-title'), [a(id(question), []), "Question"]),
		       p(class('card-text'),
		        [ "What is the probability of sucess of treatment B?"
		        ]),
		       form([class(form), method('POST'), action('#oddsratio-reverse')],
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
% sr_a = sucess rate of a; or = odds ratio; 
intermediate(_, item).
start(oddsratio, item(pi_A, or)).

expert(oddsratio, stage(2), X, Y, [step(expert, odd, [])]) :-
	X = item(Pi_A, OR),
	Y = { '<-'(odds_A, dfrac(Pi_A, 1 - Pi_A)) ;
          '<-'(odds_B, odds_A * OR) ; 
          '<-'(pi_B, dfrac(odds_B, 1 + odds_B)) ; 
          pi_B 
        }.

feedback(oddsratio, odd, [], Col, FB) =>
	FB = [ "You correctly recognised the problem as an ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(oddsratio, odd, [], Col, FB) =>
        FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

% Forgot conversion  of pi_a to odds.
buggy(oddsratio, stage(2), X, Y, [step(buggy, cona, [Pi_A])]) :-
	X = dfrac(Pi_A, (1 - Pi_A)),
	Y = omit_right(bug(cona), dfrac(Pi_A, 1 - Pi_A)).

feedback(oddsratio, cona, [Pi_A], Col, FB) =>
	FB = [ "Please remember to convert ", \mmlm(Col, color(cona, Pi_A)), " to ",
	\mmlm(Col, color(cona, odds_A)), ", with ", \mmlm(Col, color(cona, odds_A = frac(Pi_A, 1 - Pi_A)))  ].

hint(oddsratio, cona, [Pi_A], Col, FB) =>
	FB = [ "You should try converting ", \mmlm(Col, color(cona, Pi_A)), 
	       " to odds before continuing." ].

% Forgot to multiply odds_a and or.
buggy(oddsratio, stage(2), X, Y, [step(buggy, mult, [OR])]) :-
	X = odds_A * OR,
	Y = omit_right(bug(mult), odds_A * OR).

feedback(oddsratio, mult, [OR], Col, FB) =>
	FB = [ "You forgot to multiply ", \mmlm(Col, color(mult, odds_A)), " with ",
	       \mmlm(Col, color(mult, OR)) ].

hint(oddsratio, mult, [OR], Col, FB) =>
       	FB = [ "Please remember to multiply ",\mmlm(Col, color(mult, odds_A)), " with ",
	       \mmlm(Col, color(mult, OR)) ].

% Divided odds_A and or rather then multiplying them.
buggy(oddsratio, stage(2), From, To, [step(buggy, divi, [OR])]) :-
    From = odds_A * OR,
    To = instead(bug(divi), odds_A / OR, odds_A * OR).

feedback(oddsratio, divi, [OR], Col, FB) =>
    FB = [ "You divided ", \mmlm(Col, color(divi, odds_A)), " by ", 
	   \mmlm(Col, color(divi, OR)), " rather than multiplying them." ].

hint(oddsratio, divi, [OR], Col, FB) =>
    FB = [ "Please remember to multiply ", \mmlm(Col, color(divi, odds_A)), " with ",
	   \mmlm(Col, color(divi, OR)), " rather then dividing them." ].

% Forgot to convert odds_B to pi_B.
buggy(oddsratio, stage(2), From, To, [step(buggy, nopi, [])]) :-
    From = dfrac(odds_B, 1 + odds_B),
    To = omit_right(bug(nopi), dfrac(odds_B, 1 + odds_B)).

feedback(oddsratio, nopi, [], Col, FB) =>
    FB = [ "You forgot to convert ", 
	   \mmlm(Col, color(nopi, odds_B)),
           " back into a probability ", \mmlm(Col, color(nopi, pi_B)), 
	   ", with ", \mmlm(Col, color(nopi, pi_B = frac(odds_B, 1 + odds_B))) ].

hint(oddsratio, nopi, [], Col, FB) =>
    FB = [ "Remember that your end result should be a probability ",
	   "rather than ", \mmlm(Col, color(nopi, odds_B)), "." ].
