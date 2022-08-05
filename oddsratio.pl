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
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "Compare the effectiveness of ",
		         "two therapies. Therapy A has a probability of success of ",
		          \mmlm(Pi_A = r(pi_A)), ". The Odds Ratio is ",
		          \mmlm(OR = r(or)), " in favor of Therapy A." 
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

% Odds ratio with two probabilities.
% sr_a = sucess rate of a; or = odds ratio; 
intermediate(item).
intermediate(odds_A_).
intermediate(odds_B_).
start(item(pi_A, or)).

expert(stage(1), From, To, [step(expert, odd, [])]) :-
    From = item(Pi_A, OR),
    To = { '<-'(odds_A, odds_A_(Pi_A)) ;
           '<-'(odds_B, odds_B_(odds_A, OR)) ; 
           '<-'(pi_B, dfrac(odds_B, 1 + odds_B)) ; 
          pi_B 
        }.

feedback(odd, [], Col, FB) =>
    FB = [ "Correctly recognised the problem as an ", \mmlm(Col, hyph(odds, "ratio")), 
	   " and identified the main steps of the calculation."].

hint(odd, [], Col, FB) =>
    FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

% Correctly calculated the odds for A.
expert(stage(1), From, To, [step(expert, oddsa, [Pi_A])]) :-
    From = odds_A_(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(oddsa, [_], Col, FB) =>
    FB = ["Correctly determined  the ", \mmlm(Col, odds_A)].

hint(oddsa, [Pi_A], Col, FB) =>
    FB = ["The first step should be converting ", \mmlm(Col, Pi_A), " to ", \mmlm(Col, odds_A), 
	  " with ", \mmlm(Col, odds_A = dfrac(Pi_A, 1 - Pi_A))].

% Calculated odds_B.
expert(stage(2), From, To, [step(expert, oddsb, [To])]) :-
    From = odds_B_(Odds_A, OR),
    To = Odds_A * OR.

feedback(oddsb, [_], Col, FB) =>
    FB = ["Sucessfully calculated ", \mmlm(Col, odds_B)].

hint(oddsb, [To], Col, FB) =>
    FB = ["The formula for ", \mmlm(Col, odds_B), " is ", \mmlm(Col, odds_B = To)].


% Forgot conversion  of pi_a to odds.
buggy(stage(2), From, To, [step(buggy, cona, [Pi_A])]) :-
    From = dfrac(Pi_A, (1 - Pi_A)),
    To = omit_right(cona, dfrac(Pi_A, 1 - Pi_A)).

feedback(cona, [Pi_A], Col, FB) =>
    FB = [ "Please remember to convert ", \mmlm(Col, color(cona, Pi_A)), " to ",
	   \mmlm(Col, color(cona, odds_A)), ", with ", \mmlm(Col, color(cona, odds_A = frac(Pi_A, 1 - Pi_A)))  ].

hint(cona, [Pi_A], Col, FB) =>
    FB = [ "You should try converting ", \mmlm(Col, color(cona, Pi_A)), 
	   " to odds before continuing." ].

% Forgot to multiply odds_a and or.
buggy(stage(2), From, To, [step(buggy, mult, [OR])]) :-
    From = odds_A * OR,
    To = omit_right(mult, odds_A * OR).

feedback(mult, [OR], Col, FB) =>
    FB = [ "You forgot to multiply ", \mmlm(Col, color(mult, odds_A)), " with ",
	   \mmlm(Col, color(mult, OR)) ].

hint(mult, [OR], Col, FB) =>
    FB = [ "Please remember to multiply ",\mmlm(Col, color(mult, odds_A)), " with ",
	   \mmlm(Col, color(mult, OR)) ].

% Divided odds_A and or rather then multiplying them.
buggy(stage(2), From, To, [step(buggy, divi, [OR])]) :-
    From = odds_A * OR,
    To = instead(divi, odds_A / OR, odds_A * OR).

feedback(divi, [OR], Col, FB) =>
    FB = [ "You divided ", \mmlm(Col, color(divi, odds_A)), " by ", 
	   \mmlm(Col, color(divi, OR)), " rather than multiplying them." ].

hint(divi, [OR], Col, FB) =>
    FB = [ "Please remember to multiply ", \mmlm(Col, color(divi, odds_A)), " with ",
	   \mmlm(Col, color(divi, OR)), " rather then dividing them." ].

% Forgot to convert odds_B to pi_B.
buggy(stage(2), From, To, [step(buggy, nopi, [])]) :-
    From = dfrac(odds_B, 1 + odds_B),
    To = omit_right(nopi, dfrac(odds_B, 1 + odds_B)).

feedback(nopi, [], Col, FB) =>
    FB = [ "You forgot to convert ", 
	   \mmlm(Col, color(nopi, odds_B)),
           " back into a probability ", \mmlm(Col, color(nopi, pi_B)), 
	   ", with ", \mmlm(Col, color(nopi, pi_B = frac(odds_B, 1 + odds_B))) ].

hint(nopi, [], Col, FB) =>
    FB = [ "Remember that your end result should be a probability ",
	   "rather than ", \mmlm(Col, color(nopi, odds_B)), "." ].
