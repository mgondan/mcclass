%:- module(easyodds, 
%       	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(easyodds) :-
    r_session_source(easyodds).

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, pi_A, Flags, sub(pi, "A")).
mathml:hook(Flags, odds_A, Flags, sub(odds, "A")).
mathml:hook(Flags, pi_B, Flags, sub(pi, "B")).
mathml:hook(Flags, or, Flags, 'OR').

%
% R constants
%
interval:hook(pl, odds_A, r(odds_A)).
interval:hook(pl, pi_A, r(pi_A)).
interval:hook(pl, pi_B, r(pi_B)).
interval:hook(pl, or, r(or)).

render(easyodds, item(_Odds_A, _Pi_B, _OR), Form) -->
	{ option(resp(R), Form, '#.##') },
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "The Odds for sucess with treatment A are ", 
			 \mmlm([r(odds_A), ","]), " treatment B has a probability of success of ",
			 \mmlm(r(pi_B)), " and the Odds Ratio between both treatments is ",
		         \mmlm([r(or), "."])
		       ])
		    ])),
	          div(class(card), div(class('card-body'),
		     [ h4(class('card-title'), [a(id(question), []), "Question"]),
		       p(class('card-text'),
		        [ "What is the probability of sucess with treatment A?"
		        ]),
		       form([class(form), method('POST'), action('#easyodds-reverse')],
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
start(easyodds, item(odds_A, pi_b, or)) :-
    init(easyodds).

expert(easyodds, stage(2), From, To, [step(expert, odd, [])]) :-
    From = item(Odds_A, _Pi_B, _OR),
    To = { '<-'(pi_A, dfrac(Odds_A, 1 + Odds_A)) ;
	   pi_A
	 }.

feedback(easyodds, odd, [], Col, FB) :-
    FB = [ "Correctly recognised the problem as an ", 
           \mmlm(Col, hyph(odds, "ratio")), "."].

hint(easyodds, odd, [], Col, FB) :-
    FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

% 1) Tried conversion from odds to odds, as if starting with 
%    a probability.
buggy(easyodds, stage(2), From, To, [step(buggy, sub, [Odds_A])]) :-
    From = 1 + Odds_A,
    To = instead(bug(sub), 1 - Odds_A, 1 + Odds_A).

feedback(easyodds, sub, [Odds_A], Col, FB) :-
    FB = [ "Please use the formula converting, ", \mmlm(Col, color(sub, Odds_A)), " to ", 
	   \mmlm(Col, color(sub, pi_A)) ].

hint(easyodds, sub, [Odds_A], Col, FB) :-
    FB = [ "Do not try to further convert ", \mmlm(Col, color(sub, Odds_A)), " to odds." ].
