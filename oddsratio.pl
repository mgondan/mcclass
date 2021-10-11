%:- module(oddsratio, 
%       	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(oddsratio) :-
	r_init,

    {|r||
        pi_A <- runif(1, min=0.25, max=0.85)
        or <- runif(1, min=0.5, max=6)

        odds <- function(p)
        {
          p / (1 - p)
        }

        prob <- function(odds)
        {
          odds / (1 + odds)
        }
    |}.

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, pi_A, Flags, sub(pi, "A")).
mathml:hook(Flags, odds_A, Flags, sub(odds, "A")).
mathml:hook(Flags, pi_B, Flags, sub(pi, "B")).
mathml:hook(Flags, odds_B, Flags, sub(odds, "B")).
mathml:hook(Flags, or, Flags, 'OR').

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
	R <- Expr,
	[Res] = R,
	number(Res).

render(oddsratio, item(Pi_A, OR), Form) -->
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

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% Odds ratio with two probabilities.
% sr_a = sucess rate of a; or = odds ratio; 
intermediate(_, item).
start(oddsratio, item(pi_A, or)) :-
	init(oddsratio).

expert(oddsratio, stage(2), X, Y, [step(expert, odd, [])]) :-
	X = item(Pi_A, OR),
	Y = { odds_A = frac(Pi_A, 1 - Pi_A) ;
          odds_B = odds_A * OR ; 
          pi_B = frac(odds_B, 1 + odds_B) ; 
          pi_B 
        }.

feedback(oddsratio, odd, [], Col, FB) :-
	FB = [ "Correctly recognised the problem as an ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(oddsratio, odd, [], Col, FB) :-
        FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].

