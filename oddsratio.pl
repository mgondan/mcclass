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

intermediate(oddsratio, odd).
expert(oddsratio, stage(2), X, Y, [step(expert, odd, [])]) :-
	X = item(Pi_A, OR),
	Y = odd(Pi_A, OR).

feedback(oddsratio, odd, [], Col, FB) :-
	FB = [ "Correctly recognised the problem as a ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(oddsratio, odd, [], Col, FB) :-
        FB = [ "This is a ", \mmlm(Col, hyph(odds, "ratio")), "."].

% Turn percentage into decimal.
intermediate(oddsratio, odds_1).
expert(oddsratio, stage(2), X, Y, [step(expert, odds_1, [Pi_A])]) :-
	X = odd(Pi_A, OR),
	Y = odds_1(abbrev(odds_A, (Pi_A / (1 - Pi_A)), "'the odds for A'"), OR).

feedback(oddsratio, odds_1, [Pi_A], Col, FB) :-
	FB = [ "Correctly converted ", \mmlm(Col, Pi_A), " to odds." ].

hint(oddsratio, odds_1, [Pi_A], Col, FB) :-
	FB = [ "The formula to convert ", \mmlm(Col, Pi_A), " to odds is ", 
	       \mmlm(Col, Pi_A / (1 - Pi_A)) ].

% Calculate decimal for sucess rate of b.
intermediate(oddsratio, odds_2).
expert(oddsratio, stage(2), X, Y, [step(expert, odds_2, [Odds_A, OR])]) :-
	X = odds_1(Odds_A, OR),
	Y = odds_2(abbrev(odds_B, (Odds_A / OR), "'the odds for B'")).

feedback(oddsratio, odds_2, [_Odds_A, _OR], Col, FB) :-
	FB = [ "Correctly calculated the odds for sucess with treatment ", 
	       \mmlm(Col, b)].

hint(oddsratio, odds_2, [Odds_A, OR], Col, FB) :-
	FB = [ "The odds for success with treatment ",
	       \mmlm(Col, b), " are ", \mmlm(Col, (Odds_A / OR)) ].

% Turn decimal into Sucess rate of b.
expert(oddsratio, stage(2), X, Y, [step(expert, pi_B, [Odds_B])]) :-
	X = odds_2(Odds_B),
	Y = dfrac(Odds_B, (1 + Odds_B)).

feedback(oddsratio, pi_B, [_Odds_B], Col, FB) :-
	FB = [ "Correctly calculated the probability for success with treatment ", 
	       \mmlm(Col, b), " and converted it to percent." ].

hint(oddsratio, pi_B, [Odds_B], Col, FB) :-
	FB = [ " The success-rate for treatment ", \mmlm(Col, b), " is ",
	       \mmlm(Col, dfrac(Odds_B, (1 + Odds_B))) ].

% Forgot decimal conversion of sr_a.
buggy(oddsratio, stage(2), X, Y, [step(buggy, cona, [Pi_A])]) :-
	X = odd(Pi_A, OR),
	Y = odds_1(abbrev(odds_A, (omit_right(bug(cona), Pi_A / (1 - Pi_A))), "'the wrong odds for A'"), OR).

feedback(oddsratio, cona, [Pi_A], Col, FB) :-
	FB = [ "Please remember to convert ", \mmlm(Col, color(cona, Pi_A)), " to ",
	       "odds." ].

hint(oddsratio, cona, [Pi_A], Col, FB) :-
	FB = [ "You should concider converting ", \mmlm(Col, color(cona, Pi_A)), 
	       " to odds before continuing." ].

% Forgot to divide o_a and or.
buggy(oddsratio, stage(2), X, Y, [step(buggy, divi, [Odds_A, OR])]) :-
	X = odds_1(Odds_A, OR),
	Y = odds_2(abbrev(odds_B, (omit_right(bug(divi), Odds_A / OR)), "'the wrong odds for B'")).

feedback(oddsratio, divi, [Odds_A, OR], Col, FB) :-
	FB = [ "It appears you forgot to divide ", \mmlm(Col, color(divi, Odds_A)), " by ",
	       \mmlm(Col, color(divi, OR)) ].

hint(oddsratio, divi, [Odds_A, OR], Col, FB) :-
	FB = [ "Do not forget to divide ",\mmlm(Col, color(divi, Odds_A)), " by ",
	       \mmlm(Col, color(divi, OR)) ].

% Multiplied O_A and OR rather than deviding them.
buggy(oddsratio, stage(2), X, Y, [step(buggy, mult, [Odds_A, OR])]) :-
	X = odds_1(Odds_A, OR),
	Y = odds_2(abbrev(odds_B, (instead(bug(mult), (Odds_A * OR), (Odds_A / OR))), "'the wrong odds for B'")).

feedback(oddsratio, mult, [Odds_A, OR], Col, FB) :-
	FB = [ "It seems you multiplied ", \mmlm(Col, color(mult, Odds_A)), " with ", 
	       \mmlm(Col, color(mult, OR)), " rather than dividing them." ].

hint(oddsratio, mult, [Odds_A, OR], Col, FB) :-
	FB = [ "If I were you I would divide ", \mmlm(Col, color(divi, Odds_A)), " by ",
	       \mmlm(Col, color(divi, OR)), " rather then multiplying them." ].

% Forgot to convert Odds for B to Probability.
buggy(oddsratio, stage(2), X, Y, [step(buggy, nopi, [Odds_B])]) :-
 	X = odds_2(Odds_B),
	Y = instead(bug(nopi), Odds_B, dfrac(Odds_B, (1 + Odds_B))).

feedback(oddsratio, nopi, [Odds_B], Col, FB) :-
	FB = [ "It looks like you forgot to convert ", \mmlm(Col, color(nopi, Odds_B)), 
	       " back into a probability." ].

hint(oddsratio, nopi, [Odds_B], Col, FB) :-
	FB = [ "Remember that your end result should be a probabitly ",
    	       "rather than the ", \mmlm(Col, color(nopi, Odds_B)) ].
