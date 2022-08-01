:- module(oddsratio2, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

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

render(item(Pi_A, Pi_B), Form) -->
  { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Clinical Study"),
          p(class('card-text'),
            [ "Compare the effectiveness of ",
              "two therapies. Therapy A has a probability of success of ",
              \mmlm(Pi_A = r(pi_A)), ". Therapy B ",
              "one of ", \mmlm(Pi_B = r(pi_B))
            ])
        ])),
	div(class(card), div(class('card-body'),
	  [ h4(class('card-title'), [a(id(question), []), "Question"]),
	    p(class('card-text'),
	      "What is the Odds Ratio in favor of treatment A?"),
            form([class(form), method('POST'), action('#oddsratio2-reverse')],
              [ div(class("input-group mb-3"),
                [ div(class("input-group-prepend"), 
                    span(class("input-group-text"), "Response")),
                  input([class("form-control"), type(text), name(resp), value(R)]),
                  div(class("input-group-append"),
	            button([class('btn btn-primary'), type(submit)], "Submit"))
	        ])
              ])
	  ]))
      ]).

% Odds ratio with two probabilities. 
intermediate(item).
intermediate(odds_A_).
intermediate(odds_B_).
start(item(pi_A, pi_B)).

expert(stage(1), From, To, [step(expert, odd, [])]) :-
    From = item(Pi_A, Pi_B),
    To = { '<-'(odds_A, odds_A_(Pi_A)) ;
	   '<-'(odds_B, odds_B_(Pi_B)) ; 
	   '<-'(or, odds_A / odds_B) ; 
	   or
	 }.

feedback(odd, [], Col, FB) =>
    FB = [ "Correctly recognised the problem as an ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(odd, [], Col, FB) =>
    FB = [ "This is an ", \mmlm(Col, hyph(odds, "ratio")), "."].


% correctly identified odds_A.
expert(stage(1), From, To, [step(expert, oddsa, [Pi_A])]) :-
    From = odds_A_(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(oddsa, [_], Col, FB) =>
    FB = ["Correctly determined ", \mmlm(Col, odds_A)].

hint(oddsa, [Pi_A], _Col, FB) =>
    FB = ["The first step should be converting ", \mmlm(Col, Pi_A), " to ", \mmlm(Col, odds_A), 
	  " with ", \mmlm(Col, odds_A = dfrac(Pi_A, 1 - Pi_A))].

% correctly calculated odds_B.
expert(stage(2), From, To, [step(expert, oddsb, [To])]) :-
    From = odds_B_(Pi_B),
    To = dfrac(Pi_B, 1 - Pi_B).

feedback(oddsb, [_], Col, FB) =>
    FB = ["Sucessfully calculated ", \mmlm(Col, odds_B)].

hint(oddsb, [To], Col, FB) =>
    FB = ["Calculate ", \mmlm(Col, oddds_B), " using ", \mmlm(Col, odds_B = To)].

% 1) Forgot conversion  of pi_a to odds.
buggy(stage(2), From, To, [step(buggy, cona, [pi_A]), depends(conb)]) :-
    From = dfrac(pi_A, (1 - pi_A)),
    To = omit_right(cona, dfrac(pi_A, 1 - pi_A)).

feedback(cona, [pi_A], Col, FB) =>
    FB = [ "Please remember to convert ", \mmlm(Col, color(cona, pi_A)), " to ",
	   \mmlm(Col, color(cona, odds_A)), ", with ", \mmlm(Col, color(cona, odds_A = frac(pi_A, 1 - pi_A)))  ].

hint(cona, [pi_A], Col, FB) =>
    FB = [ "Do not forget to convert ", \mmlm(Col, color(cona, pi_A)), " and ",
       \mmlm(Col, color(cona, pi_B)), " to odds before continuing." ].

% 2) Forgot conversion  of pi_b to odds.
buggy(stage(2), From, To, [step(buggy, conb, [pi_B]), depends(cona)]) :-
    From = dfrac(pi_B, (1 - pi_B)),
    To = omit_right(conb, dfrac(pi_B, 1 - pi_B)).

feedback(conb, [pi_B], Col, FB) =>
    FB = [ "Please remember to convert ", \mmlm(Col, color(conb, pi_B)), " to ",
	   \mmlm(Col, color(conb, odds_B)), ", with ", \mmlm(Col, color(conb, odds_B = frac(pi_B, 1 - pi_B)))  ].

hint(conb, [pi_B], Col, FB) =>
    FB = [ "Do not forget to convert ", \mmlm(Col, color(conb, pi_A)), " and ",
	   \mmlm(Col, color(conb, pi_B)), " to odds before continuing." ].

% 3) Flipped odds_A and odds_B.
buggy(stage(2), From, To, [step(buggy, flip, [])]) :-
    From = odds_A / odds_B,
    To = instead(flip, odds_B / odds_A, odds_A / odds_B).

feedback(flip, [], Col, FB) =>
    FB = [ "Divide ", \mmlm(Col, color(flip, odds_A)), " by ",
	   \mmlm(Col, color(flip, odds_B)), " instead of ", 
	   \mmlm(Col, color(flip, odds_B / odds_A)) ].

hint(flip, [], Col, FB) =>
    FB = [ "Try using ", \mmlm(Col, color(flip, odds_A)), " and ",
	   \mmlm(Col, color(flip, odds_B)), " in a different configuration."  ].
	   
% 4) Multiplied odds_A and odds_B.
buggy(stage(2), From, To, [step(buggy, mult, [])]) :-
    From = odds_A / odds_B,
    To = instead(mult, odds_A * odds_B, odds_A / odds_B).

feedback(mult, [], Col, FB) =>
    FB = [ "Divide ", \mmlm(Col, color(mult, odds_A)), " by ",
	   \mmlm(Col, color(mult, odds_B)), " instead of multiplying them." ].

hint(mult, [], Col, FB) =>
    FB = [ "Do not multiply ", \mmlm(Col, color(mult, odds_A)), " and ",
	   \mmlm(Col, color(mult, odds_B)) ].
