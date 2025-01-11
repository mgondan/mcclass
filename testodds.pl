:- module(testodds, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_mcclass).
:- use_module('/home/jeremyirilli/interval/prolog/mcclass.pl').
:- use_module(mathml).

:- use_module(navbar).
navbar:page(testodds, "testodds").
task(oddstask).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(pi_A, subscript(pi, "A")).
math_hook(odds_A, subscript(odds, "A")).

% R constants
rint:r_hook(pi_A).

render
--> { start(item(Pi_A)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Testing odds"),
		      p(class('card-text'),
                [ "The success probability ", "is ", \mmlm([r(Pi_A), "."])
                ])]))]).

task(oddstask)
--> { start(item(_Pi_A)),
      session_data(resp(testodds, oddstask, Resp), resp(testodds, oddstask, '#.##'))
	},
	html(\htmlform(["What is the odds?"], oddstask, Resp)).
      

intermediate(oddstask, item).
start(item(pi_A)).

% Recognized the problem
expert(oddstask, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A),
    To = { '<-'(odds_A, odds(Pi_A)) }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is an odds ratio.".

% Determine the odds for A
intermediate(oddstask, odds).
expert(oddstask, stage(1), From, To, [step(expert, odds, [Pi_A, odds_A])]) :-
    From = odds(Pi_A),
    To = dfrac(Pi_A, 1 - Pi_A).

feedback(odds, [Pi_A, _], Col, FB)
 => FB = [ "Correctly determined the odds ",
           "from ", \mmlm(Col, [Pi_A, "."])
         ].

hint(odds, [Pi_A, Odds_A], Col, FB)
 => FB = [ "Convert the success probability of therapy A, ", 
            \mmlm(Col, [Pi_A, ","]), " to the respective ",
            "odds, ", \mmlm(Col, [Odds_A = dfrac(Pi_A, 1 - Pi_A), "."])
         ].
