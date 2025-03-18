% SIMPLE SETUP. DELETE AFTER DEBUGGING
:- module(debug, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(interval/interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(debug, "debug").

task(debugtask).
label(debugtask, "for debugging").

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml:math_hook(v_A, subscript(v, "A")).
mathml:math_hook(n_A, subscript(n, "A")).
mathml:math_hook(v_B, subscript(v, "B")).
mathml:math_hook(n_B, subscript(n, "B")).

% R constants
r_hook(v_A).
r_hook(n_A).
r_hook(v_B).
r_hook(n_B).
r_hook(n).
r_hook(k).

% R functions
r_hook(choose/2).
r_hook(factorial/1).
mono((choose)/2, [+, +]).
mono((factorial)/1, [+]).

render
--> { start(item(N, K)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Binomial coefficient"),
		      p(class('card-text'),
                [ "N = " , \mmlm([r(N)]), 
                ". K = " , \mmlm([r(K)])
                ])]))]).

task(debugtask)
--> { start(item(_N, _K)),
      session_data(resp(debug, debugtask, Resp), resp(debug, debugtask, '#.##'))
	},
	html(\htmlform(["What is the binomial coefficient"], debugtask, Resp)).
      

intermediate(debugtask, item).
start(item(n, k)).

% Recognized the problem
expert(debugtask, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(N, K),
    To = { choose(N, K) }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is a binomial coefficient".

% Recognized the problem
buggy(debugtask, stage(1), From, To, [step(buggy, problem1, [])]) :-
    From = item(N, K),
    To = { 1+choose(N, K) }.

feedback(problem1, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem1, [], _Col, FB)
 => FB = "This is a binomial coefficient".
 
/*  Variance
% Prettier symbols for mathematical rendering
mathml:math_hook(v_A, subscript(v, "A")).
mathml:math_hook(n_A, subscript(n, "A")).
mathml:math_hook(v_B, subscript(v, "B")).
mathml:math_hook(n_B, subscript(n, "B")).

% R constants
mcint:r_hook(v_A).
mcint:r_hook(n_A).
mcint:r_hook(v_B).
mcint:r_hook(n_B).

mcint:mono((var_pool)/4, [+, /, +, /]).
mcint:r_hook(var_pool/4).

render
--> { start(item(V_A, N_A, V_B, N_B)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Pooled Variance"),
		      p(class('card-text'),
                [ "Variance of first group = " , \mmlm([r(V_A)]), 
                ". Size of first group = " , \mmlm([r(N_A)]),
                ". Variance of second group = " , \mmlm([r(V_B)]),
                ". Size of second group = " , \mmlm([r(N_B)])
                ])]))]).

task(debugtask)
--> { start(item(_V_A, _N_A, _V_B, _N_B)),
      session_data(resp(debug, debugtask, Resp), resp(debug, debugtask, '#.##'))
	},
	html(\htmlform(["What is the pooled variance?"], debugtask, Resp)).
      

intermediate(debugtask, item).
start(item(v_A, n_A, v_B, n_B)).

% Recognized the problem
expert(debugtask, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(V_A, N_A, V_B, N_B),
    To = { var_pool(V_A, N_A, V_B, N_B) }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is an odds ratio.". */
 
/* Odds ratio
% Prettier symbols for mathematical rendering
mathml:math_hook(pi_A, subscript(pi, "A")).
mathml:math_hook(odds_A, subscript(odds, "A")).

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

task(debugtask)
--> { start(item(_Pi_A)),
      session_data(resp(debug, debugtask, Resp), resp(debug, debugtask, '#.##'))
	},
	html(\htmlform(["What is the odds?"], debugtask, Resp)).
      

intermediate(debugtask, item).
start(item(pi_A)).

% Recognized the problem
expert(debugtask, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Pi_A),
    To = { '<-'(odds_A, odds(Pi_A)) }.

feedback(problem, [], _Col, FB)
 => FB = "Correctly identified the problem and the main steps of the calculation.".

hint(problem, [], _Col, FB)
 => FB = "This is an odds ratio.".

% Determine the odds for A
intermediate(debugtask, odds).
expert(debugtask, stage(1), From, To, [step(expert, odds, [Pi_A, odds_A])]) :-
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
 */
