:- module(ztrans, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(util).
:- use_module(interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(ztrans, [i(z), "-transformation"]).
label(prob, "Probability").
label(quantile, "Quantile").

task(prob).
task(quantile).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(x, 'X').
math_hook(x_1, x).
math_hook(perc, p).

% R definitions
macro(x).
macro(x_1).
macro(sigma).
macro(z).
macro(perc).
macro(mu).

% Task description
render(Flags)
--> { start(item(X, Mu, Sigma, _Perc)) },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([digits(0) | Flags], X), " follow a normal distribution with ",
              "expectation ", \mmlm([digits(0) | Flags], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([digits(0) | Flags], [Sigma = r(sigma), "."])
            ])]))]).

% Question for the probability task
task(Flags, prob)
--> { start(item(X, _Mu, _Sigma, _Perc)),
      session_data(resp(ztrans, prob, Resp), resp(ztrans, prob, '.##'))
	},
	html(\htmlform([ "What is the probability of realizations being below ", 
         \mmlm([digits(0) | Flags], [r(X), "?"])], prob, Resp)).

% Question for the quantile task
task(Flags, quantile)
--> { start(item(_X, _Mu, _Sigma, Perc)),
      session_data(resp(ztrans, quantile, Resp), resp(ztrans, quantile, '##.#'))
	},
	html(\htmlform([ "Which quantile marks the upper ", \mmlm([digits(0) | Flags], [r(Perc), "\u0025?"])], quantile, Resp)).

start(item(x, mu, sigma, perc)).

%
% Expert rules for the probability task 
%
intermediate(prob, item).
intermediate(prob, pnorm_).
intermediate(prob, zcalc).
% First step: correctly identified the main steps of the calculation
expert(prob, stage(1), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma, _Perc),
    To = { '<-'(z, zcalc(X, Mu, Sigma)) ;
           '<-'(p, pnorm_(z)) 
         }.

feedback(allinone, [], _Col, F) 
 => F = [ "Correctly identified the main steps of the calculation."].

hint(allinone, Col, H) 
 => H = [ "Calculate the ", \nowrap([\mmlm(Col, z), "-value"]), 
          " and look up the corresponding ", \nowrap([\mmlm(Col, phi), "-value."]) 
        ].

% Second step: correctly converted to a z-value
expert(prob, stage(1), From, To, [step(expert, zcalc, [X, Mu, Sigma])]) :-
    From = zcalc(X, Mu, Sigma),
    To = dfrac(X - Mu, Sigma).

feedback(zcalc, [_X, _Mu, _Sigma], Col, F) 
 => F = [ "Correctly calculated the ", \nowrap([\mmlm(Col, z), "-value."]) ].

hint(zcalc, Col, H) 
 => H = [ "The number of realizations needs to be converted to a ", \nowrap([\mmlm(Col, z), "-value."]) ].

% Third step: selected the correct tail from the normal distribution
expert(prob, stage(2), From, To, [step(expert, correct_tail, [Z])]) :-
    From = pnorm_(Z),
    To = pnorm(Z).

feedback(correct_tail, [_Z], _Col, F) 
 => F = [ "Correctly determined the probability from the lower tail of the normal distribution." ].

hint(correct_tail, Col, H) 
 => H = [ "The result corresponds to the ", \mmlm(Col, color(wrong_tail, "wrong tail" )), " of the normal distribution." ].


%
% Buggy rules for the probability task
%
% Buggy rule: the wrong tail of the normal distribution was selected.
buggy(prob, stage(2), From, To, [step(buggy, wrong_tail, [Z])]) :-
    From = pnorm_(Z),
    To = instead(wrong_tail, 1 - pnorm(Z), pnorm(Z)).

feedback(wrong_tail, [_Z], Col, F) 
 => F = [ "The ", \mmlm(Col, color(wrong_tail, "wrong tail" )), " of the normal distribution was used." ].

hint(wrong_tail, _Col, H) 
 => H = "Do not use the upper tail of the normal distribution.".

% Buggy rule: mu and sigma were swapped.
buggy(prob, stage(1), From, To, [step(buggy, swap1, [mu, sigma])]) :-
    From = zcalc(X, mu, sigma),
    To = zcalc(X, instead(swap1, sigma, mu), instead(swap1, mu, sigma)).

feedback(swap1, [Mu, Sigma], Col, F) 
 => F = [ "The expectation ",\mmlm(Col, color(swap1, Mu)), " and the standard deviation ", 
	      \mmlm(Col, [color(swap1, Sigma), " were swapped."]) 
        ].

hint(swap1, Col, H)
 => H = [ "Use ", \mmlm(Col, color(swap1, mu)), " and ", 
          \mmlm(Col, color(swap1, sigma)), " in the correct configuration." 
        ].

% Buggy rule: standard deviation was mistaken with variance.
buggy(prob, stage(1), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = zcalc(X, Mu, sigma),
    To = zcalc(X, Mu, instead(vardev_swap, sigma^2, sigma)).

feedback(vardev_swap, [sigma], Col, F)
 => F = [ "The standard deviation ", \mmlm(Col, color(vardev_swap, sigma)), " was squared by mistake." ].

hint(vardev_swap, Col, H)
 => H = [ "Do not square ", \mmlm(Col, color(vardev_swap, sigma)), "." ].

 % Buggy rule: (x - mu)/sigma was skipped.
buggy(prob, stage(1), From, To, [step(buggy, xp, [X]), depends(xp2)]) :-
   From = zcalc(X, Mu, Sigma),
   To = omit_right(xp, dfrac(omit_right(xp, X - Mu), Sigma)).

feedback(xp, [X], Col, F)
 => F = [ \mmlm(Col, X), " was not converted to the ", \nowrap([\mmlm(Col, z), "-value."]) ].

hint(xp, Col, H)
 => H = [ "Remember to calculate the ", \nowrap([\mmlm(Col, z), "-value."]) ].

% Buggy rule: x/100 was taken to be phi(z).
buggy(prob, stage(2), From, To, [step(buggy, xp2, []), depends(xp)]) :-
    From = pnorm_(z),
    To = instead(xp2, z/100, pnorm(z)).

feedback(xp2, [], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, z), "-value"]), " was mistakenly divided by 100." ].

hint(xp2, Col, H)
 => H = [ "Do not divide the ", \nowrap([\mmlm(Col, z), "-value"]), 
          " by 100, use the normal distribution instead." 
        ].

% 
% Expert rules for the quantile task
%
% First step: identify the main steps of the calculation
intermediate(quantile, item).
intermediate(quantile, qnorm_).
intermediate(quantile, xcalc).
expert(quantile, stage(1), From, To, [step(expert, steps, [])]) :-
    From = item(_X, Mu, Sigma, Perc),
    To = { '<-'(z, qnorm_(Perc / 100)) ;
           '<-'(x_1, xcalc(z, Sigma, Mu))
         }.

feedback(steps, [], _Col, F)
 => F = [ "Correctly identified the main steps of the calculation." ].

hint(steps, Col, H)
 => H = [ "First determine the ", \nowrap([\mmlm(Col, z), "-statistic,"]), " then translate ",
           "it to the original scale." 
        ].

% Second step: determine the quantile from the normal distribution
expert(quantile, stage(2), From, To, [step(expert, correct_tail, [])]) :-
    From = qnorm_(P),
    To = qnorm(1 - P).

feedback(correct_tail, [], Col, F)
 => F = [ "Correctly determined the ", \nowrap([\mmlm(Col, z), "-value"]), " from the lower tail of the normal distribution." ].

hint(correct_tail, Col, H)
 => H = [ "The ", \nowrap([\mmlm(Col, z), "-value"]), " from the normal distribution is needed." ].

% Third step: convert the quantile to the original scale
expert(quantile, stage(3), From, To, [step(expert, xcalc, [])]) :-
    From = xcalc(Z, Sigma, Mu),
    To = dot(Z, Sigma) + Mu.

feedback(xcalc, [], Col, F)
 => F = [ "Correctly converted the ", \nowrap([\mmlm(Col, z), "-value"]), " to the original scale." ].

hint(xcalc, Col, H)
 => H = [ "The ", \nowrap([\mmlm(Col, z), "-value"]), " needs to be converted to the original scale using the expectation ",
          \mmlm(Col, mu), " and the standard deviation ", \mmlm(Col, sigma) 
        ].

%
% Buggy rules for the quantile task
%
% Buggy rule: use upper tail instead of lower tail
buggy(quantile, stage(2), From, To, [step(buggy, lowertail, [])]) :-
    From = qnorm_(P),
    To = qnorm(instead(lowertail, P, 1 - P)).

feedback(lowertail, [], Col, F)
 => F = [ "The ", \mmlm(Col, color(lowertail, "lower tail")), " of the normal distribution was used instead of the upper tail." ].

hint(lowertail, _Col, H)
 => H = [ "Do not select the upper tail of the normal distribution." ].


% Buggy rule: Mu and Sigma were swapped.
buggy(quantile, stage(3), From, To, [step(buggy, swap2, [mu, Sigma])]) :-
    From = xcalc(Z, Sigma, Mu),
    Mu0 = instead(swap2, Mu, Sigma),
    Sigma0 = instead(swap2, Sigma, Mu),
    To = dot(Z, Mu0) + Sigma0. 

feedback(swap2, [Mu, Sigma], Col, F)
 => F = [ "The expectation ", \mmlm(Col, color(swap2, Mu)), " and standard deviation ",
	      \mmlm(Col, [color(swap2, Sigma), " were swapped."]) 
        ].

hint(swap2, Col, H)
 => H = [ "Use the expectation ", \mmlm(Col, color(swap2, mu)), " and the standard deviation ", 
          \mmlm(Col, color(swap2, sigma)), " in a different configuration." 
        ].


% Buggy rule: standard deviation was mistaken with variance.
buggy(quantile, stage(3), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = xcalc(Z, Sigma, Mu),
    To = dot(Z, add_right(vardev_swap, Sigma^2)) + Mu.

feedback(vardev_swap, [Sigma], Col, F)
 => F = [ "The standard deviation ", \mmlm(Col, color(vardev_swap, Sigma)), " was squared." ].

hint(vardev_swap, _Col, H)
 => H = "Use the standard deviation instead of the variance.".


% Buggy rule: divide percentage by 1000 instead of 100 to translate to proportion
% Probably due to confusion with the omitted 0 in p-values like .05
buggy(quantile, stage(2), From, To, [step(buggy, perc1000, [1000])]) :-
    From = Perc / 100,
    To = Perc / instead(perc1000, 1000, 100).

feedback(perc1000, [Div], Col, F)
 => F = [ "The percentage was mistakenly divided by " ,
          \mmlm(Col, color(perc1000, Div)), " to obtain a proportion."
        ].

hint(perc1000, _Col, H)
 => H = "Do not divide by 1000 to translate a percentage to a proportion.".


% Buggy rule: divide percentage by 10 instead of 100 to translate to proportion
buggy(quantile, stage(1), From, To, [step(buggy, perc10, [10])]) :-
    From = qnorm_(Perc / 100),
    To = qnorm_(Perc / instead(perc10, 10, 100)).

feedback(perc10, [Div], Col, F)
 => F = [ "The percentage was mistakenly divided by " ,
          \mmlm(Col, color(perc10, Div)), " to obtain a proportion." 
        ].

hint(perc10, _Col, H)
 => H = "Do not divide by 10 to translate a percentage to a proportion.".


% Buggy rule: z-value taken as result
buggy(quantile, stage(3), From, To, [step(buggy, only_z, [z])]) :-
    From = xcalc(Z, Sigma, Mu),
    Formula = dot(Z, Sigma) + Mu,
    To = instead(only_z, z , Formula).

feedback(only_z, [z], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, color(only_z, z)), "-value"]), " was mistakenly taken as the result." ].

hint(only_z, Col, H)
 => H = [ "Translate the ", \nowrap([\mmlm(Col, z), "-value"]),  
          " back to the original scale." 
        ].
