:- module(ztrans, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(util).
:- use_module(interval/interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(ztrans, [i(z), "-transformation"]).
label(prob, "Probability").
label(quantile, "Quantile").

task(prob).
task(quantile).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(x, 'X').
math_hook(perc, p).

% R definitions
r_hook(x).
r_hook(sigma).
r_hook(z).
r_hook(perc).

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

% Question for the probability
task(Flags, prob)
--> { start(item(X, _Mu, _Sigma, _Perc)),
      session_data(resp(ztrans, prob, Resp), resp(ztrans, prob, '.##'))
	},
	html(\htmlform([ "What is the probability of having realizations below ", 
         \mmlm([digits(0) | Flags], [r(X), "?"])], prob, Resp)).

% Question for the quantile
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
 => F = [ "You correctly identified the main steps of the calculation."].

hint(allinone, [], Col, H) 
 => H = [ "Calculate the ", \nowrap([\mmlm(Col, z), "-value"]), 
          " and look up the corresponding ", \nowrap([\mmlm(Col, phi), "-value."]) 
        ].

% Second step: correctly converted to a z-value
expert(prob, stage(1), From, To, [step(expert, zcalc, [X, Mu, Sigma])]) :-
    From = zcalc(X, Mu, Sigma),
    To = dfrac(X - Mu, Sigma).

feedback(zcalc, [_X, _Mu, _Sigma], Col, F) 
 => F = [ "You correctly calculated the ", \nowrap([\mmlm(Col, z), "-value."]) ].

hint(zcalc, [X, Mu, Sigma], Col, H) 
 => H = [ "To calculate the ", \nowrap([\mmlm(Col, z), "-value"]), 
          ", use " , \mmlm(Col, [dfrac(X - Mu, Sigma), "."]) 
        ].

% Third step: selected the correct tail from the normal distribution
expert(prob, stage(2), From, To, [step(expert, correct_tail, [Z])]) :-
    From = pnorm_(Z),
    To = pnorm(Z).

feedback(correct_tail, [_Z], _Col, F) 
 => F = [ "You calculated the correct tail of the distribution." ].

hint(correct_tail, [_Z], Col, H) 
 => H = [ "Use the lower tail of the normal distribution and select the value corresponding to the ",
          \nowrap([\mmlm(Col, z), "-value."])
        ].


%
% Buggy rules for the probability task
%
% Buggy rule: the wrong tail of the normal distribution was selected.
buggy(prob, stage(2), From, To, [step(buggy, wrong_tail, [Z])]) :-
    From = pnorm_(Z),
    To = instead(wrong_tail, 1 - pnorm(Z), pnorm(Z)).

feedback(wrong_tail, [_Z], Col, F) 
 => F = [ "Your answer matches the ", \mmlm(Col, color(wrong_tail, "wrong tail" )), " of the normal distribution." ].

hint(wrong_tail, [_Z], _Col, H) 
 => H = [ "Do not use the upper tail of the normal distribution." ].

% Buggy rule: mu was added to x, not subtracted.
buggy(prob, stage(2), From, To, [step(buggy, plus, [X, Mu])]) :-
    From = dfrac(X - Mu, Sigma),
    To = dfrac(instead(plus, X + Mu, X - Mu), Sigma).

feedback(plus, [X, Mu], Col, F) 
 => F = [ "Subtract ", \mmlm(Col, color(plus, Mu)), " from ", \mmlm(Col, color(plus, X)),
          " instead of adding them up." 
        ].

hint(plus, [X, Mu], Col, H) 
 => H = [ "Try using subtraction rather than addition in ", 
            \mmlm(Col, [color(plus, X + Mu), "."]) 
        ].

% Buggy rule: mu and sigma were swapped.
buggy(prob, stage(1), From, To, [step(buggy, swap1, [mu, sigma])]) :-
    From = item(x, mu, sigma, Perc),
    To = item(x, instead(swap1, sigma, mu), instead(swap1, mu, sigma), Perc);
    From = item(x, mu, sigma^2, Perc),
    To = item(x, instead(swap1, sigma^2, mu), instead(swap1, mu, sigma), Perc).

feedback(swap1, [Mu, Sigma], Col, F) 
 => F = [ "You swapped ", \mmlm(Col, color(swap1, Mu)), " and ", 
	      \mmlm(Col, [color(swap1, Sigma), "."]) 
        ].

hint(swap1, [Mu, Sigma], Col, H)
 => H = [ "Try using ", \mmlm(Col, color(swap1, Mu)), " and ", 
	      \mmlm(Col, color(swap1, Sigma)), " in a different configuration." 
        ].

% Buggy rule: standard deviation was mistaken with variance.
buggy(prob, stage(1), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = item(x, mu, sigma, Perc),
    To = item(x, mu, instead(vardev_swap, sigma^2, sigma), Perc).

feedback(vardev_swap, [sigma], Col, F)
 => F = [ "You squared ", \mmlm(Col, color(vardev_swap, sigma)), " by mistake." ].

hint(vardev_swap, [sigma], Col, H)
 => H = [ "Use ", \mmlm(Col, color(vardev_swap, sigma)), " instead of ", \mmlm(Col, [color(vardev_swap, sigma^2), "."]) ].

% Buggy rule: (x - mu)/sigma was skipped.
buggy(prob, stage(2), From, To, [step(buggy, xp, []), depends(xp2)]) :-
   From = dfrac(x - mu, sigma),
   To = omit_right(xp, dfrac(omit_right(xp, x - mu), sigma)).

feedback(xp, [], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, z), "-value"]), " is calculated by using the formula ", 
          \mmlm(Col, [dfrac(x - mu, sigma), "."]) 
        ].

hint(xp, [], Col, H)
 => H = [ "Remember to calculate the ", \nowrap([\mmlm(Col, z), "-value."])].

% Buggy rule: x/100 was taken to be phi(z).
buggy(prob, stage(2), From, To, [step(buggy, xp2, []), depends(xp)]) :-
    From = pnorm_(z),
    To = instead(xp2, z/100, pnorm(z)).

feedback(xp2, [], Col, F)
 => F = [ "You mistakenly divided the ", \nowrap([\mmlm(Col, z), "-value"]), 
          " by 100 instead of retrieving the value from the normal distribution." 
        ].

hint(xp2, [], Col, H)
 => H = [ "Do not divide the ", \nowrap([\mmlm(Col, z), "-value"]), 
          " by 100, use the normal distribution instead." 
        ].

% 
% Expert rules for the quantile task
%
% First step: correctly identified the main steps of the calculation
intermediate(quantile, item).
intermediate(quantile, qnorm_).
expert(quantile, stage(2), From, To, [step(expert, steps, [])]) :-
    From = item(_X, Mu, Sigma, Perc),
    To = { '<-'( z, qnorm_(1 - dfrac(Perc, 100))) ;
           '<-'(x, z * Sigma + Mu)
         }.

feedback(steps, [], Col, F)
 => F = [ "Correctly determined the ", \nowrap([\mmlm(Col, z), "-statistic"]), " and translated it ",
           "to the original scale." 
        ].

hint(steps, [], Col, H)
 => H = [ "First determine the ", \nowrap([\mmlm(Col, z), "-statistic,"]), " then translate ",
           "it to the original scale." ].

% Second step: selected the correct tail from the normal distribution
expert(quantile, stage(2), From, To, [step(expert, correct_tail, [])]) :-
    From = qnorm_(P),
    To = qnorm(P).

feedback(correct_tail, [], _Col, F)
 => F = [ "Correctly used the lower tail of the normal distribution." ].

hint(correct_tail, [], _Col, H)
 => H = [ "The upper tail of the normal distribution is needed." ].

%
% Buggy rules for the quantile task
%
% Buggy rule: use upper tail instead of lower tail
%
buggy(quantile, stage(2), From, To, [step(buggy, lowertail, [])]) :-
    From = qnorm_(1 - P),
    To = qnorm(instead(lowertail, P, 1 - P)).

feedback(lowertail, [], _Col, F)
 => F = [ "Your response matches the lower instead of the upper tail of the normal ",
          "distribution." 
        ].

hint(lowertail, [], _Col, H)
 => H = [ "Do not select the upper tail of the normal distribution." ].

% Buggy rule: Mu and Sigma were swapped.
buggy(quantile, stage(2), From, To, [step(buggy, swap2, [mu, Sigma])]) :-
    From = z * Sigma + mu,
    To = instead(swap2, z * mu + Sigma, From).

feedback(swap2, [Mu, Sigma], Col, F)
 => F = [ "You swapped ", \mmlm(Col, color(swap2, Mu)), " and ",
	      \mmlm(Col, [color(swap2, Sigma), "."]) 
        ].

hint(swap2, [Mu, Sigma], Col, H)
 => H = [ "Try using ", \mmlm(Col, color(swap2, Mu)), " and ", 
	      \mmlm(Col, color(swap2, Sigma)), " in a different configuration." 
        ].

% Buggy rule: standard deviation was mistaken with variance.
buggy(quantile, stage(2), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = Z * sigma + Mu,
    To = Z * add_right(vardev_swap, sigma^2) + Mu.

feedback(vardev_swap, [Sigma], Col, F)
 => F = [ "You squared ", \mmlm(Col, color(vardev_swap, Sigma)), " by mistake." ].

hint(vardev_swap, [_Sigma], _Col, H)
 => H = [ "Use the standard deviation instead of the variance." ].

% Buggy rule: divide percentage by 1000 instead of 100 to translate to proportion
% Probably due to confusion with the omitted 0 in p-values like .05
buggy(quantile, stage(2), From, To, [step(buggy, perc1000, [1000])]) :-
    From = dfrac(Perc, 100),
    To = dfrac(Perc, instead(perc1000, 1000, 100)).

feedback(perc1000, [Div], Col, F)
 => F = [ "You divided the percentage by " ,
          \mmlm(Col, color(perc1000, Div)), " instead of 100 to obtain a proportion."
        ].

hint(perc1000, [_Div], _Col, H)
 => H = [ "Make sure to divide by 100 when translating a percentage to a proportion." ].

% Buggy rule: divide percentage by 10 instead of 100 to translate to proportion
buggy(quantile, stage(2), From, To, [step(buggy, perc10, [10])]) :-
    From = dfrac(P, 100),
    To = instead(perc10, dfrac(P, 10), From).

feedback(perc10, [Div], Col, F)
 => F = [ "You divided the percentage by " ,
          \mmlm(Col, color(perc10, Div)), " instead of 100 to obtain a proportion." 
        ].

hint(perc10, [_Div], _Col, H)
 => H = [ "Make sure to divide by 100 when translating a percentage to a proportion." ].

% Buggy rule: z-value taken as result
buggy(quantile, stage(2), From, To, [step(buggy, only_z, [z, sigma, mu])]) :-
    From = z * sigma + mu,
    To = instead(only_z, z , From).

feedback(only_z, [z, sigma, mu], Col, F)
 => F = [ "You only calculated the ", \nowrap([\mmlm(Col, z), "-value."]) ].

hint(only_z, [z, sigma, mu], Col, H)
 => H = [ "Translate the ", \nowrap([\mmlm(Col, z), "-value"]),  
           " back to the original scale." 
        ].