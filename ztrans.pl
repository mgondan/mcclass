:- module(ztrans, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(interval/interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(ztrans, [i(z), "-transform (1)"]).

task(prob).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(x, 'X').

% R definitions
r_hook(x).
r_hook(sigma).
r_hook(z).
r_hook(p).

% Task description
render(Flags)
--> { start(item(X, Mu, Sigma)) },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([digits(0) | Flags], X), " follow a normal distribution with ",
              "expectation ", \mmlm([digits(0) | Flags], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([digits(0) | Flags], [Sigma = r(sigma), "."]),
              " A table of the standard ",
              "normal distribution is found below."
            ])]))]).

% Question for the probability
task(Flags, prob)
--> { start(item(_X, _Mu, _Sigma)),
      session_data(resp(ztrans, prob, Resp), resp(ztrans, prob, '.##'))
	},
	html(\htmlform([ "What is the probability of having realizations below ", 
         \mmlm([digits(0) | Flags], [r(x), "?"])], prob, Resp)).

start(item(x, mu, sigma)).

%
% Expert rules for the probability task 
%
intermediate(prob, item).
intermediate(prob, pnorm_).
intermediate(prob, zcalc).
% First step: correctly identified the main steps of the calculation
expert(prob, stage(1), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma),
    To = { '<-'(z, zcalc(X, Mu, Sigma)) ;
           '<-'(p, pnorm_(z)) 
         }.

feedback(allinone, [], _Col, F) 
 => F = [ "You correctly identified the main steps of the calculation."].

hint(allinone, [], Col, H) 
 => H = [ "Calculate the ", nowrap([\mmlm(Col, z), "-value"]), 
          " and look up the corresponding ", nowrap([\mmlm(Col, phi), "-value."]) 
        ].

% Second step: correctly converted to a z-value
expert(prob, stage(1), From, To, [step(expert, zcalc, [X, Mu, Sigma])]) :-
    From = zcalc(X, Mu, Sigma),
    To = dfrac(X - Mu, Sigma).

feedback(zcalc, [_X, _Mu, _Sigma], Col, F) 
 => F = [ "You correctly calculated the ", nowrap([\mmlm(Col, z), "-value."]) ].

hint(zcalc, [X, Mu, Sigma], Col, H) 
 => H = [ "To calculate the ", nowrap([\mmlm(Col, z), "-value"]), 
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
          nowrap([\mmlm(Col, z), "-value."])
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
buggy(prob, stage(1), From, To, [step(buggy, swap, [mu, sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, instead(swap, sigma, mu), instead(swap, mu, sigma));
    From = item(x, mu, sigma^2),
    To = item(x, instead(swap, sigma^2, mu), instead(swap, mu, sigma)).

feedback(swap, [Mu, Sigma], Col, F) 
 => F = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ", 
	      \mmlm(Col, [color(swap, Sigma), "."]) 
        ].

hint(swap, [Mu, Sigma], Col, H)
 => H = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
	      \mmlm(Col, color(swap, Sigma)), " in a different configuration." 
        ].

% Buggy rule: standard deviation was mistaken with variance.
buggy(prob, stage(1), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, mu, instead(vardev_swap, sigma^2, sigma)).

feedback(vardev_swap, [sigma], Col, F)
 => F = [ "You squared ", \mmlm(Col, color(vardev_swap, sigma)), " by mistake." ].

hint(vardev_swap, [sigma], Col, H)
 => H = [ "Use ", \mmlm(Col, color(vardev_swap, sigma)), " instead of ", \mmlm(Col, [color(vardev_swap, sigma^2), "."]) ].

% Buggy rule: (x - mu)/sigma was skipped.
buggy(prob, stage(2), From, To, [step(buggy, xp, []), depends(xp2)]) :-
   From = dfrac(x - mu, sigma),
   To = omit_right(xp, dfrac(omit_right(xp, x - mu), sigma)).

feedback(xp, [], Col, F)
 => F = [ "The ", nowrap([\mmlm(Col, z), "-value"]), " is calculated by using the formula ", 
          \mmlm(Col, [dfrac(x - mu, sigma), "."]) 
        ].

hint(xp, [], Col, H)
 => H = [ "Remember to calculate the ", nowrap([\mmlm(Col, z), "-value."])].

% Buggy rule: x/100 was taken to be phi(z).
buggy(prob, stage(2), From, To, [step(buggy, xp2, []), depends(xp)]) :-
    From = pnorm_(z),
    To = instead(xp2, z/100, pnorm(z)).

feedback(xp2, [], Col, F)
 => F = [ "You mistakenly divided the ", nowrap([\mmlm(Col, z), "-value"]), 
          " by 100 instead of retrieving the value from the normal distribution." 
        ].

hint(xp2, [], Col, H)
 => H = [ "Do not divide the ", nowrap([\mmlm(Col, z), "-value"]), 
          " by 100, use the normal distribution instead." 
        ].

