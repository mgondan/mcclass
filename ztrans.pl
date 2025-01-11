:- module(ztrans, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module('/home/jeremyirilli/interval/prolog/mcclass.pl').
:- use_module(r_mcclass).
%:- use_module(r).
%:- use_module(rint).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(ztrans, [i(z), "-transform (1)"]).
task(ztrans).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

math_hook(x, 'X').

r_hook(x).
r_hook(sigma).
r_hook(z).
r_hook(pnorm(_Z)).
r_hook(p).

render
--> { start(item(X, Mu, Sigma)) },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([digits(0)], X), " follow a normal distribution with ",
              "expectation ", \mmlm([digits(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([digits(0)], [Sigma = r(sigma), "."]),
              " A table of the standard ",
              "normal distribution is found below."
            ])]))]).

task(ztrans)
--> { start(item(_X, _Mu, _Sigma)),
      session_data(resp(ztrans, ztrans, Resp), resp(ztrans, ztrans, '.##'))
	},
	html(\htmlform([ "How many realizations are ",
        "below ", \mmlm([digits(0)], [r(x), "?"])], ztrans, Resp)).
      
% z-transformation and normal distribution
intermediate(ztrans, item).
start(item(x, mu, sigma)).

intermediate(ztrans, pnorm_).
intermediate(ztrans, zcalc).
expert(ztrans, stage(1), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma),
    To = { '<-'(z, zcalc(X, Mu, Sigma)) ;
           '<-'(p, pnorm_(z)) 
         }.

feedback(allinone, [], _Col, FB) =>
    FB = [ "You correctly identified the main steps of the calculation."].

hint(allinone, [], Col, FB) =>
    FB = [ "Calculate the z-value and look up the corresponding ", \mmlm(Col, hyph(phi,"value.")) ].


% Expert rule (zcalc)
expert(ztrans, stage(1), From, To, [step(expert, zcalc, [X, Mu, Sigma])]) :-
    From = zcalc(X, Mu, Sigma),
    To = dfrac(X - Mu, Sigma).

feedback(zcalc, [_X, _Mu, _Sigma], _Col, FB) =>
    FB = [ "You correctly calculated Z." ].

hint(zcalc, [X, Mu, Sigma], Col, FB) =>
    FB = [ "To calculate the z-value, use " , \mmlm(Col, [dfrac(X - Mu, Sigma), "."]) ].

% Expert rule (correct tail)
expert(ztrans, stage(2), From, To, [step(expert, correct_tail, [Z])]) :-
    From = pnorm_(Z),
    To = pnorm(Z).

feedback(correct_tail, [_Z], _Col, FB) =>
    FB = [ "You calculated the correct tail of the distribution." ].

hint(correct_tail, [_Z], _Col, FB) =>
    FB = [ "Use the lower tail of the normal distribution and select the value corresponding to the z-value." ].


% Buggy rule (wrong tail) The wrong tail of the normal distribution was selected.
buggy(ztrans, stage(2), From, To, [step(buggy, wrong_tail, [Z])]) :-
    From = pnorm_(Z),
    To = instead(wrong_tail, 1 - pnorm(Z), pnorm(Z)).

feedback(wrong_tail, [_Z], Col, FB) =>
    FB = [ "Your answer matches the ", \mmlm(Col, color(wrong_tail, "wrong tail" )), " of the normal distribution." ].

hint(wrong_tail, [_Z], _Col, FB) =>
    FB = [ "Do not use the upper tail of the normal distribution." ].

% Buggy Rule (plus) Mu was added to X, not subtracted.
buggy(ztrans, stage(2), From, To, [step(buggy, plus, [X, Mu])]) :-
    From = dfrac(X - Mu, Sigma),
    To = dfrac(instead(plus, X + Mu, X - Mu), Sigma).

feedback(plus, [X, Mu], Col, FB) =>
    FB = [ "Subtract ", \mmlm(Col, color(plus, Mu)), " from ", \mmlm(Col, color(plus, X)),
           " instead of adding them up." ].

hint(plus, [X, Mu], Col, FB) =>
    FB = [ "Try using subtraction rather than addition in ", 
           \mmlm(Col, [color(plus, X + Mu), "."]) ].

% Buggy Rule (swap) Mu and Sigma were swapped.
buggy(ztrans, stage(1), From, To, [step(buggy, swap, [mu, sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, instead(swap, sigma, mu), instead(swap, mu, sigma));
    From = item(x, mu, sigma^2),
    To = item(x, instead(swap, sigma^2, mu), instead(swap, mu, sigma)).

feedback(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, [color(swap, Sigma), "."]) ].

hint(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, color(swap, Sigma)), " in a different configuration." ].

% Buggy Rule (vardev swap) standard deviation was mistaken with variance.
buggy(ztrans, stage(1), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, mu, instead(vardev_swap, sigma^2, sigma)).

feedback(vardev_swap, [sigma], Col, FB) =>
    FB = [ "You squared ", \mmlm(Col, color(vardev_swap, sigma)), " by mistake." ].

hint(vardev_swap, [sigma], Col, FB) =>
    FB = [ "Use ", \mmlm(Col, color(vardev_swap, sigma)), " instead of ", \mmlm(Col, [color(vardev_swap, sigma^2), "."]) ].

% Buggy Rule (xp) (x - mu)/sigma was skipped.
buggy(ztrans, stage(2), From, To, [step(buggy, xp, []), depends(xp2)]) :-
   From = dfrac(x - mu, sigma),
   To = omit_right(xp, dfrac(omit_right(xp, x - mu), sigma)).

feedback(xp, [], Col, FB) =>
    FB = [ "The z-value is calculated by using the formula ", \mmlm(Col, [dfrac(x - mu, sigma), "."]) ].

hint(xp, [], _Col, FB) =>
    FB = [ "Remember to calculate the z-value." ].

% Buggy Rule (xp2) x/100 was taken to be phi(z).
buggy(ztrans, stage(2), From, To, [step(buggy, xp2, []), depends(xp)]) :-
    From = pnorm_(z),
    To = instead(xp2, z/100, pnorm(z)).

feedback(xp2, [], _Col, FB) =>
    FB = [ "You mistakenly divided the z-value by 100 instead of retrieving the value from the normal distribution." ].

hint(xp2, [], _Col, FB) =>
    FB = [ "Do not divide the z-value by 100, use the normal distribution instead." ].

