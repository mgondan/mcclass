:- module(ztrans2, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(ztrans2, [i(z), "-transform (2)"]).
task(ztrans).

:- multifile intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

math_hook(x, 'X').

r_hook(x).
r_hook(sigma).
r_hook(z).
r_hook(p).

r_hook('<-'/2).

render
--> {start(item(_P, Mu, Sigma)) },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm(x), " follow a normal distribution with ",
              "expectation ", \mmlm([digits(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([digits(0)], [Sigma = r(sigma), "."]),
              " A table of the standard ",
              "normal distribution is found below."
            ])]))]).

task(ztrans)
--> { start(item(P, _Mu, _Sigma)),
      session_data(resp(ztrans2, ztrans, Resp), resp(ztrans2, ztrans, '##.#'))
	},
	html(\htmlform([ "In which area do the upper ", \mmlm([digits(0)], [r(P), "% fall?"])], ztrans, Resp)).

intermediate(ztrans, item).
start(item(p, mu, sigma)).

intermediate(ztrans, qnorm_).
expert(ztrans, stage(2), From, To, [step(expert, steps, [])]) :-
    From = item(P, Mu, Sigma),
    To = { '<-'( z, qnorm_(1 - dfrac(P, 100))) ;
           '<-'(x, z * Sigma + Mu)
         }.

feedback(steps, [], Col, FB) =>
    FB = [ "You determined the ", \mmlm(Col, hyph(z, "statistic")), " and translated it ",
           "to the original scale." ].

hint(steps, [], Col, FB) =>
    FB = [ "First determine the ", \mmlm(Col,  hyph(z, "statistic")), " Then translate ",
           "it to the original scale." ].

% Expert rule (correct tail)
expert(ztrans, stage(2), From, To, [step(expert, correct_tail, [])]) :-
    From = qnorm_(P),
    To = qnorm(P).

feedback(correct_tail, [], _Col, FB) =>
    FB = [ "Your response matches the lower tail of the normal distribution." ].

hint(correct_tail, [], _Col, FB) =>
    FB = [ "Try using the upper tail of the normal distribution." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% wrong_tail
%
% The wrong tail of the Normal distribution was selected.
%
buggy(ztrans, stage(2), From, To, [step(buggy, wrong_tail, [])]) :-
    From = qnorm_(1 - P),
    To = qnorm(instead(wrong_tail, P, 1 - P)).

feedback(wrong_tail, [], _Col, FB) =>
    FB = [ "Your response matches the lower instead of the upper tail of the normal ",
           "distribution." ].

hint(wrong_tail, [], _Col, FB) =>
    FB = [ "Make sure to use the correct tail of the normal distribution." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (swap) Mu and Sigma were swapped.
buggy(ztrans, stage(2), From, To, [step(buggy, swap, [mu, Sigma])]) :-
    From = z * Sigma + mu,
    To = instead(swap, z * mu + Sigma, From).

feedback(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ",
	   \mmlm(Col, [color(swap, Sigma), "."]) ].

hint(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, color(swap, Sigma)), " in a different configuration." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (vardev swap) standard deviation was mistaken with variance.
buggy(ztrans, stage(2), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = Z * sigma + Mu,
    To = Z * add_right(vardev_swap, sigma^2) + Mu.

feedback(vardev_swap, [Sigma], Col, FB) =>
    FB = [ "You squared ", \mmlm(Col, color(vardev_swap, Sigma)), " by mistake." ].

hint(vardev_swap, [_Sigma], _Col, FB) =>
    FB = [ "Use the standard deviation instead of the variance." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% pdec1000
%
% Probably due to confusion with the omitted 0 in p-values like .05, some 
% students divide by 1000 instead of 100 when translating percentages to
% proportions.
%
buggy(ztrans, stage(2), From, To, [step(buggy, perc1000, [1000])]) :-
    From = dfrac(P, 100),
    To = dfrac(P, instead(perc1000, 1000, 100)).

feedback(perc1000, [P], Col , FB) =>
    FB = [ "You divided the percentage by " ,
            \mmlm(Col, color(perc1000, P)), " instead of 100 to obtain ",
           "a proportion." ].

hint(perc1000, [_P], _Col, FB) =>
    FB = [ "Make sure to divide by 100 when translating a percentage to a ",
           "proportion." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (pdecimal2) p was taken to be ten times its true value (5% -/-> 0,05. 5% --> 0,5).
buggy(ztrans, stage(2), From, To, [step(buggy, pdecimal2, [P])]) :-
    From = dfrac( P , 100 ),
    To = instead(pdecimal2, dfrac( P , 10 ), From).

feedback(pdecimal2, [_], _ , FB) =>
    FB = [ "You converted P-% incorrectly to a decimal representation." ].

hint(pdecimal2, [P], Col, FB) =>
    FB = [ \mmlm(Col, color(pdecimal2, perc(r(P)))), " in decimal representation is ", \mmlm(Col, [color(pdecimal2, r(P/100)), "."]) ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (zx) The z value was calculated but taken to be the endresult.
buggy(ztrans, stage(2), From, To, [step(buggy, zx, [z, sigma, mu])]) :-
    From = z * sigma + mu,
    To = instead(zx, z , From).

feedback(zx, [z, sigma, mu], _Col, FB) =>
    FB = [ "To complete the exercise successfully, you have to perform the second calculation as well." ].

hint(zx, [z, sigma, mu], Col, FB) =>
    FB = [ \mmlm(Col, color(zx, z)), " is the correct answer of the first equation. To continue, you need to calculate ", \mmlm(Col, [color(zx, z * sigma + mu), "."]) ].

