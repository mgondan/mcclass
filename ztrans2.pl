:- module(ztrans2, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(tpaired, [i(z), "-transform (2)"]).

:- multifile intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

mathml_hook(x, 'X').

interval:r_hook(x).
interval:r_hook(sigma).
interval:r_hook(z).
interval:r_hook(qnorm(_P)).
interval:r_hook(p).

render(item(P, Mu, Sigma), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm(x), " follow a Normal distribution with ",
              "expectation ", \mmlm([digits(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([digits(0)], [Sigma = r(sigma), "."]),
              "A table of the standard ",
              "Normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "In which area do the upper ", \mmlm([digits(0)], [r(P), "% fall?"])
              ]),
            form([class(form), method('POST'), action('#ztrans2-response')],
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

intermediate(item).
start(item(p, mu, sigma)).

intermediate(qnorm_).
expert(stage(2), From, To, [step(expert, steps, [])]) :-
    From = item(P, Mu, Sigma),
    To = { '<-'( z, qnorm_(1 - dfrac(P, 100))) ;
           '<-'(x, z * Sigma + Mu) ;
           x
         }.

feedback(steps, [], Col, FB) =>
    FB = [ "You determined the ", \mmlm(Col, z), "-statistic and translated it ",
           "to the original scale." ].

hint(steps, [], Col, FB) =>
    FB = [ "First determine the ", \mmlm(Col, z), "statistic. Then translate ",
           "it to the original scale." ].

% Expert rule (correct tail)
expert(stage(2), From, To, [step(expert, correct_tail, [])]) :-
    From = qnorm_(P),
    To = qnorm(P).

feedback(correct_tail, [], _Col, FB) =>
    FB = [ "Your response matches the lower tail of the Normal distribution." ].

hint(correct_tail, [], _Col, FB) =>
    FB = [ "Try using the upper tail of the Normal distribution." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% wrong_tail
%
% The wrong tail of the Normal distribution was selected.
%
buggy(stage(2), From, To, [step(buggy, wrong_tail, [])]) :-
    From = qnorm_(1 - P),
    To = qnorm(instead(wrong_tail, P, 1 - P)).

feedback(wrong_tail, [], _Col, FB) =>
    FB = [ "Your response matches the lower, not the upper tail of the Normal ",
           "distribution." ].

hint(wrong_tail, [], _Col, FB) =>
    FB = [ "Make sure to use the correct tail of the Normal distribution." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (swap) Mu and Sigma were swapped.
buggy(stage(2), From, To, [step(buggy, swap, [mu, Sigma])]) :-
    From = z * Sigma + mu,
    To = instead(swap, z * mu + Sigma, From).

feedback(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ",
	   \mmlm(Col, color(swap, Sigma)) ].

hint(swap, [Mu, Sigma], Col, FB) =>
    FB = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, color(swap, Sigma)), " in a different configuration." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (vardev swap) standard deviation was mistaken with variance.
buggy(stage(2), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = Z * sigma + Mu,
    To = Z * invent_right(vardev_swap, sigma^2) + Mu.

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
buggy(stage(2), From, To, [step(buggy, perc1000, [1000])]) :-
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
buggy(stage(2), From, To, [step(buggy, pdecimal2, [P])]) :-
    From = dfrac( P , 100 ),
    To = instead(pdecimal2, dfrac( P , 10 ), From).

feedback(pdecimal2, [_], _ , FB) =>
    FB = [ "You converted P-% incorrectly to a decimal representation." ].

hint(pdecimal2, [P], Col, FB) =>
    FB = [ \mmlm(Col, color(pdecimal2, r(P))), "% in decimal representation is ", \mmlm(Col, color(pdecimal2, r(P/100))) ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% Buggy Rule (zx) The z value was calculated but taken to be the endresult.
buggy(stage(2), From, To, [step(buggy, zx, [z, sigma, mu])]) :-
    From = z * sigma + mu,
    To = instead(zx, z , From).

feedback(zx, [z, sigma, mu], _Col, FB) =>
    FB = [ "To complete the exercise successfully you have to do the second calculation aswell." ].

hint(zx, [z, sigma, mu], Col, FB) =>
    FB = [ \mmlm(Col, color(zx, z)), "is the correct answer of the first equation. To continue calculate ", \mmlm(Col, color(zx, z * sigma + mu)), "." ].

