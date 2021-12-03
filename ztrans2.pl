:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(ztrans2) :-
    r_session_source(ztrans2).

mathml:hook(Flags, x, Flags, 'X').

interval:hook(pl, x, r(x)).
interval:hook(pl, sigma, r(sigma)).
interval:hook(pl, z, r(z)).
interval:hook(pl, qnorm(Z), r(qnorm(Z))).
interval:hook(pl, p, r(p)).

render(ztrans2, item(P, Mu, Sigma), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([round(0)], P), " follow a Normal distribution with ",
              "expectation ", \mmlm([round(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([round(0)], [Sigma = r(sigma), "."]),
              "A table of the standard ",
              "Normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "In which area do the upper ", \mmlm([round(0)], [r(p), "% fall?"])
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

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

intermediate(_, item).
start(ztrans2, item(p, mu, sigma)) :-
    init(ztrans2).

intermediate(ztrans2, qnorm_).
expert(ztrans2, stage(2), From, To, [step(expert, steps, [])]) :-
    From = item(P, Mu, Sigma),
    To = { '<-'( z, qnorm_(1 - dfrac(P, 100))) ;
           '<-'(x, z * Sigma + Mu) ;
           x
         }.

feedback(ztrans2, steps, [], Col, FB) :-
    FB = [ "Determined the ", \mmlm(Col, z), "-statistic and translated it ",
           "to the original scale." ].

hint(ztrans2, steps, [], Col, FB) :-
    FB = [ "First determine the ", \mmlm(Col, z), "statistic. Then translate ",
           "it to the original scale." ].


% Expert rule (correct tail)
expert(ztrans2, stage(2), From, To, [step(expert, correct_tail, [])]) :-
    From = qnorm_(P),
    To = qnorm(P).

feedback(ztrans2, correct_tail, [], _Col, FB) :-
    FB = [ "The response matches the lower tail of the Normal distribution." ].

hint(ztrans2, correct_tail, [], _Col, FB) :-
    FB = [ "The upper tail of the Normal distribution is used." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% wrong_tail
%
% The wrong tail of the Normal distribution was selected.
%
buggy(ztrans2, stage(2), From, To, [step(buggy, wrong_tail, [])]) :-
    From = qnorm_(1 - P),
    To = qnorm(instead(bug(wrong_tail), P, 1 - P)).

feedback(ztrans2, wrong_tail, [], _Col, FB) :-
    FB = [ "The response matches the lower tail of the Normal ",
           "distribution. (wrong_tail)" ].

hint(ztrans2, wrong_tail, [], _Col, FB) :-
    FB = [ "Make sure to use the correct tail of the Normal distribution." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% Buggy Rule (swap) Mu and Sigma were swapped.
%buggy(ztrans2, stage(2), From, To, [step(buggy, swap, [mu, Sigma])]) :-
%    From = z * Sigma + mu,
%    To = instead(bug(swap), z * mu + Sigma, From).
%
%feedback(ztrans2, swap, [Mu, Sigma], Col, FB) :-
%    FB = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ",
%	   \mmlm(Col, color(swap, Sigma)), "(swap)" ].
%
%hint(ztrans2, swap, [Mu, Sigma], Col, FB) :-
%    FB = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
%	   \mmlm(Col, color(swap, Sigma)), " in a different configuration." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% Buggy Rule (vardev swap) standard deviation was mistaken with variance.
%buggy(ztrans2, stage(2), From, To, [step(buggy, vardev_swap, [sigma])]) :-
%    From = Z * sigma + Mu,
%    To = Z * invent_right(bug(vardev_swap), sigma^2) + Mu.
%
%feedback(ztrans2, vardev_swap, [Sigma], Col, FB) :-
%    FB = [ \mmlm(Col, color(vardev_swap, Sigma)), "was squared by mistake. (vardev_swap)" ].
%
%hint(ztrans2, vardev_swap, [_Sigma], _Col, FB) :-
%    FB = [ "Use the standard deviation instead of the variance." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% pdec1000
%
% Probably due to confusion with the omitted 0 in p-values like .05, some 
% students divide by 1000 instead of 100 when translating percentages to
% proportions.
%
buggy(ztrans2, stage(2), From, To, [step(buggy, perc1000, [1000])]) :-
    From = dfrac(P, 100),
    To = dfrac(P, instead(bug(perc1000), 1000, 100)).

feedback(ztrans2, perc1000, [P], Col , FB) :-
    FB = [ "The percentage was divided ",
           "by ", \mmlm(Col, color(perc1000, P)), " instead of 100 to obtain ",
           "a proportion (perc1000)." ].

hint(ztrans2, perc1000, [_P], _Col, FB) :-
    FB = [ "Make sure to divide by 100 when translating a percentage to a ",
           "proportion." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% Buggy Rule (pdecimal2) p was taken to be ten times its true value (5% -/-> 0,05. 5% --> 0,5).
%buggy(ztrans2, stage(2), From, To, [step(buggy, pdecimal2, [P])]) :-
%    From = dfrac( P , 100 ),
%    To = instead(bug(pdecimal2), dfrac( P , 10 ), From).
%
%feedback(ztrans2, pdecimal2, [_], _ , FB) :-
%    FB = [ "P-% was incorrectly converted to a decimal representation. (pdecimal2)" ].
%
%hint(ztrans2, pdecimal2, [P], Col, FB) :-
%    FB = [ \mmlm(Col, color(pdecimal2, r(P))), "% in decimal representation is ", \mmlm(Col, color(pdecimal2, r(P/100))) ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%

% Buggy Rule (zx) The z value was calculated but taken to be the endresult.
%buggy(ztrans2, stage(2), From, To, [step(buggy, zx, [z, sigma, mu])]) :-
%    From = z * sigma + mu,
%    To = instead(bug(zx), z , From).
%
%feedback(ztrans2, zx, [z, sigma, mu], _Col, FB) :-
%    FB = [ "To complete the exercise successfully you have to do the second calculation aswell. (zx)" ].
%
%hint(ztrans2, zx, [z, sigma, mu], Col, FB) :-
%    FB = [ \mmlm(Col, color(zx, z)), "is the correct answer of the first equation. To continue calculate ", \mmlm(Col, color(zx, z * sigma + mu)), "." ].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
