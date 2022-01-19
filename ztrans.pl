:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

mathml:hook(Flags, x, [task(ztrans) | Flags], 'X').

interval:r_hook(x).
interval:r_hook(sigma).
interval:r_hook(z).
interval:r_hook(pnorm(_Z)).
interval:r_hook(p).

render(ztrans, item(X, Mu, Sigma), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([task(ztrans), digits(0)], X), " follow a normal distribution with ",
              "expectation ", \mmlm([task(ztrans), digits(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([task(ztrans), digits(0)], [Sigma = r(sigma), "."]),
              "A table of the standard ",
              "normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "How many realizations are ",
                  "below ", \mmlm([task(ztrans), digits(0)], [r(x), "?"])
              ]),
            form([class(form), method('POST'), action('#ztrans-response')],
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

% z-transformation and normal distribution
intermediate(_, item).
start(ztrans, item(x, mu, sigma)).

intermediate(ztrans, pnorm_).
intermediate(ztrans, zcalc).
expert(ztrans, stage(1), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma),
    To = { '<-'(z, zcalc(X, Mu, Sigma)) ;
           '<-'(p, pnorm_(z)) ; 
           p
         }.

feedback(ztrans, allinone, [], _Col, FB) =>
    FB = [ "You correctly identified the main steps of the calculation."].

hint(ztrans, allinone, [], Col, FB) =>
    FB = [ "Calculate the z-value and look up the corresponding ", \mmlm(Col, hyph(phi,"value.")) ].


% Expert rule (zcalc)
expert(ztrans, stage(1), From, To, [step(expert, zcalc, [X, Mu, Sigma])]) :-
    From = zcalc(X, Mu, Sigma),
    To = dfrac(X - Mu, Sigma).

feedback(ztrans, zcalc, [_X, _Mu, _Sigma], _Col, FB) =>
    FB = [ "Good Job! You correctly calculated Z." ].

hint(ztrans, zcalc, [X, Mu, Sigma], Col, FB) =>
    FB = [ "To calculate the z-value, use " , \mmlm(Col, dfrac(X - Mu, Sigma)) ].

% Expert rule (correct tail)
expert(ztrans, stage(2), From, To, [step(expert, correct_tail, [Z])]) :-
    From = pnorm_(Z),
    To = pnorm(Z).

feedback(ztrans, correct_tail, [_Z], _Col, FB) =>
    FB = [ "You calculated the correct tail of the distribution." ].

hint(ztrans, correct_tail, [_Z], _Col, FB) =>
    FB = [ "Use the lower tail of the normal distribution and select the value corresponding to the z-value." ].


% Buggy rule (wrong tail) The wrong tail of the normal distribution was selected.
buggy(ztrans, stage(2), From, To, [step(buggy, wrong_tail, [Z])]) :-
    From = pnorm_(Z),
    To = instead(bug(wrong_tail), 1 - pnorm(Z), pnorm(Z)).

feedback(ztrans, wrong_tail, [_Z], Col, FB) =>
    FB = [ "Your answer matches the ", \mmlm(Col, color(wrong_tail, "wrong tail " )), "of the normal distribution." ].

hint(ztrans, wrong_tail, [_Z], _Col, FB) =>
    FB = [ "Do not use the upper tail of the normal distribution." ].

% Buggy Rule (plus) Mu was added to X, not subtracted.
buggy(ztrans, stage(2), From, To, [step(buggy, plus, [X, Mu])]) :-
    From = dfrac(X - Mu, Sigma),
    To = dfrac(instead(bug(plus), X + Mu, X - Mu), Sigma).

feedback(ztrans, plus, [X, Mu], Col, FB) =>
    FB = [ "Subtract ", \mmlm(Col, color(plus, Mu)), " from ", \mmlm(Col, color(plus, X)),
           " instead of adding them up." ].

hint(ztrans, plus, [X, Mu], Col, FB) =>
    FB = [ "Try using subtraction rather than addition in ", 
           \mmlm(Col, color(plus, X + Mu)) ].

% Buggy Rule (swap) Mu and Sigma were swapped.
buggy(ztrans, stage(1), From, To, [step(buggy, swap, [mu, sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, instead(bug(swap), sigma, mu), instead(bug(swap), mu, sigma));
    From = item(x, mu, sigma^2),
    To = item(x, instead(bug(swap), sigma^2, mu), instead(bug(swap), mu, sigma)).

feedback(ztrans, swap, [Mu, Sigma], Col, FB) =>
    FB = [ "You swapped ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, color(swap, Sigma)) ].

hint(ztrans, swap, [Mu, Sigma], Col, FB) =>
    FB = [ "Try using ", \mmlm(Col, color(swap, Mu)), " and ", 
	   \mmlm(Col, color(swap, Sigma)), " in a different configuration." ].

% Buggy Rule (vardev swap) standard deviation was mistaken with variance.
buggy(ztrans, stage(1), From, To, [step(buggy, vardev_swap, [sigma])]) :-
    From = item(x, mu, sigma),
    To = item(x, mu, instead(bug(vardev_swap), sigma^2, sigma)).

feedback(ztrans, vardev_swap, [sigma], Col, FB) =>
    FB = [ "You squared ", \mmlm(Col, color(vardev_swap, sigma)), " by mistake." ].

hint(ztrans, vardev_swap, [sigma], Col, FB) =>
    FB = [ "Use ", \mmlm(Col, color(vardev_swap, sigma)), " instead of ", \mmlm(Col, color(vardev_swap, sigma^2)) ].

% Buggy Rule (xp) (x - mu)/sigma was skipped.
buggy(ztrans, stage(2), From, To, [step(buggy, xp, []), depends(xp2)]) :-
   From = dfrac(x - mu, sigma),
   To = omit_right(bug(xp), dfrac(omit_right(bug(xp), x - mu), sigma)).

feedback(ztrans, xp, [], Col, FB) =>
    FB = [ "The z-value is calculated by calculating ", \mmlm(Col, dfrac(x - mu, sigma)) ].

hint(ztrans, xp, [], _Col, FB) =>
    FB = [ "Remember to calculate the z-value." ].

% Buggy Rule (xp2) x/100 was taken to be phi(z).
buggy(ztrans, stage(2), From, To, [step(buggy, xp2, []), depends(xp)]) :-
    From = pnorm_(z),
    To = instead(bug(xp2), z/100, pnorm(z)).

feedback(ztrans, xp2, [], _Col, FB) =>
    FB = [ "von Matthias hinzugefuegt." ].

hint(ztrans, xp2, [], Col, FB) =>
    FB = [ "von Matthias hinzugefuegt ", \mmlm(Col, dfrac(x - mu, sigma)) ].

