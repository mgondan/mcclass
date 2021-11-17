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
interval:hook(pl, pnorm(Z), r(pnorm(Z))).
interval:hook(pl, p, r(p)).

render(ztrans2, item(X, Mu, Sigma), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm([round(0)], X), " follow a Normal distribution with ",
              "expectation ", \mmlm([round(0)], Mu = r(mu)), " and ",
              "standard deviation ", \mmlm([round(0)], [Sigma = r(sigma), "."]),
              "A table of the standard ",
              "Normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "How many realizations are ",
                  "below ", \mmlm([round(0)], [r(x), "?"])
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


intermediate(_, item).
start(ztrans2, item(x, mu, sigma)) :-
    init(ztrans2).

intermediate(ztrans2, pnorm_).
expert(ztrans2, stage(2), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma),
    To = { '<-'(z, dfrac(X - Mu, Sigma)) ;
           '<-'(p, pnorm_(z)) ; 
           p
         }.

feedback(ztrans2, allinone, [], _Col, FB) :-
    FB = [ "Everything done correctly."].

hint(ztrans2, allinone, [], _Col, FB) :-
    FB = [ "Try to do everything correctly."].


expert(ztrans2, stage(2), From, To, [step(expert, correct_tail, [Z])]) :-
    From = pnorm_(Z),
    To = pnorm(Z).

feedback(ztrans2, correct_tail, [_Z], _Col, FB) :-
    FB = [ "The response matches the correct tail of the Normal distribution." ].

hint(ztrans2, correct_tail, [_Z], _Col, FB) :-
    FB = [ "The lower tail of the Normal distribution is used." ].
