:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(ztrans) :-
    r_session_source(ztrans).

mathml:hook(Flags, x, Flags, 'X').

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
    r_session(Expr, R),
    #(Res) = R,
    number(Res).

render(ztrans, item(X, Mu, Sigma), Form) -->
    { option(resp(R), Form, '## %') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm('X'), " follow a Normal distribution with ",
              "expectation ", \mmlm([round(0)], Mu), " and ",
              "standard deviation ", \mmlm([round(0)], [Sigma, "."]),
              "A table of the standard ",
              "Normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "How many realizations are ",
                  "below ", \mmlm([round(0)], [X, "?"])
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

% z-transformation and Normal distribution
intermediate(_, item).
start(ztrans, item(x, mu, sigma)) :-
    init(ztrans).

expert(ztrans, stage(2), From, To, [step(expert, allinone, [])]) :-
    From = item(X, Mu, Sigma),
    To = { '<-'(z, frac(X - Mu, Sigma)) ;
           '<-'(p, pnorm(z)) ; 
           p
         }.

feedback(ztrans, allinone, [], _Col, FB) :-
    FB = [ "Everything done correctly."].

hint(ztrans, allinone, [], _Col, FB) :-
    FB = [ "Try to do everything correctly."].

