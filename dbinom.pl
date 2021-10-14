% Binomial density

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(dbinom) :-
    data(dbinom).

data(dbinom) :-
    r_init,
    {|r||
        k  <- 14
	n  <- 26
	p0 <- 0.6
    |}.

mathml:hook(Flags, n, Flags, 'N').
mathml:hook(Flags, p0, Flags, pi).

% Render R result - check if this is needed
mathml:hook(Flags, r(Expr), Flags, Res) :-
    R <- Expr,
    [Res] = R,
    number(Res).

render(dbinom, item(K, N, P0), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), 
              "Binary outcomes"),
            p(class('card-text'),
            [ "Consider a clinical study with ", \mmlm(N = r(N)), " ",
              "patients. We assume that the success probability ",
              "is ", \mmlm(r(P0)), " in all patients, and that the successes ",
              "occur independently."
	        ])
          ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), 
              "Question"]),
            p(class('card-text'),
            [ "What is the probability for exactly ", \mmlm(r(K)), " ",
                "successes?" 
            ]), 
            form([class(form), method('POST'), action('#dbinom-dbinom')],
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

intermediate(dbinom, item).
start(dbinom, item(k, n, p0)).

% Recognise as a binomial density
expert(dbinom, stage(2), From, To, [step(expert, dens, [])]) :-
    From = item(K, N, Pi),
    To   = dbinom(K, N, Pi).

feedback(dbinom, dens, [], _Col, FB) :-
    FB = [ "Correctly recognised the problem as a binomial probability." ].

hint(dbinom, dens, [], _Col, FB) :-
    FB = [ "This is a binomial probability." ].

