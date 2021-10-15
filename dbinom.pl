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

        bernoulli <- function(k, n, p0)
        {
          successes(k, p0) * failures(n - k, 1 - p0)
        }

        successes <- function(k, p0)
        {
          p0^k
        }

        # this may change
        failures <- function(nk, q0)
        {
          q0^nk
        }
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
intermediate(dbinom, dbinom).
expert(dbinom, stage(2), From, To, [step(expert, dens, [])]) :-
    From = item(K, N, P0),
    To   = dbinom(K, N, P0).

feedback(dbinom, dens, [], _Col, Feed) :-
    Feed = [ "Correctly recognised the problem as a binomial probability." ].

hint(dbinom, dens, [], _Col, Hint) :-
    Hint = [ "This is a binomial probability." ].

% Convert to product
intermediate(dbinom, bernoulli).
expert(dbinom, stage(2), From, To, [step(expert, prod, [K, N, P0])]) :-
    From = dbinom(K, N, P0),
    To   = choose(N, K) * bernoulli(K, N, P0).

feedback(dbinom, prod, [_K, _N, _P0], _Col, Feed) :-
    Feed = [ "Correctly identified the formula for the binomial probability." ].

hint(dbinom, prod, [K, N, P0], Col, Hint) :-
    Hint = [ "The formula for the binomial probability ",
             "is ", \mmlm(Col, choose(N, K) * bernoulli(K, N, P0)), "."
           ].

% Successes and failures
intermediate(dbinom, successes).
expert(dbinom, stage(2), From, To, [step(expert, bern, [K, N, P0])]) :-
    From = bernoulli(K, N, P0),
    To   = successes(K, P0) * failures(N - K, 1 - P0).

feedback(dbinom, bern, [K, N, _P0], Col, Feed) :-
    Feed = [ "Correctly determined the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ", 
             "and ", \mmlm(Col, N - K), " failures."
           ].

hint(dbinom, bern, [K, N, _P0], Col, Hint) :-
    Hint = [ "Determine the probability for a sequence ",
             "of ", \mmlm(Col, K), " successes ",
             "and ", \mmlm(Col, N - K), "failures."
           ].

% Successes
expert(dbinom, stage(2), From, To, [step(expert, success, [K, P0])]) :-
    From = successes(K, P0),
    To   = P0^K.

feedback(dbinom, success, [K, _P0], Col, Feed) :-
    Feed = [ "Correctly determined the probability for ", \mml(Col, K), " ",
             "independent successes."
           ].

hint(dbinom, success, [K, _P0], Col, Hint) :-
    Hint = [ "Determine the probability for ", \mml(Col, K), " independent ",
             "successes."
           ].

% Failures

