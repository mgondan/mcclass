% Binomial test (critical value)
:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

mathml:hook(Flags, p0, [task(qbinom) | Flags], sub(pi, 0)).
mathml:hook(Flags, n, [task(qbinom) | Flags], 'N').

interval:r_hook(alpha).
interval:r_hook(n).
interval:r_hook(p0).
interval:r_hook(uqbinom(_Alpha, _Size, _Prob)).

render(qbinom, item(Alpha, N, P0), Form) -->
    { option(resp(R), Form, '#'),
      binomtable(N, P0, Caption, Rows, Cols, Cells)
    }, 
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'), 
              [ "Consider a clinical study with ", \mmlm([task(qbinom)], n = r(N)), " ",
                "patients. The variable ", \mmlm([task(qbinom)], 'X'), " represents the ",
                "number of therapeutic successes in the sample. We assume ",
                "that the successes occur independently, and under the null ",
                "hypothesis, the success probability is ", \mmlm([task(qbinom)], r(P0)), " ",
                "in all patients. The binomial probabilities are given in ",
                "the table below."
	          ]),
	        div(class(container),
	          div(class("row justify-content-md-center"),
	            div(class("col-6"),
	              \htmltable(Caption, Rows, Cols, Cells))))
	      ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), 
              "Question"]),
            p(class('card-text'),
              [ "How many successes are needed to rule out the null ",
                "hypothesis at the one-tailed significance level ",
                "of ", \mmlm([task(qbinom)], alpha = r(Alpha)), "?"
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

intermediate(qbinom, item).
start(qbinom, item(alpha, n, p0)).

% This is a problem that involves the binomial test 
intermediate(qbinom, binom).
expert(qbinom, stage(2), From, To, [step(expert, binomial, [])]) :-
    From = item(Alpha, N, P0),
    To   = binom(Alpha, N, P0).

feedback(qbinom, binomial, [], _Col, Feed) =>
    Feed = [ "Correctly identified the problem as a binomial test." ].

hint(qbinom, binomial, [], _Col, Hint) =>
    Hint = [ "This problem involves the binomial test." ].

% Upper tail of the binomial distribution
expert(qbinom, stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0),
    To   = uqbinom(Alpha, N, P0).

feedback(qbinom, upper, [], _Col, Feed) =>
    Feed = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(qbinom, upper, [], _Col, Hint) =>
    Hint = [ "The upper tail of the binomial distribution is needed." ].

% Helper function(s)
binomtable(N, P0, Caption, Rows, Cols, Cells) :-
    r_task(qbinom, 'as.integer'(qbinom(0.05, N, P0) - 1), L),
    r_task(qbinom, 'as.integer'(qbinom(0.95, N, P0) + 1), H),
    Caption = [em("Table 1. "), "Binomial probabilities"],
    findall(\mmlm([task(qbinom)], R), between(L, H, R), Rows),
    Cols = [\mmlm([task(qbinom)], k), \mmlm([task(qbinom)], dbinom(k, n = r(N), p0 = r(P0)))],
    findall([\mmlm([task(qbinom)], r(dbinom(D, n, p0)))], between(L, H, D), Cells).

