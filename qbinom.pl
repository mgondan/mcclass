% Binomial test (critical value)
:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(qbinom) :-
    r_session_source(qbinom).

mathml:math_hook(Flags, p0, Flags, sub(pi, 0)).
mathml:math_hook(Flags, n, Flags, 'N').

% Render R result - check if this is needed
mathml:hook(Flags, r(Expr), Flags, Res) :-
    r_session(Expr, R),
    #(Res) = R,
    number(Res).

render(qbinom, item(alpha, n, p0), Form) -->
    { option(resp(R), Form, '#'),
      r_session(alpha, #(Alpha)),
      r_session(n, #(N)),
      r_session(p0, #(P0))
    }, 
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'), 
              [ "Consider a clinical study with ", \mmlm(n = N), " ",
                "patients. The variable ", \mmlm('X'), " represents the number ",
                "of therapeutic successes in the sample. We assume that the ",
		"successes occur independently, and under the null ",
		"hypothesis, the success probability is ", \mmlm(P0), " in ",
		"all patients. The binomial probabilities are given in the ",
		"table below."
	      ]),
	    div(class(container),
	       div(class("row justify-content-md-center"),
	          div(class("col-6"),
	            \htmltable(
	              [ em("Table 1. "), "Binomial probabilities" ],
                      [ \mmlm(10), \mmlm(11), \mmlm(12) ],
                      [ \mmlm(dbinom(k, n, p0 = P0)) ],
                      [ [ \mmlm(r(dbinom(10, n, p0))) ],
		        [ \mmlm(r(dbinom(10, n, p0))) ],
			[ \mmlm(r(dbinom(10, n, p0))) ]
                      ]))))
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), 
              "Question"]),
            p(class('card-text'),
              [ "How many successes are needed to rule out the null ",
                "hypothesis at the one-tailed significance level ",
                "of ", \mmlm(alpha = Alpha), "?"
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

% Recognise as a binomial density
expert(qbinom, stage(2), From, To, [step(expert, quantile, [])]) :-
    From = item(Alpha, N, P0),
    To   = uqbinom(Alpha, N, prob=P0).

feedback(qbinom, quantile, [], _Col, Feed) :-
    Feed = [ "Correctly recognised the problem as a binomial test." ].

hint(qbinom, quantile, [], _Col, Hint) :-
    Hint = [ "This is a binomial test." ].
