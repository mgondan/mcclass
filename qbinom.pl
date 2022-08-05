% Binomial test (critical value)
:- module(qbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(qbinom, "Binomial test").

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

mathml_hook(p0, sub(pi, 0)).
mathml_hook(n, 'N').

interval:r_hook(alpha).
interval:r_hook(n).
interval:r_hook(p0).
interval:r_hook(uqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(lqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(pbinom(_Q, _Size, _Prob)).
interval:r_hook(pbinom(_Q, _Size, _Prob, _Tail)).

render(item(Alpha, N, P0), Form) -->
    { option(resp(R), Form, '#'),
      binomtable(N, P0, Caption, Rows, Cols, Cells)
    }, 
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'), 
              [ "Consider a clinical study with ", \mmlm(n = r(N)), " ",
                "patients. The variable ", \mmlm('X'), " represents the ",
                "number of therapeutic successes in the sample. We assume ",
                "that the successes occur independently, and under the null ",
                "hypothesis, the success probability is ", \mmlm(r(P0)), " ",
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
                "of ", \mmlm(alpha = r(Alpha)), "?"
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

intermediate(item).
start(item(alpha, n, p0)).

% This is a problem that involves the binomial test 
intermediate(binom).
expert(stage(2), From, To, [step(expert, binomial, [])]) :-
    From = item(Alpha, N, P0),
    To   = binom(Alpha, N, P0).

feedback(binomial, [], _Col, Feed) =>
    Feed = [ "Correctly identified the problem as a binomial test." ].

hint(binomial, [], _Col, Hint) =>
    Hint = [ "This problem involves the binomial test." ].

% Upper tail of the binomial distribution
expert(stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0),
    To   = uqbinom(Alpha, N, P0).

feedback(upper, [], _Col, Feed) =>
    Feed = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(upper, [], _Col, Hint) =>
    Hint = [ "The upper tail of the binomial distribution is needed." ].

% Lower tail of the binomial distribution
buggy(stage(2), From, To, [step(buggy, lower, [])]) :-
    From = binom(Alpha, N, P0),
    To   = instead(lower, lqbinom(Alpha, N, P0), uqbinom(Alpha, N, P0)).

feedback(lower, [], _Col, Feed) =>
    Feed = [ "The result matches the lower tail of the binomial distribution." ].

hint(lower, [], _Col, Hint) =>
    Hint = [ "Do not select the lower tail of the binomial distribution." ].

% Helper function(s)
binomtable(N, P0, Caption, Rows, Cols, Cells) :-
    r_task('as.integer'(qbinom(0.05, N, P0) - 1), L),
    r_task('as.integer'(qbinom(0.95, N, P0) + 1), H),
    Caption = [em("Table 1. "), "Binomial probabilities"],
    Cols = [\mmlm(k), \mmlm(dbinom(k, n = r(N), p0 = r(P0)))],
    % lower tail
    L0 is L - 1,
    Row0 = \mmlm([0, "...", L0]),
    Cell0 = \mmlm([digits=3], r(pbinom(L0, n, p0))),
    % middle range
    findall(\mmlm(R), between(L, H, R), RowsX),
    findall([\mmlm([digits=3], r(dbinom(D, n, p0)))], between(L, H, D), CellsX),
    % upper tail
    HN is H + 1,
    RowN = \mmlm([HN, "...", N]),
    CellN = \mmlm([digits=3], r(pbinom(H, n, p0, 'lower.tail'='FALSE'))), % H not HN
    append([[Row0], RowsX, [RowN]], Rows),
    append([[[Cell0]], CellsX, [[CellN]]], Cells).

