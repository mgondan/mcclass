% Binomial test (power)
:- module(powbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- http_handler(mcclass(powbinom), handler(powbinom), []).

:- use_module(navbar).
navbar:page(powbinom, "Power test").

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

mathml_hook(p0, sub(pi, 0), p1).
mathml_hook(n, 'N').

interval:r_hook(alpha).
interval:r_hook(n).
interval:r_hook(p0).
interval:r_hook(p1).
interval:r_hook(k).
interval:r_hook(uqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(lqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(tail(_Tail, _K)).
interval:r_hook(arg(_Arg, _K)).
interval:r_hook(cbinom(_Alpha, _Size, _Prob, _Tail, _Arg)).
interval:r_hook(pbinom(_Q, _Size, _Prob)).
interval:r_hook(pbinom(_Q, _Size, _Prob, _Tail)).

render(item(Alpha, N, P0, P1), Form) -->
    { option(resp(R), Form, '#'),
      binomtable(N, P0, P1, Caption, Rows, Cols, Cells)
    },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'),
              [ "Consider a clinical study with ", \mmlm(n = r(N)), " ",
                "patients testing a new treatment. The variable ", \mmlm('X'), " represents the ",
                "number of therapeutic successes in the sample. We assume ",
                "that the successes occur independently, and under the null ",
                "hypothesis, the success probability is ", \mmlm(r(P0)), " ",
                "in all patients. Under the alternative hypothesis, we hope ",
                "that the success probability is ", \mmlm([r(P1), "."]), "The ",
                "binomial probabilities are given in the table below."
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
              [ "What is the power of the test at the one-tailed ",
                "significance level of ", \mmlm([alpha = r(Alpha), "?"])
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
start(item(alpha, n, p0, p1)).

% This is a problem that involves the binomial test
intermediate(binom).
expert(stage(2), From, To, [step(expert, problem, [])]) :-
    From = item(Alpha, N, P0, P1),
    To   = binom(Alpha, N, P0, P1).

feedback(problem, [], _Col, Feed) =>
    Feed = [ "Correctly identified the problem as a binomial test." ].

hint(problem, [], _Col, Hint) =>
    Hint = [ "This problem involves the binomial test." ].

% Upper tail of the binomial distribution
expert(stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0, P1),
    To   = binom(Alpha, N, P0, P1, tail("upper", k), arg("min", k > N*P0)).

feedback(upper, [], _Col, Feed)
 => Feed = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(upper, [], _Col, Hint)
 => Hint = [ "The upper tail of the binomial distribution is needed." ].

% Lower tail of the binomial distribution
buggy(stage(2), From, To, [step(buggy, lower, [])]) :-
    From = binom(Alpha, N, P0, P1),
    To   = binom(Alpha, N, P0, P1, instead(lower, tail("lower", k), tail("upper", k)), instead(lower, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower, [], _Col, Feed)
 => Feed = [ "The result matches the lower tail of the binomial distribution." ].

hint(lower, [], _Col, Hint)
 => Hint = [ "Do not select the lower tail of the binomial distribution." ].

% Critical value based on distribution
expert(stage(2), From, To, [step(expert, dist, [])]) :-
    From = binom(Alpha, N, P0, P1, Tail, Arg),
    To   = cbinom(Alpha, N, P0, Tail, Arg).

feedback(dist, [], _Col, Feed)
 => Feed = [ "Correctly used the critical value of the cumulative ",
             "distribution."
           ].

hint(dist, [], _Col, Hint)
 => Hint = [ "The critical value should be determined on the cumulative ",
             "distribution."
           ].

% Critical value based on density
buggy(stage(3), From, To, [step(buggy, dens, [K])]) :-
    From = tail(Tail, K),
    member(Tail, ["upper", "lower"]),
    To = instead(dens, tail("equal", K), tail("upper", K)).

feedback(dens, [K], Col, Feed)
 => Feed = [ "The result matches the critical value based on the binomial ",
             "probability, ", \mmlm(Col, [fn(sub('P', "Bi"), [color(dens, tail("equal", K))]), "."]),
             "Please report the critical value based on the cumulative ",
             "distribution, ", \mmlm(Col, [fn(sub('P', "Bi"), [tail("upper", K)]), "."])
           ].

hint(dens, [_K], _Col, Hint)
 => Hint = [ "Make sure to use the cumulative binomial distribution to ",
             "determine the critical value."
           ].

% Helper function(s)
binomtable(N, P0, P1, Caption, Rows, Cols, Cells) :-
    r_task(lqbinom(0.05, N, P0), L),
    r_task(uqbinom(0.05, N, P1), H),
    Caption = [em("Table 1. "), "Binomial probabilities"],
    Cols = [\mmlm(k), \mmlm(dbinom(k, N = r(N), p0 = r(P0))), \mmlm(dbinom(k, N = r(N), p1 = r(P1)))],
    % lower tail
    L0 is L - 1,
    FirstRow = \mmlm([0, "...", L0]),
    FirstCell0 = \mmlm([digits=3], r(pbinom(L0, N, P0))),
    FirstCell1 = \mmlm([digits=3], r(pbinom(L0, N, P1))),
    % middle range
    findall(\mmlm(R), between(L, H, R), MiddleRows),
    findall([\mmlm([digits=3], r(dbinom(D, N, P0))), \mmlm([digits=3], r(dbinom(D, N, P1)))], between(L, H, D), MiddleCells),
    % upper tail
    HN is H + 1,
    LastRow = \mmlm([HN, "...", N]),
    LastCell0 = \mmlm([digits=3], r(pbinom(H, N, P0, 'lower.tail'='FALSE'))), % H not HN
    LastCell1 = \mmlm([digits=3], r(pbinom(H, N, P1, 'lower.tail'='FALSE'))),
    append([[FirstRow], MiddleRows, [LastRow]], Rows),
    append([[[FirstCell0, FirstCell1]], MiddleCells, [[LastCell0, LastCell1]]], Cells).

