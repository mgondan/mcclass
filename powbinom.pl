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

mathml_hook(p0, sub(pi, 0)).
mathml_hook(p1, sub(pi, 1)).
mathml_hook(crit, c).
mathml_hook(power, 'Pwr').
mathml_hook(n, 'N').

interval:r_hook(alpha).
interval:r_hook(n).
interval:r_hook(p0).
interval:r_hook(p1).
interval:r_hook(k).
interval:r_hook(crit).
interval:r_hook(uqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(lqbinom(_Alpha, _Size, _Prob)).
interval:r_hook(tail(_Tail, _K)).
interval:r_hook(arg(_Arg, _K)).
interval:r_hook(cbinom(_Alpha, _Size, _Prob, _Tail, _Arg)).
interval:r_hook(pwbinom(_Crit, _Size, _Prob, _Tail)).
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

% This is a problem that involves two steps, critical value and binomial probability
intermediate(crit).
intermediate(power).
expert(stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Alpha, N, P0, P1),
    To = { '<-'(crit, crit(Alpha, N, P0)) ;
           '<-'(power, power(crit, N, P1))
         }.

feedback(problem, [], _Col, Feed) =>
    Feed = [ "Correctly identified the two steps of the problem." ].

hint(problem, [], _Col, Hint) =>
    Hint = [ "In the first step, the critical value is determined. In the ",
             "second step, the probability for a significant result is ",
             "calculated."
           ].

% Upper tail of the binomial distribution
expert(stage(2), From, To, [step(expert, upper1, [])]) :-
    From = crit(Alpha, N, P0),
    To   = crit(Alpha, N, P0, tail("upper", k), arg("min", k > N*P0)).

feedback(upper1, [], _Col, Feed)
 => Feed = [ "Correctly determined the critical value from the upper tail of ",
             "the binomial distribution." 
           ].

hint(upper1, [], _Col, Hint)
 => Hint = [ "The critical value is determined from the upper tail of the ",
             "binomial distribution." 
           ].

% Critical value based on cumulative distribution
expert(stage(2), From, To, [step(expert, dist1, [])]) :-
    From = crit(Alpha, N, P0, Tail, Arg),
    To   = cbinom(Alpha, N, P0, Tail, Arg).

feedback(dist1, [], _Col, Feed)
 => Feed = [ "Correctly used the critical value of the cumulative ",
             "distribution."
           ].

hint(dist1, [], _Col, Hint)
 => Hint = [ "The critical value should be determined on the cumulative ",
             "distribution."
           ].

% Power based on upper tail
expert(stage(3), From, To, [step(expert, upper2, [])]) :-
    From = power(Alpha, N, P1),
    To   = power(Alpha, N, P1, tail("upper", k)).

feedback(upper2, [], _Col, Feed)
 => Feed = [ "Correctly selected the upper tail of cumulative distribution ",
             "for the power."
           ].

hint(upper2, [], _Col, Hint)
 => Hint = [ "The power is determined from the upper tail of the binomial ",
             "distribution."
           ].

% Power based on cumulative distribution
expert(stage(3), From, To, [step(expert, dist2, [])]) :-
    From = power(Alpha, N, P1, Tail),
    To   = pwbinom(Alpha, N, P1, Tail).

feedback(dist2, [], _Col, Feed)
 => Feed = [ "Correctly calculated the power using the cumulative ",
             "distribution."
           ].

hint(dist2, [], _Col, Hint)
 => Hint = [ "The power should be determined using the cumulative ",
             "distribution."
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

