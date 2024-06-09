% Binomial test (critical value)
:- module(qbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(r).
:- use_module(rint).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(qbinom, "Binomial test").
task(amountsuccess).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

math_hook(p0, subscript(pi, 0)).
math_hook(n, 'N').

rint:r_hook(alpha).
rint:r_hook(n).
rint:r_hook(p0).
rint:r_hook(k).
rint:r_hook(uqbinom(_Alpha, _Size, _Prob)).
rint:r_hook(lqbinom(_Alpha, _Size, _Prob)).
rint:r_hook(tail(_Tail, _K)).
interval:hook(arg(A, _K), Res, Flags) :-
  interval:int(A, Res, Flags).
%rint:r_hook(arg(_Arg, _K)).
rint:r_hook(cbinom(_Alpha, _Size, _Prob, _Tail, _Arg)).
rint:r_hook(pbinom(_Q, _Size, _Prob)).
rint:r_hook(pbinom(_Q, _Size, _Prob, _Tail)).

render
--> { start(item(_Alpha, N, P0)), 
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
              div(class("col-6"), \htmltable(Caption, Rows, Cols, Cells))))
        ]))
      ]).

task(amountsuccess)
--> { start(item(Alpha, _N, _P0)),
      session_data(resp(qbinom, amountsuccess, Resp), resp(qbinom, amountsuccess, '#'))
	},
	html(\htmlform([ "How many successes are needed to rule out the null ",
        "hypothesis at the one-tailed significance level ",
        "of ", \mmlm(alpha = r(Alpha)), "?"], amountsuccess, Resp)).

intermediate(amountsuccess, item).
start(item(alpha, n, p0)).

% This is a problem that involves the binomial test 
intermediate(amountsuccess, binom).
expert(amountsuccess, stage(2), From, To, [step(expert, binom, [])]) :-
    From = item(Alpha, N, P0),
    To   = { '<-'(k, binom(Alpha, N, P0)) }.

feedback(binom, [], _Col, Feed) =>
    Feed = [ "Correctly identified the problem as a binomial test." ].

hint(binom, [], _Col, Hint) =>
    Hint = [ "This problem involves the binomial test." ].

% Upper tail of the binomial distribution
expert(amountsuccess, stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, tail("upper", k), arg("min", k > N*P0)).

feedback(upper, [], _Col, Feed)
 => Feed = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(upper, [], _Col, Hint)
 => Hint = [ "The upper tail of the binomial distribution is needed." ].

% Lower tail of the binomial distribution
buggy(amountsuccess, stage(2), From, To, [step(buggy, lower, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, instead(lower, tail("lower", k), tail("upper", k)), instead(lower, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower, [], _Col, Feed)
 => Feed = [ "The result matches the lower tail of the binomial distribution." ].

hint(lower, [], _Col, Hint)
 => Hint = [ "Do not select the lower tail of the binomial distribution." ].

% Critical value based on distribution
expert(amountsuccess, stage(2), From, To, [step(expert, dist, [])]) :-
    From = binom(Alpha, N, P0, Tail, Arg),
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
buggy(amountsuccess, stage(3), From, To, [step(buggy, dens, [K])]) :-
    From = tail(Tail, K),
    member(Tail, ["upper", "lower"]),
    To = instead(dens, tail("equal", K), tail("upper", K)).

feedback(dens, [K], Col, Feed)
 => Feed = [ "The result matches the critical value based on the binomial ",
             "probability, ", \mmlm(Col, [fn(subscript('P', "Bi"), [color(dens, tail("equal", K))]), "."]),
             "Please report the critical value based on the cumulative ",
             "distribution, ", \mmlm(Col, [fn(subscript('P', "Bi"), [tail("upper", K)]), "."])
           ].

hint(dens, [_K], _Col, Hint)
 => Hint = [ "Make sure to use the cumulative binomial distribution to ",
             "determine the critical value."
           ].

% Helper function(s)
binomtable(N, P0, Caption, Rows, Cols, Cells) :-
    r_topic(lqbinom(0.05, N, P0), L),
    r_topic(uqbinom(0.05, N, P0), H),
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

