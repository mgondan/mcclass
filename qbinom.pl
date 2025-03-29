% Binomial test (critical value)
:- module(qbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(r_session).
:- use_module(interval/interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(qbinom, "Binomial test").
task(amountsuccess).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

mathml:math_hook(p0, subscript(pi, 0)).
mathml:math_hook(n, 'N').
mathml:math_hook(upper(K), M) :-
    M = ('X' >= K).
mathml:math_hook(lower(K), M) :-
    M = ('X' =< K).
mathml:math_hook(densi(K), M) :-
    M = ('X' = K).
mathml:math_hook(cbinom(Alpha, N, Pi, Tail, MinArg), M) :-
    M = (nodot(MinArg, fn(subscript('P', "Bi"), ([Tail] ; [N, Pi])) =< Alpha)).

r_hook(alpha).
r_hook(n).
r_hook(p0).
r_hook(k).

render
--> { start(item(_Alpha, N, P0)), 
      binomtable(N, P0, Caption, Rows, Cols, Cells) 
    },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Phase II clinical study with binary outcome"),
          p(class('card-text'), 
            [ "Consider a clinical study with ", \mmlm(r(N)), " patients. We ",
              "assume that the successes occur independently, and under the ",
              "null hypothesis, the success probability is ", \mmlm(r(P0)), " ",
              "in all patients. The binomial probabilities are given below."
            ]),
          div(class(container),	  
            div(class("row justify-content-md-center"),
              div(class("col-6"), 
                \htmltable(Caption, Rows, Cols, Cells))))
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
    To   = { round(binom(Alpha, N, P0)) }.

feedback(binom, [], _Col, F) =>
    F = [ "Correctly identified the problem as a binomial test." ].

hint(binom, [], _Col, H) =>
    H = [ "This problem is solved with a binomial test." ].

% Upper tail of the binomial distribution
expert(amountsuccess, stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, upper(k), arg("min", k > N*P0)).

feedback(upper, [], _Col, F)
 => F = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(upper, [], _Col, H)
 => H = [ "The upper tail of the binomial distribution is needed." ].

% Lower tail of the binomial distribution
buggy(amountsuccess, stage(2), From, To, [step(buggy, lower, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, instead(lower, lower(k), upper(k)),
           instead(lower, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower, [], _Col, F)
 => F = [ "The result matches the lower tail of the binomial ",
          "distribution." 
        ].

hint(lower, [], _Col, H)
 => H = [ "Select the upper tail of the binomial distribution." ].

% Critical value based on distribution
expert(amountsuccess, stage(2), From, To, [step(expert, dist, [])]) :-
    From = binom(Alpha, N, P0, Tail, Arg),
    To   = cbinom(Alpha, N, P0, Tail, Arg).

feedback(dist, [], _Col, F)
 => F = [ "Correctly used the critical value of the cumulative ",
          "distribution."
        ].

hint(dist, [], _Col, H)
 => H = [ "The critical value is determined with help of the cumulative ",
          "distribution."
        ].

% Critical value based on density
buggy(amountsuccess, stage(3), From, To, [step(buggy, dens1, [K])]) :-
    From = upper(K),
    To = instead(dens1, densi(K), upper(K)).

feedback(dens1, [K], Col, F)
 => F = [ "The result matches the critical value based on the binomial ",
          "probability, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [color(dens1, densi(K))])), "."]), " ",
          "Please report the critical value based on the cumulative ",
          "distribution, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [upper(K)])), "."])
        ].

hint(dens1, [_K], _Col, H)
 => H = [ "Use the cumulative distribution to determine the critical value, ",
          "not the density."
        ].

buggy(amountsuccess, stage(3), From, To, [step(buggy, dens2, [K])]) :-
    From = lower(K),
    To = instead(dens2, densi(K), lower(K)).

feedback(dens2, [K], Col, F)
 => F = [ "The result matches the critical value based on the binomial ",
          "probability, ",
          span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [color(dens1, densi(K))])), "."]), " ",
          "Please report the critical value based on the cumulative ",
          "distribution, ",
          span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [lower(K)])), "."])
        ].

hint(dens2, [_K], _Col, H)
 => H = [ "Use the cumulative distribution to determine the critical value, ",
          "not the density"
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
    CellN = \mmlm([digits=3], r(pbinom(H, n, p0, 'lower.tail'=false))),
    append([[Row0], RowsX, [RowN]], Rows),
    append([[[Cell0]], CellsX, [[CellN]]], Cells).

