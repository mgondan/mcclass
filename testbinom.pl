:- module(testbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(interval/interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(testbinom, "Binomial test").
label(critical, "Critical value").
label(powbinom, "Power").
label(pval, [math(mi(p)), "-value"]).

task(critical).
task(powbinom).
task(pval).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(p0, subscript(pi, 0)).
math_hook(p1, subscript(pi, 1)).
math_hook(n, 'N').
math_hook(tail("upper", K), 'X' >= K).
math_hook(tail("lower", K), 'X' =< K).
math_hook(tail("densi", K), 'X' = K).
math_hook(cbinom(Alpha, N, Pi, Tail, MinArg), M) :-
    M = (nodot(MinArg, fn(subscript('P', "Bi"), ([Tail] ; [N, Pi])) =< Alpha)).
math_hook(pbinom1(_K, N, Pi, Tail), M) :-
    M = (fn(subscript('P', "Bi"), ([Tail] ; [N, Pi]))).
math_hook(tail("upper"), 'X' >= k).
math_hook(tail("lower"), 'X' =< k).
math_hook(tail("densi"), 'X' = k).

% R definitions
r_hook(alpha).
r_hook(n).
r_hook(p0).
r_hook(p1).
r_hook(c).
r_hook(k).

% Task description
render(Flags)
--> { start(item(Alpha, N, P0, P1, K)),
      binomtable(Flags, Alpha, N, P0, P1, Caption, Rows, Cols, Cells)
    },
    html(
        div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Binary outcomes"),
            p(class('card-text'),
              [ "Consider a clinical study with ", \mmlm(Flags, n = r(N)), " patients ",
                "testing a new treatment. We assume that the success ",
                "probability is ", \mmlm(Flags, p0 = r(P0)), " under the null ",
                "hypothesis, and that successes occur independently in all ",
                "patients. Under the alternative hypothesis, we hope ",
                "that the success probability is ",
                span(class('text-nowrap'), [\mmlm(Flags, p1 = r(P1)), "."]), 
                " At the end of the treatment, ", \mmlm(Flags, k = r(K)), 
                " successes are reported among the patients.",
                " The binomial probabilities are given in the tables below."
              ]),
            div(class(container),
              div(class("row justify-content-md-center"),
                div(class("col-6"), \htmltable(Caption, Rows, Cols, Cells))))
          ]))).

% Question for the critical value
task(Flags, critical)
--> { start(item(Alpha, _N, _P0, _P1, _K)),
      session_data(resp(testbinom, critical, Resp), resp(testbinom, critical, '#'))
    },
    html(\htmlform([ "How many successes are needed to rule out the null ",
      "hypothesis at the one-tailed significance level ",
      "of ", \mmlm(Flags, alpha = r(Alpha)), "?"], critical, Resp)).

% Question for the power   
task(Flags, powbinom)
--> { start(item(Alpha, _N, _P0, _P1, _K)),
      session_data(resp(testbinom, powbinom, Resp), resp(testbinom, powbinom, '#.##'))
    },
    html(\htmlform([ "What is the power of the test at the one-tailed ",
        "significance level of ",
        nowrap([\mmlm(Flags, alpha = r(Alpha)), "?"])],
        powbinom, Resp)).

% Question for the p-value  
task(Flags, pval)
--> { start(item(Alpha, _N, _P0, _P1, _K)),
      session_data(resp(testbinom, pval, Resp), resp(testbinom, pval, '#.##'))
    },
    html(\htmlform([ "What is the ", nowrap([\mmlm(Flags, p), "-value"]), " of the test at the one-tailed ",
        "significance level of ",
        nowrap([\mmlm(Flags, alpha = r(Alpha)), "?"])],
        pval, Resp)).

start(item(alpha, n, p0, p1, k)).

%
% Expert rules for the critical value task 
%
intermediate(critical, item).

% First step: identify as a binomial test  
intermediate(critical, binom).
expert(critical, stage(2), From, To, [step(expert, binom, [])]) :-
    From = item(Alpha, N, P0, _P1, _K),
    To   = { round(binom(Alpha, N, P0)) }.

feedback(binom, [], _Col, F) =>
    F = [ "Correctly identified the problem as a binomial test." ].

hint(binom, [], _Col, H) =>
    H = [ "This problem is solved with a binomial test." ].

% Second step: upper tail of the binomial distribution
expert(critical, stage(2), From, To, [step(expert, upper, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, tail("upper", k), arg("min", k > N*P0)).

feedback(upper, [], _Col, F)
 => F = [ "Correctly selected the upper tail of the binomial distribution." ].

hint(upper, [], _Col, H)
 => H = [ "The upper tail of the binomial distribution is needed." ].


%
% Buggy rules for the critical value task
%
% Buggy rule: lower tail of the binomial distribution
buggy(critical, stage(2), From, To, [step(buggy, lower, [])]) :-
    From = binom(Alpha, N, P0),
    To   = binom(Alpha, N, P0, instead(lower, tail("lower", k), tail("upper", k)),
           instead(lower, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower, [], _Col, F)
 => F = [ "The result matches the lower tail of the binomial ",
          "distribution." 
       ].

hint(lower, [], _Col, H)
 => H = [ "Select the upper tail of the binomial distribution, ",
          "not the lower tail."
        ].

% Buggy rule: critical value based on distribution
expert(critical, stage(2), From, To, [step(expert, dist, [])]) :-
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

% Buggy rule: critical value based on density
buggy(critical, stage(2), From, To, [step(buggy, dens1, [Tail, K])]) :-
    From = tail(Tail, K),
    member(Tail, ["lower", "upper"]),
    To = instead(dens1, tail("densi", K), tail(Tail, K)).

feedback(dens1, [Tail, K], Col, F)
 => F = [ "The result matches the critical value based on the binomial ",
          "probability, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [color(dens1, tail("densi", K))])), "."]), " ",
          "Please report the critical value based on the cumulative ",
          "distribution, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [tail(Tail, K)])), "."])
        ].

hint(dens1, [_Tail, _K], _Col, H)
 => H = [ "Use the cumulative distribution to determine the critical value, ",
          "not the density."
        ].


% 
% Expert rules for the power task
%
% First step: extract the correct information for the critical value and the 
% binomial probability.
intermediate(powbinom, item).
intermediate(powbinom, crit).

expert(powbinom, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(Alpha, N, P0, P1, _K),
    To = { '<-'(c, crit(Alpha, N, P0)) ;
           power(c, N, P1) }.

feedback(problem, [], _Col, F)
 => F = [ "Correctly identified the two steps of the solution." ].

hint(problem, [], _Col, H)
 => H = [ "First, the critical value is determined under the null hypothesis. ",
          "Then, the probability for a significant result is calculated under ",
          "the alternative."
        ].
%
% Second step: determine the critical value based on the upper tail 
%
expert(powbinom, stage(2), From, To, [step(expert, upper1, [])]) :-
    From = crit(Alpha, N, P0),
    To   = crit(Alpha, N, P0, tail("upper", k), arg("min", k > N*P0)).

feedback(upper1, [], _Col, F)
 => F = [ "Correctly determined the critical value from the upper tail of ",
          "the binomial distribution." 
        ].

hint(upper1, [], _Col, H)
 => H = [ "The critical value is determined from the upper tail of the ",
          "binomial distribution." 
        ].

% Third step: determine the critical value based on the cumulative distribution
expert(powbinom, stage(2), From, To, [step(expert, dist1, [])]) :-
    From = crit(Alpha, N, P0, Tail, Arg),
    To   = cbinom(Alpha, N, P0, Tail, Arg).

feedback(dist1, [], _Col, F)
 => F = [ "Correctly calculated the critical value from the cumulative ",
          "distribution."
        ].

hint(dist1, [], _Col, H)
 => H = [ "The critical value should be determined from the cumulative ",
          "distribution."
        ].

% Fourth step: determine the power based on the upper tail
intermediate(powbinom, power).
expert(powbinom, stage(3), From, To, [step(expert, upper2, [])]) :-
    From = power(Crit, N, P1),
    To   = power(Crit, N, P1, tail("upper", Crit)).

feedback(upper2, [], _Col, F)
 => F = [ "Correctly determined the power from the upper tail of the ",
          "distribution."
        ].

hint(upper2, [], _Col, H)
 => H = [ "The power is determined from the upper tail of the binomial ",
          "distribution."
        ].

% Fifth step: determine the power based on the cumulative distribution
expert(powbinom, stage(3), From, To, [step(expert, dist2, [])]) :-
    From = power(Crit, N, P1, Tail),
    To   = pwbinom(Crit, N, P1, Tail).

feedback(dist2, [], _Col, F)
 => F = [ "Correctly calculated the power using the cumulative ",
          "distribution."
        ].

hint(dist2, [], _Col, H)
 => H = [ "The power should be determined using the cumulative ",
          "distribution."
        ].

%
% Buggy rules for the power task
%
% Buggy rule: determine the critical value from the lower tail of the 
% binomial distribution.
buggy(powbinom, stage(2), From, To, [step(buggy, lower1, [])]) :-
    From = crit(Alpha, N, P0),
    To   = crit(Alpha, N, P0, instead(lower1, tail("lower", k), tail("upper", k)),
                instead(lower1, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower1, [], _Col, F)
 => F = [ "The result matches the lower critical value of the binomial ",
          "distribution. Please use the upper tail to determine the ",
          "critical value."
        ].

hint(lower1, [], _Col, H)
 => H = [ "Make sure to determine the critical value from the upper tail ",
          "of the binomial distribution."
        ].

% Buggy rule: critical value based on density (not cumulated)
buggy(powbinom, stage(2), From, To, [step(buggy, dens1, [K])]) :-
    From = tail(Tail, K),
    member(Tail, ["upper", "lower"]),
    To = instead(dens1, tail("densi", K), tail("upper", K)).

feedback(dens1, [K], Col, F)
 => F = [ "The result matches the critical value based on the binomial ",
          "probability, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [color(dens1, tail("densi", K))])), "."]),
          " Please calculate the critical value based on the cumulative ",
          "distribution, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [tail("upper", K)])), "."])
        ].

hint(dens1, [_K], _Col, H)
 => H = [ "Make sure to use the cumulative binomial distribution to ",
          "determine the critical value."
        ].

% Buggy rule: power based on lower tail
buggy(powbinom, stage(3), From, To, [step(buggy, lower2, [])]) :-
    From = power(Crit, N, P1),
    To   = power(Crit, N, P1, instead(lower2, tail("lower", Crit), tail("upper", Crit))).

feedback(lower2, [], _Col, F)
 => F = [ "The result matches the power based on the lower tail of the ",
          "binomial distribution. Please use the upper tail to calculate ",
          "the power."
        ].

hint(lower2, [], _Col, H)
 => H = [ "The power is determined from the upper tail of the binomial ",
          "distribution. Don\u0027t select the lower tail of the binomial distribution."
        ].

% Buggy rule: power based on density
buggy(powbinom, stage(3), From, To, [step(buggy, dens2, [C])]) :-
    From = tail("upper", C),
    To = instead(dens2, tail("densi", C), tail("upper", C)).

feedback(dens2, [C], Col, F)
 => F = [ "The result matches the power based on the binomial probability, ",
          span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [color(dens2, tail("densi", C))])), "."]),
          " Please determine the power based on the cumulative ",
          "distribution, ", 
	  span(class('text-nowrap'), [\mmlm(Col, fn(subscript('P', "Bi"), [tail("upper", C)])), "."])
        ].

hint(dens2, [_], _Col, H)
 => H = [ "Make sure to use the cumulative binomial distribution to ",
          "determine the power."
        ].


%
% Expert rules for the p-value task
%
% First step: recognise the problem as involving a binomial distribution
intermediate(pval, item).
expert(pval, stage(2), From, To, [step(expert, pbinom, [])]) :-
    From = item(Alpha, N, P0, P1, K),
    To =  { '<-'(p, pbinom0(Alpha, N, P0, P1, K)) ;
           pval(p) 
          }.

feedback(pbinom, [], _Col, F)
 => F = [ "Correctly recognized the problem as involving a binomial distribution."
        ].

hint(pbinom, [], _Col, H)
 => H = [ "The problem involves a binomial distribution." ].

% Second step: determine the upper tail of the binomial distribution
intermediate(pval, pbinom0).
expert(pval, stage(2), From, To, [step(expert, tail, [K, P0])]) :-
  From = pbinom0(_Alpha, N, P0, _P1, K),
  To = { pbinom1(K, N, P0, tail("upper")) }.

feedback(tail, [K, P0], Col, F)
 => F = [ "Correctly calculated the probability of having ", \mmlm(Col, k = r(K)),
          " or more successes under the null hypothesis of ", \mmlm(Col, p0 = r(P0)), "."
        ].

hint(tail, [_K, _P0], _Col, H)
 => H = [ "The upper tail of the binomial distribution is needed." ]. 


%
% Buggy rules for the p-value task
%
% Buggy rule: use lower tail instead of upper tail
buggy(pval, stage(2), From, To, [step(buggy, lowertail, [K])]) :-
    From = pbinom0(_Alpha, N, P0, _P1, K),
    To = { pbinom1(K, N, P0, instead(lowertail, tail("lower"), tail("upper"))) }.

feedback(lowertail, [K], Col, F)
 => F = [ "The result matches the probability of having ", \mmlm(Col, k = r(K)),
          " or less successes."
        ].

hint(lowertail, [_K], _Col, H)
 => H = [ "Do not select the lower tail of the binomial distribution." 
        ].

% Buggy rule: use density instead of upper tail
buggy(pval, stage(2), From, To, [step(buggy, density, [K])]) :-
    From = pbinom0(_Alpha, N, P0, _P1, K),
    To = { pbinom1(K, N, P0, instead(lowertail, tail("densi"), tail("upper"))) }.

feedback(density, [K], Col, F)
 => F = [ "The result matches the probability of having exactly ", \mmlm(Col, k = r(K)),
          " successes."
        ].

hint(density, [_K], _Col, H)
 => H = [ "Do not use the density." 
        ].

% Buggy rule: use success probability of alternative instead of null hypothesis
buggy(pval, stage(2), From, To, [step(buggy, alternative, [p1])]) :-
    From = p0,
    To = { instead(alternative, p1, p0) }.

feedback(alternative, [P1], Col, F)
 => F = [ "The result matches the probability under the alternative hypothesis of ", \mmlm(Col, p1 = r(P1)), "."
        ].

hint(alternative, [_P1], _Col, H)
 => H = [ "Do not use the success probability under the alternative hypothesis." 
        ].

% Buggy rule: use alpha instead of success probability
buggy(pval, stage(2), From, To, [step(buggy, alpha, [alpha])]) :-
    From = p0,
    To = { instead(alpha, alpha, p0) }.

feedback(alpha, [Alpha], Col, F)
 => F = [ "The result matches the probability using the significance level ", \mmlm(Col, alpha = r(Alpha)), 
          " as the success probability."
        ].

hint(alpha, [_Alpha], Col, H)
 => H = [ "Do not use ", \mmlm(Col, alpha), " as the success probability." 
        ].


% Helper function(s)
binomtable(Flags, Alpha, N, P0, P1, Caption, Rows, Cols, Cells) :-
    interval(r('as.integer'(qbinom(Alpha, N, P0, true))) - 1, L),
    interval(r('as.integer'(qbinom(Alpha, N, P1, false))) + 1, H),
    Caption = [em("Table 1. "), "Binomial probabilities"],
    Cols = [\mmlm(Flags, k), \mmlm(Flags, dbinom(k, N, P0)), \mmlm(Flags, dbinom(k, N, P1))],
    % lower tail
    L0 is L - 1,
    FirstRow = \mmlm(Flags, [0, "...", L0]),
    FirstCell0 = \mmlm([digits=3 | Flags], r(pbinom(L0, N, P0))),
    FirstCell1 = \mmlm([digits=3 | Flags], r(pbinom(L0, N, P1))),
    % middle range
    findall(\mmlm(Flags, R), between(L, H, R), MiddleRows),
    findall([\mmlm([digits=3 | Flags], r(dbinom(D, N, P0))), \mmlm([digits=3], r(dbinom(D, N, P1)))], between(L, H, D), MiddleCells),
    % upper tail
    HN is H + 1,
    LastRow = \mmlm(Flags, [HN, "...", N]),
    LastCell0 = \mmlm([digits=3 | Flags], r(pbinom(H, N, P0, 'lower.tail'=false))), % H not HN
    LastCell1 = \mmlm([digits=3 | Flags], r(pbinom(H, N, P1, 'lower.tail'=false))),
    append([[FirstRow], MiddleRows, [LastRow]], Rows),
    append([[[FirstCell0, FirstCell1]], MiddleCells, [[LastCell0, LastCell1]]], Cells).

