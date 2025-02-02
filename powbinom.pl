% Binomial test (power)
:- module(powbinom, []).

:- use_module(library(http/html_write)).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- http_handler(mcclass(powbinom), handler(powbinom), []).

:- use_module(navbar).
navbar:page(powbinom, "Power of binomial test").
task(powbinom).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering
math_hook(p0, subscript(pi, 0)).
math_hook(p1, subscript(pi, 1)).
math_hook(crit, c).
math_hook(power, 'Pwr').
math_hook(n, 'N').
math_hook(tail1(Tail, K), tail(Tail, K)).
math_hook(tail2(Tail, K), tail(Tail, K)).

% R definitions
r_hook(alpha).
r_hook(n).
r_hook(p0).
r_hook(p1).
r_hook(k).
r_hook(crit).

% Todo: update definitions to new pattern mcint:r_hook(Name/Arity)
r_hook(uqbinom(_Alpha, _Size, _Prob)).
r_hook(lqbinom(_Alpha, _Size, _Prob)).
r_hook(tail1(_Tail, _K)).
r_hook(tail2(_Tail, _K)).
r_hook(arg(_Arg, _K)).
r_hook(cbinom(_Alpha, _Size, _Prob, _Tail, _Arg)).
r_hook(pwbinom(_Crit, _Size, _Prob, _Tail)).
r_hook(pbinom(_Q, _Size, _Prob)).
r_hook(pbinom(_Q, _Size, _Prob, _Tail)).

/* mcint:int_hook(arg, arg(_, _), _, []).
mcint:arg(A, _K, Res, Flags) :-
  mcint:interval_(A, Res, Flags). */

% Task description
render
--> { start(item(_Alpha, N, P0, P1)),
      binomtable(N, P0, P1, Caption, Rows, Cols, Cells)
    },
    html(
       div(class(card), div(class('card-body'),
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
	      ]))).

task(powbinom)
-->  { start(item(Alpha, _N, _P0, _P1)),
      session_data(resp(powbinom, powbinom, Resp), resp(powbinom, powbinom, '#.##'))
    },
    html(\htmlform([ "What is the power of the test at the one-tailed ",
        "significance level of ", \mmlm([alpha = r(Alpha), "?"])],
        powbinom, Resp)).

% Power-test
intermediate(powbinom, item).
start(item(alpha, n, p0, p1)).

% First Step: Extract the correct information for the critical value and the 
% binomial probability.
intermediate(powbinom, crit).
intermediate(powbinom, power).
expert(powbinom, stage(1), From, To, [step(expert, problem, [])]) :-
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

% Second Step: Determine the critical value from the upper tail of the 
% binomial distribution.
expert(powbinom, stage(2), From, To, [step(expert, upper1, [])]) :-
    From = crit(Alpha, N, P0),
    To   = crit(Alpha, N, P0, tail1("upper", k), arg("min", k > N*P0)).

feedback(upper1, [], _Col, Feed)
 => Feed = [ "Correctly determined the critical value from the upper tail of ",
             "the binomial distribution." 
           ].

hint(upper1, [], _Col, Hint)
 => Hint = [ "The critical value is determined from the upper tail of the ",
             "binomial distribution." 
           ].

% Third Step: Determine the critical value based on the cumulative distribution
expert(powbinom, stage(2), From, To, [step(expert, dist1, [])]) :-
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

% Fourth step: Determine the power based on upper tail
expert(powbinom, stage(2), From, To, [step(expert, upper2, [])]) :-
    From = power(Crit, N, P1),
    To   = power(Crit, N, P1, tail2("upper", Crit)).

feedback(upper2, [], _Col, Feed)
 => Feed = [ "Correctly selected the upper tail of cumulative distribution ",
             "for the Power."
           ].

hint(upper2, [], _Col, Hint)
 => Hint = [ "The Power is determined from the upper tail of the binomial ",
             "distribution."
           ].

% Fifth step: Power based on cumulative distribution
expert(powbinom, stage(2), From, To, [step(expert, dist2, [])]) :-
    From = power(Crit, N, P1, Tail),
    To   = pwbinom(Crit, N, P1, Tail).

feedback(dist2, [], _Col, Feed)
 => Feed = [ "Correctly calculated the Power using the cumulative ",
             "distribution."
           ].

hint(dist2, [], _Col, Hint)
 => Hint = [ "The Power should be determined using the cumulative ",
             "distribution."
           ].




% Buggy-Rule: Determine the critical value from the lower tail of the 
% binomial distribution.
buggy(powbinom, stage(2), From, To, [step(buggy, lower1, [])]) :-
    From = crit(Alpha, N, P0),
    To   = crit(Alpha, N, P0, instead(lower1, tail1("lower", k), tail1("upper", k)),
                instead(lower1, arg("max", k < N*P0), arg("min", k > N*P0))).

feedback(lower1, [], _Col, Feed)
 => Feed = [ "The result matches the Power obtained from the lower critical ",
             "value of the binomial distribution. Please use the upper critical ",
	     "value to determine the Power."
           ].

hint(lower1, [], _Col, Hint)
 => Hint = [ "Make sure to determine the critical value from the upper tail ",
             "of the binomial distribution."
           ].

% Buggy- Rule: Critical value based on density = not cumulated
buggy(powbinom, stage(2), From, To, [step(buggy, dens1, [K])]) :-
    From = tail1(Tail, K),
    member(Tail, ["upper", "lower"]),
    To = instead(dens1, tail1("equal", K), tail1("upper", K)).

feedback(dens1, [K], Col, Feed)
 => Feed = [ "The result matches the critical value based on the binomial ",
             "probability, ", \mmlm(Col, [fn(subscript('P', "Bi"), [color(dens1, tail1("equal", K))]), "."]),
             "Please calculate the critical value based on the cumulative ",
             "distribution, ", \mmlm(Col, [fn(subscript('P', "Bi"), [tail1("upper", K)]), "."])
           ].

hint(dens1, [_K], _Col, Hint)
 => Hint = [ "Make sure to use the cumulative binomial distribution to ",
             "determine the critical value."
           ].

% Buggy-Rule: Power based on lower tail (wrong tail for power)
buggy(powbinom, stage(2), From, To, [step(buggy, lower2, [])]) :-
    From = power(Crit, N, P1),
    To   = power(Crit, N, P1, instead(lower2, tail2("lower", k), tail2("upper", k))).

feedback(lower2, [], _Col, Feed)
 => Feed = ["The result matches the Power based on the lower critical ",
            "value of the binomial distribution. Please use the upper critical ",
	    "value to determine the Power."
	   ].

hint(lower2, [], _Col, Hint)
 => Hint = [ "The power is determined from the upper tail of the binomial ",
             "distribution. Don\u0027t select the lower tail of the binomial distribution."
           ].

% Buggy- Rule: Critical value based on density
buggy(powbinom, stage(2), From, To, [step(buggy, dens2, [C])]) :-
    From = tail2(Tail, C),
    member(Tail, ["upper", "lower"]),
    To = instead(dens2, tail2("equal", C), tail2("upper", C)).

feedback(dens2, [C], Col, Feed)
 => Feed = [ "The result matches the Power based on the binomial probability, ",
             \mmlm(Col, [fn(subscript('P', "Bi"), [color(dens2, tail2("equal", C))]), "."]),
             "Please report the power based on the cumulative ",
             "distribution, ", \mmlm(Col, [fn(subscript('P', "Bi"), [tail2("upper", C)]), "."])
           ].

hint(dens2, [_C], _Col, Hint)
 => Hint = [ "Make sure to use the cumulative binomial distribution to ",
             "determine the power."
           ].




% Helper function(s)
binomtable(N, P0, P1, Caption, Rows, Cols, Cells) :-
    r_topic(lqbinom(0.05, N, P0), L),
    r_topic(uqbinom(0.05, N, P1), H),
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
    LastCell0 = \mmlm([digits=3], r(pbinom(H, N, P0, 'lower.tail'=false))), % H not HN
    LastCell1 = \mmlm([digits=3], r(pbinom(H, N, P1, 'lower.tail'=false))),
    append([[FirstRow], MiddleRows, [LastRow]], Rows),
    append([[[FirstCell0, FirstCell1]], MiddleCells, [[LastCell0, LastCell1]]], Cells).

