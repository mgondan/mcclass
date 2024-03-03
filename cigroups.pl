:- module(cigroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(rint).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(cigroups, ["Confidence interval for independent samples"]).
task(cigroups).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(n_mc, subscript(n, "MC")).
math_hook(n_rc, subscript(n, "RC")).
math_hook(mc, overline("MC")).
math_hook(s_mc, subscript(s, "MC")).
math_hook(rc, overline("RC")).
math_hook(s_rc, subscript(s, "RC")).
math_hook(s2p, subscript(s, "pool")^2).
math_hook(alpha, greek("alpha")).

% R definitions
rint:r_hook(n_mc).
rint:r_hook(n_rc).
rint:r_hook(mc).
rint:r_hook(s_mc).
rint:r_hook(rc).
rint:r_hook(s_rc).
rint:r_hook(s2p).
rint:r_hook(t).
rint:r_hook(lo).
rint:r_hook(mu).
rint:r_hook(qt(_P, _DF)).
rint:r_hook(var_pool(_N1, _V1, _N2, _V2)).

% Task description
render
--> {start(item(_RC, _S_RC, N_RC, _MC, _S_MC, N_MC, _Alpha)) },
    html(
       div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "New math teaching concept"),
            p(class('card-text'),
            [ "There are plans to introduce a new concept for teaching ",
              "mathematics nationwide. Prior to this, a school is testing ",
              "whether the new concept improves the students\u0027 understanding ",
              "of the subject matter compared to the previous teaching concept. ", 
	      "In this test phase, ", \mmlm([], N_MC = r(N_MC)),  " students from the model ",
              "class (MC) are taking part in lessons using the new concept and ",
              \mmlm([], N_RC = r(N_RC)), " students from a reference class (RC) are taking ",
              "part in lessons using the existing concept. At the end of the ",
	      "test phase, all students take a standardized mathematics test with ",
	      "the following results."
	     ]),
            div(class('container'),
              div(class("row justify-content-md-center"),
                div(class("col-6"),
                  \htmltable(
                    [ em("Table 1. "), "Observed test scores of the model ",
                      "and reference class." ],
                    [ "Average", "SD" ],
                    [ "", "Model class", "Reference class"],
                    [ [ \mmlm([digits(1)], r(mc)),
                        \mmlm([digits(1)], r(rc)) ],
                      [ \mmlm([digits(1)], r(s_mc)),
                        \mmlm([digits(1)], r(s_rc))]
                    ])))),
            \download(cigroups)
          ]))).

task(cigroups)
--> { start(item(_RC, _S_RC, _N_RC, _MC, _S_MC, _N_MC, Alpha)),
      session_data(resp(cigroups, cigroups, Resp), resp(cigroups, cigroups, '#.# to #.#'))
    },
    html(\htmlform(["Determine the confidence interval for the difference ",
        "between the two group means. The alpha level ",
        "is ", \mmlm([], Alpha = perc(r(Alpha)))], cigroups, Resp)).

% t-test and confidence intervall for independent samples 
intermediate(cigroups, item).
start(item(rc, s_rc, n_rc, mc, s_mc, n_mc, alpha)). 

% First step: Extract the correct information for a t-test for independent 
% samples and the associated confidence interval from the task description
intermediate(cigroups, indep).
expert(cigroups, stage(1), X, Y, [step(expert, problem, [])]) :-
    X = item(RC, S_RC, N_RC, MC, S_MC, N_MC, Alpha),
    Y = { '<-'(s2p, var_pool(S_RC^2, N_RC, S_MC^2, N_MC)) ;
          '<-'(ci, indep(RC, MC, s2p, N_RC, N_MC, Alpha))
         }.

feedback(problem, [], Col, F)
 => F = [ "Correctly recognised the problem as ",
          "a ", \mmlm(Col, hyph(t, "test")), " for independent samples and that ",
          "a confidence interval for the difference of group means has to be calculated." 
        ].

hint(problem, [], Col, H)
 => H = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for independent",
          "samples. Calculate the confidence interval for the difference of group means." 
        ].


% Second step: To calculate the confidence interval for the difference of 
% group means, the pooled variance has to be calculated first. Apply the formula
% for the pooled variance.
% dfrac/2 is a fraction in "display" mode (a bit larger font than normal)
intermediate(cigroups, var_pool).
expert(cigroups, stage(1), X, Y, [step(expert, pooled, [S2P])]) :-
    X = '<-'(S2P, var_pool(S_A^2, N_A, S_B^2, N_B)),
    Y = '<-'(S2P, dfrac((N_A-1) * S_A^2 + (N_B-1) * S_B^2, N_A + N_B - 2)).

feedback(pooled, [S2P], Col, FB)
 => FB = [ "Correctly determined the pooled variance ", \mmlm(Col, [S2P, "."]) ].

hint(pooled, [S2P], Col, FB)
 => FB = [ "The pooled variance ", \mmlm(Col, S2P), " needs to be ",
           "calculated." 
         ].


% Third step: Apply the formula for the confidence interval for the difference
% of group means
intermediate(cigroups, quant).
expert(cigroups, stage(2), X, Y, [step(expert, ci_bounds, [RC, MC, S2P, N_RC, N_MC, Alpha])]) :-
    X = indep(RC, MC, S2P, N_RC, N_MC, Alpha),
    Y = hdrs(pm(RC - MC, dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), sqrt(dot(S2P, frac(1, N_RC) + frac(1, N_MC)))))).

feedback(ci_bounds, [_RC, _MC, _S2P, _N_RC, _N_MC, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for the difference of group means." 
        ].

hint(ci_bounds, [RC, MC, S2P, N_RC, N_MC, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval for the difference of group means is ",
	  \mmlm(Col, [pm(RC - MC, qt(1 - Alpha/2, N_RC + N_MC - 2) * sqrt(dot(S2P, frac(1, N_RC) + frac(1, N_MC)))), "."])
        ].

% Fourth step: Choose the correct quantile of the t-distribution
expert(cigroups, stage(2), X, Y, [step(expert, tquant, [Alpha])]) :-
    X = quant(_RC, _MC, _S2P, N_RC, N_MC, Alpha),
    Y = qt(1 - Alpha/2, N_RC + N_MC - 2).

feedback(tquant, [Alpha], Col, F)
 => F = [ "Correctly used the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          " of the ", \mmlm(Col, hyph(t, "distribution."))
        ].

hint(tquant, [Alpha], Col, H)
 => H = [ "Make sure to use the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          " of the ", \mmlm(Col, hyph(t, "distribution."))
        ].



				
% Buggy-Rule: Use t-statistic instead of t-quantile
buggy(cigroups, stage(2), X, Y, [step(buggy, tstat, [N_RC, N_MC, Alpha])]) :-
    X = quant(RC, MC, S2P, N_RC, N_MC, Alpha),
    P = denote(t, dfrac(RC - MC, sqrt(S2P * (frac(1, N_RC) + frac(1, N_MC)))), ["the observed", space, t, "-statistic."]),
    Y = P. 

feedback(tstat, [N_RC, N_MC, Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the observed ",
          \mmlm(Col, hyph(t, "statistic.")), " Please use the quantile ",
           "of the ", \mmlm(Col, hyph(t, "distribution ")), " ",
	   \mmlm(Col, color(qt, qt(1 - Alpha/2, N_RC + N_MC - 2))), " instead."
        ].

hint(tstat, [_N_RC, _N_MC, _Alpha], Col, H)
 => H = [ "Do not insert the observed ", \mmlm(Col, hyph(t, "statistic ")),
          "into the formula for the confidence interval. Use the quantile of ", 
	  "the ", \mmlm(Col, hyph(t, "distribution")), " instead."
        ].


% Buggy-Rule: Use z-quantile instead of t-quantile. 
% This rule may be dropped because we might not be able to distinguish the results.
buggy(cigroups, stage(2), X, Y, [step(buggy, qt, [N_RC, N_MC, Alpha])]) :-
    X = quant(_RC, _MC, _S2P, N_RC, N_MC, Alpha),
    Y = instead(qt, qnorm(1 - Alpha/2) , qt(1 - Alpha/2, N_RC + N_MC - 2)).

feedback(qt, [N_RC, N_MC, Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the standard ",
          "normal distribution. ",
          "Please insert the quantile of the ", \mmlm(Col, hyph(t, "distribution")), " ", 
          \mmlm(Col, color(qt, qt(1 - Alpha/2, N_RC + N_MC - 2))), " into ",
          "the formula for the confidence interval."
        ].

hint(qt, [_N_RC, _N_MC, _Alpha], Col, H)
 => H = [ "Do not insert the quantile of the ", \mmlm(Col, hyph(z, "distribution ")), 
          "into the formula for the confidence interval. Use the quantile of the ", 			
	  \mmlm(Col, hyph(t, "distribution")), "instead."
        ].


% Buggy-Rule: Using the wrong control group
buggy(cigroups, stage(1), X, Y, [step(buggy, control, [rc, mc])]) :-
    X = item(rc, s_rc, n_rc, mc, s_mc, n_mc, alpha),
    Y = item(instead(control, mc, rc), s_rc, n_rc, instead(control, rc, mc), s_mc, n_mc, alpha).

feedback(control, [RC, MC], Col, FB)
 => FB = [ "The sign of the result matches the ",
           "negative ", \mmlm(Col, hyph(t, "ratio,")), " ",
           "with ", \mmlm(Col, color(control, RC)), " subtracted ",
           "from ", \mmlm(Col, [color(control, MC)]), ". Please keep in mind ",
	   "that the control intervention must be subtracted from the tested ",
	   "intervention."
         ]. 

hint(control, [_RC, _MC], _Col, FB)
 => FB = [ "The control intervention must be subtracted from the tested ",
	   "intervention in the formula of the confidence interval for the difference",
	   " of group means."
         ].


% Buggy-Rule: Forgot to use square of standard deviation in pooled variance
buggy(cigroups, stage(1), From, To, [step(buggy, square, [S_A, S_B])]) :-
    From = var_pool(S_A^2, N_A, S_B^2, N_B),
    To = dfrac((N_A-1) * omit_right(square, S_A^2) + (N_B-1) * omit_right(square, S_B^2), N_A + N_B - 2).

feedback(square, [S_A, S_B], Col, FB)
 => FB = [ "The result matches the expression for the pooled variance without ",
	   "the square of ", \mmlm(Col, color(square, S_A)), " and ", 
	   \mmlm(Col, color(square, S_B)), ". Please do not forget the square of ",
	   \mmlm(Col, color(square, S_A)), " and ", \mmlm(Col, color(square, S_B)),
	   " when calculating the pooled variance."
         ].

hint(square, [_S_A, _S_B], _Col, FB)
 => FB = [ "Do not forget to use the square of the standard deviations ",
           "when calculating the pooled variance." 
         ].


% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)] when calculating CI
buggy(cigroups, stage(2), From, To, [step(buggy, school_1, [N_A, N_B])]) :-
    From = dot(quant(RC, MC, S2P, N_A, N_B, Alpha), sqrt(dot(S2P, frac(1, N_A) + frac(1, N_B)))),
    To = dot(quant(RC, MC, S2P, N_A, N_B, Alpha), sqrt(dot(S2P, color(school_1, frac(1, N_A + N_B))))).

feedback(school_1, [A, B], Col, FB)
 => FB = [ "The result matches the the confidence interval for independent samples with ",
	   \mmlm(Col, frac(1, color(school_1, color("black", A) + color("black", B)))),
	   ". Please keep in mind that ", \mmlm(Col, color(school_1, 
		color("black", frac(1, A)) + color("black", frac(1, B)))
		=\= frac(1, color(school_1, color("black", A) + color("black", B)))), "."
         ].

hint(school_1, [A, B], Col, FB)
 => FB = [ "Do not forget that ",
           \mmlm(Col, color(school_1, color("black", frac(1, A)) + color("black", frac(1, B))) =\= frac(1, color(school_1, color("black", A) + color("black", B)))), 
           "."
         ].


% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)] when calculating t-statistic
buggy(cigroups, stage(2), From, To, [step(buggy, school_2, [N_A, N_B])]) :-
    From = dfrac(RC - MC, sqrt(S2P * (frac(1, N_A) + frac(1, N_B)))),
    To = dfrac(RC - MC, sqrt(S2P * color(school_2, frac(1, N_A + N_B)))).

feedback(school_2, [A, B], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " for independent samples with ",
	   \mmlm(Col, frac(1, color(school_2, color("black", A) + color("black", B)))),
	   ". Please keep in mind that ", \mmlm(Col, color(school_2, 
		color("black", frac(1, A)) + color("black", frac(1, B)))
		=\= frac(1, color(school_2, color("black", A) + color("black", B)))), "."
         ].

hint(school_2, [A, B], Col, FB)
 => FB = [ "Do not forget that ",
           \mmlm(Col, color(school_2, color("black", frac(1, A)) + color("black", frac(1, B))) =\= frac(1, color(school_2, color("black", A) + color("black", B)))), 
           "."
         ].


% Buggy-Rule: Forget square root around the denominator	when calculating the CI
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt1, [S2P, N_RC, N_MC])]) :-			
    X = dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), sqrt(dot(S2P, frac(1, N_RC) + frac(1, N_MC)))),
    Y = dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), omit_right(sqrt1, (dot(S2P, frac(1, N_RC) + frac(1, N_MC)))^(1/2))).


feedback(sqrt1, [S2P, N_RC, N_MC], Col, FB)
 => FB = [ "The result matches the confidence interval without square root around ", 
           \mmlm(Col, color(sqrt1, dot(S2P, frac(1, N_RC) + frac(1, N_MC)))), ". Please do not forget the square root",
           " around the denominator."
         ].

hint(sqrt1, [S2P, N_RC, N_MC], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, color(sqrt1, dot(S2P, frac(1, N_RC) + frac(1, N_MC))))
	     ].


% Buggy-Rule: Forget square root around the denominator	when calculating the t-statistic
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt2, [S2P, N_RC, N_MC])]) :-			
    X = dfrac(RC - MC, sqrt(S2P * (frac(1, N_RC) + frac(1, N_MC)))),
    Y = dfrac(RC - MC, omit_right(sqrt2, (S2P * frac(1, N_RC) + frac(1, N_MC))^(1/2))).


feedback(sqrt2, [S2P, N_RC, N_MC], Col, FB)
 => FB = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " without ",
	   "square root around ", 
           \mmlm(Col, color(sqrt2, dot(S2P, frac(1, N_RC) + frac(1, N_MC)))), ". Please do not forget the square root",
           " around the denominator."
         ].

hint(sqrt2, [S2P, N_RC, N_MC], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, color(sqrt2, dot(S2P, frac(1, N_RC) + frac(1, N_MC))))
	     ].


% Buggy-Rule: Forget square root around sample size when calculating the CI
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt3, [Ns])]) :-
    Ns = frac(1, _N_RC) + frac(1, _N_MC),
    X = dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), sqrt(dot(S2P, Ns))),
    Y = dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), add_right(sqrt3, sqrt(omit_right(sqrt3, dot(S2P, Ns))) * Ns)).

feedback(sqrt3, [Ns], Col, FB)
 => FB = [ "The result matches the confidence interval with the square root ",
	   "stopping before ", \mmlm(Col, paren(color(sqrt3, Ns))), 
	   ". Please do not forget to take the square root of the whole denominator."
	 ].
% Alternative Rückmeldung (hierfür müsste die Variable in der eckigen Klammer geändert werden): 
% "The result matches the confidence interval with the square root only around ",
% \mmlm(Col, paren(color(sqrt3, S2P)), ". Please do not forget to take the "
% "square root of the whole denominator."

hint(sqrt3, [_Ns], _Col, FB)
 => FB = [ "The square root of the whole denomiator should be taken." 
	].

% Buggy-Rule: Forget square root around sample size when calculating the t-statistic
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt4, [Ns])]) :-
    Ns = frac(1, _N_RC) + frac(1, _N_MC),
    X = dfrac(RC - MC, sqrt(S2P * Ns)),
    Y = dfrac(RC - MC, add_right(sqrt4, sqrt(omit_right(sqrt4, dot(S2P, Ns))) * Ns)).

feedback(sqrt4, [Ns], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " with the square root stopping before ",
	   \mmlm(Col, paren(color(sqrt4, Ns))), ". Please do not forget to take the ",
	   "square root of the whole denominator."
	 ].
% Alternative Rückmeldung (hierfür müsste die Variable in der eckigen Klammer geändert werden): 
% "The result matches the expression for the ", \mmlm(Col, hyph(t, "ratio")),
% " with the square root only around ", \mmlm(Col, paren(color(sqrt4, S2P)),
% ". Please do not forget to take the square root of the whole denominator."

hint(sqrt4, [_Ns], _Col, FB)
 => FB = [ "The square root of the whole denomiator should be taken." 
	].
