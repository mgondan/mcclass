:- module(cigroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(cigroups, ["Confidence interval for independent samples"]).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(n_mc, sub(n, "N_MC")).
mathml_hook(n_rc, sub(n, "N_RC")).
mathml_hook(mc, overline("MC")).
mathml_hook(s_mc, sub(s, "S_MC")).
mathml_hook(rc, overline("RC")).
mathml_hook(s_rc, sub(s, "S_RC")).
mathml_hook(s2p, sub(s, "pool")^2).
mathml_hook(alpha, greek("alpha")).

% R definitions
r:r_hook(n_mc).
r:r_hook(n_rc).
r:r_hook(mc).
r:r_hook(s_mc).
r:r_hook(rc).
r:r_hook(s_rc).
r:r_hook(s2p).
r:r_hook(t).
r:r_hook(lo).
r:r_hook(mu).
r:r_hook(qt(_P, _DF)).

% Task description
render(item(RC, S_RC, N_RC, MC, S_MC, N_MC, Alpha), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "New math teaching concept"),
            p(class('card-text'),
            [ "There are plans to introduce a new concept for teaching ",
              "mathematics nationwide. Prior to this, a school is testing ",
              "whether the new concept improves the students' understanding ",
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
                    [ [ \mmlm([digits(1)], r(MC)),
                        \mmlm([digits(1)], r(RC)) ],
                      [ \mmlm([digits(1)], r(S_MC)),
                        \mmlm([digits(1)], r(S_RC))]
                    ])))),
            \download(cigroups)
          ])),
        \htmlform(["Determine the confidence interval for the difference between the two group means. The alpha level is ", \mmlm([], alpha = perc(Alpha))
                ], '#cigroups', R)
    ]).


% t-test and confidence intervall for independent samples 
intermediate(item).
start(item(rc, s_rc, n_rc, mc, s_mc, n_mc, alpha)). 

% First step: Extract the correct information for a t-test for independent 
% samples and the associated confidence interval from the task description
intermediate(tratio).
expert(stage(1), X, Y, [step(expert, problem, [])]) :-
    X = item(RC, S_RC, N_RC, MC, S_MC, N_MC, Alpha),
    Y = { '<-'(s2p, var_pool(S_RC^2, N_RC, S_MC^2, N_MC)) ;
          '<-'(t, tratio(RC, MC, s2p, N_RC, N_MC, Alpha))
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
intermediate(var_pool).
expert(stage(1), X, Y, [step(expert, pooled, [S2P])]) :-
    X = '<-'(S2P, var_pool(S_A^2, N_A, S_B^2, N_B)),
    Y = '<-'(S2P, dfrac((N_A-1) * S_A^2 + (N_B-1) * S_B^2, N_A + N_B - 2)).

feedback(pooled, [S2P], Col, FB)
 => FB = [ "Correctly determined the pooled variance ", \mmlm(Col, S2P) ].

hint(pooled, [S2P], Col, FB)
 => FB = [ "The pooled variance ", \mmlm(Col, S2P), " needs to be ",
           "calculated." 
         ].


% Third step: Apply the formula for the confidence interval for the difference
% of group means
intermediate(quant).
expert(stage(2), X, Y, [step(expert, ci_bounds, [RC, MC, S2P, N_RC, N_MC, Alpha])]) :-
    X = tratio(RC, MC, S2P, N_RC, N_MC, Alpha),
    Y = hdrs(pm(RC - MC, dot(quant(RC, MC, S2P, N_RC, N_MC, Alpha), sqrt(dot(S2P, frac(1, N_RC) + frac(1, N_MC)))))).

feedback(ci_bounds, [_RC, _MC, _S2P, _N_RC, _N_MC, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for the difference of group means." 
        ].

hint(ci_bounds, [RC, MC, S2P, N_RC, N_MC, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval for the difference of group means is ",
	  \mmlm(Col, pm(RC - MC, qt(1 - Alpha/2, N_RC + N_MC - 2) * sqrt(dot(S2P, frac(1, N_RC) + frac(1, N_MC)))))
        ].

% Fourth step: Choose the correct quantile of the t-distribution
expert(stage(2), X, Y, [step(expert, tquant, [Alpha])]) :-
    X = quant(_RC, _MC, _S2P, N_RC, N_MC, Alpha),
    Y = qt(1 - Alpha/2, N_RC + N_MC - 2).

feedback(tquant, [Alpha], Col, F)
 => F = [ "Correctly used the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution"))
        ].

hint(tquant, [Alpha], Col, H)
 => H = [ "Make sure to use the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution"))
        ].
