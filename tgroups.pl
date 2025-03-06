:- module(tgroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(tgroups, ["Independent ", i(t), "-test"]).

task(s2p).
task(tratio).
task(cigroups).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering
mathml:math_hook(n_vr, subscript(n, "VR")).
mathml:math_hook(n_box, subscript(n, "BOX")).
mathml:math_hook(vr, overline("VR")).
mathml:math_hook(s_vr, subscript(s, "VR")).
mathml:math_hook(box, overline("BOX")).
mathml:math_hook(s_box, subscript(s, "BOX")).
mathml:math_hook(s2p, subscript(s, "pool")^2).
mathml:math_hook(alpha, greek("alpha")).

% Obtain information from R
r_hook(n_vr).
r_hook(n_box).
r_hook(vr).
r_hook(s_vr).
r_hook(box).
r_hook(s_box).
r_hook(s2p).
r_hook(t).
r_hook(lo).
r_hook(mu).

r_hook(var_pool/4).
mono((var_pool)/4, [+, /, +, /]).

% Task description
render 
--> {start(item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX, Alpha)) },
    html(
       div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Training of surgical skills"),
          p(class('card-text'),
            [ "Surgeons need special motor skills, especially for ",
              "endoscopic surgery through the belly. Nickel et al. (2015) ",
              "report the results of a study with two learning methods for ",
              "motor skill training. One group underwent a virtual reality ",
              "training (VR group), the other group participated in a ",
              "mixture of online courses and classical training of motor ",
              "skill with the so-called Box-trainer (Box group). ",
              "The primary dependent variable is the result on the OSATS ",
              "test (interval scale, normally distributed, high scores = ",
              "good performance). A few more dependent variables were ",
              "assessed, including a knowledge test (interval scale), ",
              "operation time (dichotomized, above or below 80 min), and ",
              "efficiency ratings (ordinal scale, 1=bad ... 5=good)."
            ]),
	  p(class('card-text'),
            [ "Please check the following text from the publication ",
              "(40 ± 10 means “average 40, standard deviation 10”):"
	    ]),
          div(class(card), 
            div(class('card-body'),
              p(class('card-text'),
                [ "“Laparoscopy-naïve medical students were randomized into ",
                  "two groups. The Box ",
                  "group ", \mmlm(["(", N_BOX = r(n_box), ")"]), " used ",
                  "e-learning for laparoscopic cholecystectomy and practiced ",
                  "basic skills with Box trainers. The VR ",
                  "group ", \mmlm(["(", N_VR = r(n_vr), ")"]), " trained ",
                  "basic skills and laparoscopic cholecystectomy on ",
                  "LAP Mentor II (Simbionix, Cleveland, USA). Each group ",
                  "trained 3 × 4 hours followed by a knowledge test. Blinded ",
                  "raters assessed the operative performance using the ",
                  "Objective Structured Assessment of Technical Skills ",
                  "(OSATS). The VR group completed the operation significantly ",
                  "faster and more often within 80 min than the Box ",
                  "group (VR: 28% vs. Box: 22%, p = 0.596). The Box group ",
                  "scored higher than the VR group in the knowledge ",
                  "test (Box: 13.4 ± 1.2 vs. VR: 10.8 ± 1.8, p < 0.001). Both ",
                  "groups showed equal operative performance in the OSATS score ",
                  "(VR: ", \mmlm([digits(1)], r(vr)), " ± ", \mmlm([digits(1)], r(s_vr1)), 
                  " vs. BOX: ", \mmlm([digits(1)], r(box)), " ± ", \mmlm([digits(1)], r(s_box)), 
                  ", p = 0.437). The significance level is set to ",
                  \mmlm(alpha = percent(r(Alpha))), " two-tailed. ",
                  "The medical students generally appreciated the training and felt well prepared for ", 
                  "assisting in laparoscopic surgery. The efficiency of the training ",
                  "was judged higher by the VR group than by the Box group."
                ]))),
            \download(tgroups)]))).

% Question for pooled variance.
task(s2p)
--> { start(item(_VR, _S_VR, _N_VR, _Box, _S_Box, _N_Box, _Alpha)),
      session_data(resp(tgroups2, s2p, Resp), resp(tgroups2, s2p, '#.##'))
    },
    html(\htmlform(
      [ "Compare the OSATS scores of both groups assuming homogeneity",
        " of variance and calculate the pooled variance ", \mmlm([s2p, "."])
      ], s2p, Resp)).

% Question for independent t-test.
task(tratio)
--> { start(item(_VR, _S_VR, _N_VR, _BOX, _S_BOX, _N_BOX, _Alpha)),
      session_data(resp(tgroups, tratio, Resp), resp(tgroups, tratio, '#.##'))
    },
    html(\htmlform(
      [ "Is VR training superior to traditional Box training? ",
        "Please report the ", \mmlm(hyph(t, "ratio")), " using the Box training",
        "as the control intervention." 
      ], tratio, Resp)).

% Question for the confidence interval.
task(cigroups)
--> { start(item(_VR, _S_VR, _N_VR, _Box, _S_Box, _N_Box, _Alpha)),
      session_data(resp(cigroups, cigroups, Resp), resp(cigroups, cigroups, '#.# to #.#'))
    },
    html(\htmlform(["Determine the confidence interval for the difference ",
        "between the two group means using the Box training as the control intervention."],
	cigroups, Resp)).

%
%% Expert-Rules for pooled variance Task
%
% t-test for independent groups
intermediate(s2p, item).
start(item(vr, s_vr, n_vr, box, s_box, n_box, alpha)).

% First step: Correctly calculate the pooled variance.
expert(s2p, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(_VR, S_VR, N_VR, _Box, S_Box, N_Box, _Alpha),
    To = { '<-'(s2p, dfrac((N_VR - 1) * S_VR ^ 2 + (N_Box - 1) * S_Box ^ 2, 
			   N_VR + N_Box - 2))
	 }.

feedback(indep, [], Col, FB) =>
    FB = [ "Correctly determined the pooled variance ", \mmlm(Col, [s2p, "."]) ].

hint(indep, [], Col, FB) =>
    FB = [ "The pooled variance ", \mmlm(Col, s2p), " needs to be ",
           "calculated." 
	 ].

%
%% Buggy-Rules for pooled variance Task
%
% Buggy-Rule: Used standard deviation instead of variance.
buggy(s2p, stage(2), From, To, [step(buggy, sd, [S_VR, S_Box])]) :-
    From = dfrac(A * S_VR ^ 2 + B * S_Box ^ 2, C),
    To = dfrac(A * instead(sd, S_VR, S_VR ^ 2) + B * instead(sd, S_Box, S_Box ^ 2), C).

feedback(sd, [S_VR, S_Box], Col, FB) =>
    FB = [ "The result matches the expression for the pooled variance with the",
	   " standard deviation instead of the standard variations.",
	   " Please remember to use the squares of the standard deviations ", \mmlm(Col, color(sd, S_VR)),
	   " and ", \mmlm(Col, color(sd, S_Box)), " to calculate the pooled variance."
	 ].

hint(sd, [_S_VR, _S_Box], _Col, FB) =>
    FB = [ "Do not forget to use the square of the standard deviations ",
           "when calculating the pooled variance."
	 ].

% Buggy-Rule: Swapped N_VR and N_Box.
buggy(s2p, stage(2), From, To, [step(buggy, nswap, [N_VR, N_Box])]) :-
    From = dfrac((N_VR - 1) * S_VR ^ 2 + (N_Box - 1) * S_Box ^ 2, N_VR + N_Box - 2),
    To = dfrac((instead(nswap, N_Box, N_VR) - 1) * S_VR ^ 2 + (instead(nswap, N_VR, N_Box) - 1) * S_Box ^ 2, N_VR + N_Box - 2).


feedback(nswap, [N_VR, N_Box], Col, FB) =>
    FB = [ "The result matches the expression for the pooled variance with ",
	   "swaped sample sizes. Please double check the sample sizes ", 
	   \mmlm(Col, color(nswap, N_VR)), " and ", \mmlm(Col, color(nswap, N_Box)),
	   " of both groups."
	 ].

hint(nswap, [_N_VR, _N_Box], Col, FB) =>
    FB = [ "Do not swap the sample sizes in the formula for the pooled variance ", \mmlm(Col, [color(nswap,s2p), "."]) ].

%
%% Expert Rules for independent t-test task
%
% t-test for independent samples
intermediate(tratio, item).


% First step: Extract the correct information for a t-test for independent 
% samples from the task description.
intermediate(tratio, tratio).
expert(tratio, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX, _Alpha),
    To = { '<-'(s2p, var_pool(S_VR^2, N_VR, S_BOX^2, N_BOX)) ;
           '<-'(t, tratio(VR, BOX, s2p, N_VR, N_BOX))
         }.

feedback(problem, [], Col, FB)
 => FB = [ "Correctly identified the problem as a ",
           \mmlm(Col, hyph(t, "test")), " for independent samples."
         ].

hint(problem, [], Col, FB)
 => FB = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for independent ",
           "samples." 
         ].

% Second step: To calculate the t-ratio for independent samples, the pooled 
% variance has to be calculated first. Apply the formula for the pooled variance.
intermediate(tratio, var_pool).
expert(tratio, stage(1), From, To, [step(expert, pooled, [S2P])]) :-
    From = '<-'(S2P, var_pool(S_A^2, N_A, S_B^2, N_B)),
    To = '<-'(S2P, dfrac((N_A-1) * S_A^2 + (N_B-1) * S_B^2, N_A + N_B - 2)).

feedback(pooled, [S2P], Col, FB)
 => FB = [ "Correctly determined the pooled variance ", \mmlm(Col, [S2P, "."]) ].

hint(pooled, [S2P], Col, FB)
 => FB = [ "The pooled variance ", \mmlm(Col, S2P), " needs to be ",
           "calculated." 
         ].

% Third step: Apply the formula for the t-ratio for independent samples.
expert(tratio, stage(2), From, Fmt, [step(expert, tratio, [To])]) :-
    From = tratio(VR, BOX, S2P, N_VR, N_BOX),
    To = dfrac(VR - BOX, sqrt(S2P * (frac(1, N_VR) + frac(1, N_BOX)))),
    Fmt = tstat(To).

feedback(tratio, [_T], Col, FB) =>
    FB = [ "Correctly determined the ",  \mmlm(Col, hyph(t, "statistic.")) ].

hint(tratio, [T], Col, FB) =>
    FB = [ "The ", \mmlm(Col, hyph(t, "statistic")), " must be determined: ", 
           \mmlm(Col, [T, "."]) 
         ].

%
%% Buggy-Rules for independent t-test task
%
% Buggy-Rule: Using the wrong control group
buggy(tratio, stage(1), From, To, [step(buggy, control, [vr, box])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box, alpha),
    To = item(instead(control, box, vr), s_vr, n_vr, instead(control, vr, box), s_box, n_box, alpha).

feedback(control, [VR, Box], Col, FB)
 => FB = [ "The sign of the result matches the ",
           "negative ", \mmlm(Col, hyph(t, "ratio,")), " ",
           "with ", \mmlm(Col, color(control, VR)), " subtracted ",
           "from ", \mmlm(Col, [color(control, Box), "."]), " Please keep in mind ",
	   "that the control intervention must be subtracted from the tested ",
	   "intervention."
         ].

hint(control, [VR, Box], Col, FB)
 => FB = [ "The control intervention ", \mmlm(Col, [color(control, Box)]),
	   " must be subtracted from the tested intervention ",
	   \mmlm(Col, color(control, VR)), " in the numerator of the ",
	   \mmlm(Col, hyph(t, "ratio."))
         ].

% Buggy-Rule: Forgot to use square of standard deviation in pooled variance
buggy(tratio, stage(1), From, To, [step(buggy, square, [S_A, S_B])]) :-
    From = var_pool(S_A^2, N_A, S_B^2, N_B),
    To = dfrac((N_A-1) * omit_right(square, S_A^2) + (N_B-1) * omit_right(square, S_B^2), N_A + N_B - 2).

feedback(square, [S_A, S_B], Col, FB)
 => FB = [ "The result matches the expression for the pooled variance without ",
	   "the square of ", \mmlm(Col, color(square, S_A)), " and ", 
	   \mmlm(Col, [color(square, S_B), "."]), " Please do not forget the square of ",
	   \mmlm(Col, color(square, S_A)), " and ", \mmlm(Col, color(square, S_B)),
	   "when calculating the pooled variance."
         ].

hint(square, [_S_A, _S_B], Col, FB)
 => FB = [ "Do not forget to use the square of the standard deviations ",
           "when calculating the pooled variance ", \mmlm(Col, color(square, [s2p, "."])) 
         ].

% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)]
buggy(tratio, stage(2), From, To, [step(buggy, school, [N_A, N_B])]) :-
    From = frac(1, N_A) + frac(1, N_B),
    To = color(school, frac(1, N_A + N_B)).

feedback(school, [A, B], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " for independent samples with ",
	   \mmlm(Col, [frac(1, color(school, color("black", A) + color("black", B))), "."]),
	   " Please keep in mind that ", \mmlm(Col, [color(school, 
		color("black", frac(1, A)) + color("black", frac(1, B)))
		=\= frac(1, color(school, color("black", A) + color("black", B))), "."])
         ].

hint(school, [A, B], Col, FB)
 => FB = [ "Do not forget that ",
           \mmlm(Col, [color(school, color("black", frac(1, A)) + color("black", frac(1, B))) =\= frac(1, color(school, color("black", A) + color("black", B))), "."])
         ].

%% Buggy-Rule: Forgot paranthesis around numerator in t-statistic.
%buggy(tratio, stage(2), From, To, [step(buggy, bug1, [VR, BOX, S2P, N_VR, N_BOX])]) :-
%    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
%    To = add_left(bug1, VR - dfrac(BOX, sqrt(S2P * (1/N_VR + 1/N_BOX)))).
%%VR - dfrac(BOX, sqrt(s2p * (1/N_VR + 1/N_BOX))).
%
%feedback(bug1, [_VR, _BOX, S2P, N_VR, N_BOX], Col, FB) =>
%    FB = [ "The result matches the expression for the ", 
%	    \mmlm(Col, hyph(t, "ratio")), " for independent samples without the ",
%	    "parentheses around the numerator of ", 
%	    \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", overline("VR") - overline("BOX")))), sqrt(S2P * (1/N_VR + 1/N_BOX)))), 
%	    ". Please do not forget the parenthesis",
%	    " around the numerator."
%	 ].
%
%hint(bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) =>
%    FB = [ "Remember to use parenthesis around the numerator. ",
%	    "The correct formula for the ", \mmlm(Col, hyph(t, "ratio")), "is ", 
%	    \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), sqrt(S2P * (1/N_VR + 1/N_BOX)))), "."
%	 ].

% Buggy-Rule: Forget square root around the denominator	
buggy(tratio, stage(2), From, To, [step(buggy, sqrt1, [S2P * Ns])]) :-
    From = sqrt(S2P * Ns),
    To = instead(sqrt1, S2P * Ns, sqrt(S2P * Ns)).

feedback(sqrt1, [S2P_Ns], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " for independent samples without the square",
	   " root around ", \mmlm(Col, [color(sqrt1, S2P_Ns), "."]), " Please do not forget",
	   " the square root around the denominator."
         ].

hint(sqrt1, [_], Col, FB)
 => FB = [ "Do not forget the square root around the denominator of ",
           "the ", \mmlm(Col, hyph(t, "ratio."))
         ].

% Buggy-Rule: Forget square root around sample size
buggy(tratio, stage(2), From, To, [step(buggy, sqrt2, [Ns])]) :-
    Ns = frac(1, _N_VR) + frac(1, _N_Box),
    From = sqrt(S2P * Ns),
    To = add_right(sqrt2, sqrt(omit_right(sqrt2, S2P * Ns)) * Ns).

feedback(sqrt2, [Ns], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " with the square root stopping before ",
	   \mmlm(Col, [paren(color(sqrt2, Ns)), "."]), " Please do not forget to take the ",
	   "square root of the whole denominator."
         ].
% Alternative Rückmeldung (hierfür müsste die Variable in der eckigen Klammer geändert werden): 
% "The result matches the expression for the ", \mmlm(Col, hyph(t, "ratio")),
% " with the square root only around ", \mmlm(Col, [paren(color(sqrt2, S2P)),
% ". Please do not forget to take the square root of the whole denominator."

hint(sqrt2, [_Ns], Col, FB)
 => FB = [ "You need to take the square root of the whole denomiator in the formula for the ",
            \mmlm(Col, hyph(t, "ratio."))
         ].




%
%% Expert Rules for the confidence intervall of an independent t-test task
%
% t-test and confidence intervall for independent samples 
intermediate(cigroups, item).

% First step: Extract the correct information for a t-test for independent 
% samples and the associated confidence interval from the task description
intermediate(cigroups, indep).
expert(cigroups, stage(1), X, Y, [step(expert, problem, [])]) :-
    X = item(VR, S_VR, N_VR, Box, S_Box, N_Box, Alpha),
    Y = { '<-'(s2p, var_pool(S_VR^2, N_VR, S_Box^2, N_Box)) ;
          '<-'(ci, indep(VR, Box, s2p, N_VR, N_Box, Alpha))
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
 => FB = [ "Correctly determined the pooled variance ", \mmlm(Col, S2P) ].

hint(pooled, [S2P], Col, FB)
 => FB = [ "The pooled variance ", \mmlm(Col, S2P), " needs to be ",
           "calculated." 
         ].


% Third step: Apply the formula for the confidence interval for the difference
% of group means
intermediate(cigroups, quant).
expert(cigroups, stage(2), X, Y, [step(expert, ci_bounds, [VR, Box, S2P, N_VR, N_Box, Alpha])]) :-
    X = indep(VR, Box, S2P, N_VR, N_Box, Alpha),
    Y = hdrs(pm(VR - Box, dot(quant(VR, Box, S2P, N_VR, N_Box, Alpha), sqrt(dot(S2P, frac(1, N_VR) + frac(1, N_Box)))))).

feedback(ci_bounds, [_VR, _Box, _S2P, _N_VR, _N_Box, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for the difference of group means." 
        ].

hint(ci_bounds, [VR, Box, S2P, N_VR, N_Box, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval for the difference of group means is ",
	  \mmlm(Col, [pm(VR - Box, qt(1 - Alpha/2, N_VR + N_Box - 2) * sqrt(dot(S2P, frac(1, N_VR) + frac(1, N_Box)))), "."])
        ].

% Fourth step: Choose the correct quantile of the t-distribution
expert(cigroups, stage(2), X, Y, [step(expert, tquant, [Alpha])]) :-
    X = quant(_VR, _Box, _S2P, N_VR, N_Box, Alpha),
    Y = qt(1 - Alpha/2, N_VR + N_Box - 2).

feedback(tquant, [Alpha], Col, F)
 => F = [ "Correctly used the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution."))
        ].

hint(tquant, [Alpha], Col, H)
 => H = [ "Make sure to use the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution."))
        ].

%
%% Buggy-Rules for the confidence intervall of an independent t-test task
%				
% Buggy-Rule: Use t-statistic instead of t-quantile
buggy(cigroups, stage(2), X, Y, [step(buggy, tstat, [N_VR, N_Box, Alpha])]) :-
    X = quant(VR, Box, S2P, N_VR, N_Box, Alpha),
    P = denote(t, dfrac(VR - Box, sqrt(S2P * (frac(1, N_VR) + frac(1, N_Box)))), ["the observed", space, t, "-statistic."]),
    Y = P. 

feedback(tstat, [N_VR, N_Box, Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the observed ",
          \mmlm(Col, hyph(t, "statistic.")), " Please use the quantile ",
           "of the ", \mmlm(Col, hyph(t, "distribution ")), 
	   \mmlm(Col, color(qt, qt(1 - Alpha/2, N_VR + N_Box - 2))), " instead."
        ].

hint(tstat, [_N_VR, _N_Box, _Alpha], Col, H)
 => H = [ "Do not insert the observed ", \mmlm(Col, hyph(t, "statistic ")),
          "into the formula for the confidence interval. Use the quantile of ", 
	  "the ", \mmlm(Col, hyph(t, "distribution")), " instead."
        ].


% Buggy-Rule: Using the wrong control group
buggy(cigroups, stage(1), X, Y, [step(buggy, control, [vr, box])]) :-
    X = item(vr, s_vr, n_vr, box, s_box, n_box, alpha),
    Y = item(instead(control, box, vr), s_vr, n_vr, instead(control, vr, boc), s_box, n_box, alpha).

feedback(control, [VR, Box], Col, FB)
 => FB = [ "The sign of the result matches the ",
           "negative ", \mmlm(Col, hyph(t, "ratio,")), " ",
           "with ", \mmlm(Col, color(control, VR)), " subtracted ",
           "from ", \mmlm(Col, [color(control, Box), "."]), " Please keep in mind ",
	   "that the control intervention must be subtracted from the tested ",
	   "intervention."
         ]. 


hint(control, [VR, Box], Col, FB)
 => FB = [ "The control intervention ", \mmlm(Col, [color(control, Box)]),
	   " must be subtracted from the tested intervention ",
	   \mmlm(Col, color(control, VR)), " formula of the confidence interval",
	   "  for the difference of group means."
         ].

% Buggy-Rule: Forgot to use square of standard deviation in pooled variance
buggy(cigroups, stage(1), From, To, [step(buggy, square, [S_A, S_B])]) :-
    From = var_pool(S_A^2, N_A, S_B^2, N_B),
    To = dfrac((N_A-1) * omit_right(square, S_A^2) + (N_B-1) * omit_right(square, S_B^2), N_A + N_B - 2).

feedback(square, [S_A, S_B], Col, FB)
 => FB = [ "The result matches the expression for the pooled variance without ",
	   "the square of ", \mmlm(Col, color(square, S_A)), " and ", 
	   \mmlm(Col, [color(square, S_B), "."]), " Please do not forget the square of ",
	   \mmlm(Col, color(square, S_A)), " and ", \mmlm(Col, color(square, S_B)),
	   "when calculating the pooled variance."
         ].

hint(square, [_S_A, _S_B], Col, FB)
 => FB = [ "Do not forget to use the square of the standard deviations ",
           "when calculating the pooled variance ", \mmlm(Col, [color(square, s2p), "."]) 
         ].


% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)] when calculating CI
buggy(cigroups, stage(2), From, To, [step(buggy, school_1, [N_A, N_B])]) :-
    From = dot(quant(VR, Box, S2P, N_A, N_B, Alpha), sqrt(dot(S2P, frac(1, N_A) + frac(1, N_B)))),
    To = dot(quant(VR, Box, S2P, N_A, N_B, Alpha), sqrt(dot(S2P, color(school_1, frac(1, N_A + N_B))))).

feedback(school_1, [A, B], Col, FB)
 => FB = [ "The result matches the the confidence interval for independent samples with ",
	   \mmlm(Col, [frac(1, color(school_1, color("black", A) + color("black", B))), "."]),
	   " Please keep in mind that ", \mmlm(Col, [color(school_1, 
		color("black", frac(1, A)) + color("black", frac(1, B)))
		=\= frac(1, color(school_1, color("black", A) + color("black", B))), "."])
         ].

hint(school_1, [A, B], Col, FB)
 => FB = [ "Do not forget that ",
           \mmlm(Col, [color(school_1, color("black", frac(1, A)) + color("black", frac(1, B))) =\= frac(1, color(school_1, color("black", A) + color("black", B))), "."]) 
         ].


% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)] when calculating t-statistic
buggy(cigroups, stage(2), From, To, [step(buggy, school_2, [N_A, N_B])]) :-
    From = dfrac(VR - Box, sqrt(S2P * (frac(1, N_A) + frac(1, N_B)))),
    To = dfrac(VR - Box, sqrt(S2P * color(school_2, frac(1, N_A + N_B)))).

feedback(school_2, [A, B], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " for independent samples with ",
	   \mmlm(Col, [frac(1, color(school_2, color("black", A) + color("black", B))), "."]),
	   " Please keep in mind that ", \mmlm(Col, [color(school_2, 
		color("black", frac(1, A)) + color("black", frac(1, B)))
		=\= frac(1, color(school_2, color("black", A) + color("black", B))), "."])
         ].

hint(school_2, [A, B], Col, FB)
 => FB = [ "Do not forget that ",
           \mmlm(Col, [color(school_2, color("black", frac(1, A)) + color("black", frac(1, B))) =\= frac(1, color(school_2, color("black", A) + color("black", B))), "."])
         ].


% Buggy-Rule: Forget square root around the denominator	when calculating the CI
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt1, [S2P, N_VR, N_Box])]) :-			
    X = dot(quant(VR, Box, S2P, N_VR, N_Box, Alpha), sqrt(dot(S2P, frac(1, N_VR) + frac(1, N_Box)))),
    Y = dot(quant(VR, Box, S2P, N_VR, N_Box, Alpha), omit_right(sqrt1, (dot(S2P, frac(1, N_VR) + frac(1, N_Box)))^(1/2))).


feedback(sqrt1, [S2P, N_VR, N_Box], Col, FB)
 => FB = [ "The result matches the confidence interval without square root around ", 
           \mmlm(Col, [color(sqrt1, dot(S2P, frac(1, N_VR) + frac(1, N_Box))), "."]), " Please do not forget the square root",
           " around the denominator."
         ].

hint(sqrt1, [S2P, N_VR, N_Box], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, [color(sqrt1, dot(S2P, frac(1, N_VR) + frac(1, N_Box))), "."])
	     ].


% Buggy-Rule: Forget square root around the denominator	when calculating the t-statistic
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt2, [S2P, N_VR, N_Box])]) :-			
    X = dfrac(VR - Box, sqrt(S2P * (frac(1, N_VR) + frac(1, N_Box)))),
    Y = dfrac(VR - Box, omit_right(sqrt2, (S2P * frac(1, N_VR) + frac(1, N_Box))^(1/2))).


feedback(sqrt2, [S2P, N_VR, N_Box], Col, FB)
 => FB = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " without ",
	   "square root around ", 
           \mmlm(Col, [color(sqrt2, dot(S2P, frac(1, N_VR) + frac(1, N_Box))), "."]), " Please do not forget the square root",
           " around the denominator."
         ].

hint(sqrt2, [S2P, N_VR, N_Box], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, [color(sqrt2, dot(S2P, frac(1, N_VR) + frac(1, N_Box))), "."])
	     ].


% Buggy-Rule: Forget square root around sample size when calculating the CI
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt3, [Ns])]) :-
    Ns = frac(1, _N_VR) + frac(1, _N_Box),
    X = dot(quant(VR, Box, S2P, N_VR, N_Box, Alpha), sqrt(dot(S2P, Ns))),
    Y = dot(quant(VR, Box, S2P, N_VR, N_Box, Alpha), add_right(sqrt3, sqrt(omit_right(sqrt3, dot(S2P, Ns))) * Ns)).

feedback(sqrt3, [Ns], Col, FB)
 => FB = [ "The result matches the confidence interval with the square root ",
	   "stopping before ", \mmlm(Col, [paren(color(sqrt3, Ns)), "."]), 
	   " Please do not forget to take the square root of the whole denominator."
	 ].
% Alternative Rückmeldung (hierfür müsste die Variable in der eckigen Klammer geändert werden): 
% "The result matches the confidence interval with the square root only around ",
% \mmlm(Col, paren(color(sqrt3, S2P)), ". Please do not forget to take the "
% "square root of the whole denominator."

hint(sqrt3, [_Ns], _Col, FB)
 => FB = [ "The square root of the whole denomiator should be taken." ].

% Buggy-Rule: Forget square root around sample size when calculating the t-statistic
buggy(cigroups, stage(2), X, Y, [step(buggy, sqrt4, [Ns])]) :-
    Ns = frac(1, _N_VR) + frac(1, _N_Box),
    X = dfrac(VR - Box, sqrt(S2P * Ns)),
    Y = dfrac(VR - Box, add_right(sqrt4, sqrt(omit_right(sqrt4, dot(S2P, Ns))) * Ns)).

feedback(sqrt4, [Ns], Col, FB)
 => FB = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " with the square root stopping before ",
	   \mmlm(Col, [paren(color(sqrt4, Ns)), "."]), " Please do not forget to take the ",
	   "square root of the whole denominator."
	 ].
% Alternative Rückmeldung (hierfür müsste die Variable in der eckigen Klammer geändert werden): 
% "The result matches the expression for the ", \mmlm(Col, hyph(t, "ratio")),
% " with the square root only around ", \mmlm(Col, paren(color(sqrt4, S2P)),
% ". Please do not forget to take the square root of the whole denominator."

hint(sqrt4, [_Ns], _Col, FB)
 => FB = [ "The square root of the whole denomiator should be taken."].
