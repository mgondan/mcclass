:- module(tgroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(tgroups, ["Independent ", i(t), "-test (1)"]).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(n_vr, sub(n, "VR")).
mathml_hook(n_box, sub(n, "BOX")).
mathml_hook(vr, overline("VR")).
mathml_hook(s_vr, sub(s, "VR")).
mathml_hook(box, overline("BOX")).
mathml_hook(s_box, sub(s, "BOX")).
mathml_hook(s2p, sub(s, "pool")^2).

% Obtain information from R
interval:r_hook(n_vr).
interval:r_hook(n_box).
interval:r_hook(vr).
interval:r_hook(s_vr).
interval:r_hook(box).
interval:r_hook(s_box).
interval:r_hook(s2p).
interval:r_hook(t).

render(item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
    { option(resp(R), Form, '#.##') },
	html(
	  [ div(class(card), div(class('card-body'),
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
		    "test (interval scaled, normally distributed, high scores = ",
		    "good performance). A few more dependent variables were ",
		    "assessed, including a knowledge test (interval scaled), ",
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
                        "E-learning for laparoscopic cholecystectomy and practiced ",
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
                        "(VR: ", \mmlm([digits(1)], r(vr)), " ± ", \mmlm([digits(1)], r(s_vr)), 
                        " vs. BOX: ", \mmlm([digits(1)], r(box)), " ± ", \mmlm([digits(1)], r(s_box)), 
                        ", p = 0.437). The significance level is set to ",
                        \mmlm(alpha = [5, "%"]), " two-tailed. ",
                        "Students generally liked training and felt well prepared for ", 
                        "assisting in laparoscopic surgery. The efficiency of the training ",
                        "was judged higher by the VR group than by the Box group."
                      ]))),
                 \download(tgroups)
	      ])),
            \htmlform(
              [ "Is VR training superior to traditional Box training? ",
                "Please report the ", \mmlm(hyph(t, "ratio,")), " using Box ",
                "as the control intervention." 
              ], "#tratio", R)
          ]).

intermediate(item).
start(item(vr, s_vr, n_vr, box, s_box, n_box)).

% t-test for independent groups.
intermediate(tratio).
expert(stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool(S_VR^2, N_VR, S_BOX^2, N_BOX)) ;
           '<-'(t, tratio(VR, BOX, s2p, N_VR, N_BOX))
         }.

feedback(problem, [], Col, FB)
 => FB = [ "Correctly identified the problem as ",
           "a ", \mmlm(Col, hyph(t, "test")), " for independent samples."
         ].

hint(problem, [], Col, FB)
 => FB = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for independent ",
           "samples." 
         ].

% Pooled variance.
intermediate(var_pool).
expert(stage(1), From, To, [step(expert, pooled, [S2P])]) :-
    From = '<-'(S2P, var_pool(S_A^2, N_A, S_B^2, N_B)),
    To = '<-'(S2P, dfrac((N_A-1) * S_A^2 + (N_B-1) * S_B^2, N_A + N_B - 2)).

feedback(pooled, [S2P], Col, FB)
 => FB = [ "Correctly determined the pooled variance ", \mmlm(Col, S2P) ].

hint(pooled, [S2P], Col, FB)
 => FB = [ "The pooled variance ", \mmlm(Col, S2P), " needs to be ",
           "calculated." 
         ].

% t-statistic
expert(stage(2), From, To, [step(expert, tratio, [To])]) :-
    From = tratio(VR, BOX, S2P, N_VR, N_BOX),
    To = dfrac(VR - BOX, sqrt(S2P * (1/N_VR + 1/N_BOX))).

feedback(tratio, [_T], Col, FB) =>
    FB = [ "Correctly determined the ",  \mmlm(Col, hyph(t, "statistic.")) ].

hint(tratio, [T], Col, FB) =>
    FB = [ "The ", \mmlm(Col, hyph(t, "statistic")), " must be determined, ", 
           \mmlm(Col, T) 
         ].

% Wrong control group
buggy(stage(1), From, To, [step(buggy, control, [vr, box])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(instead(control, box, vr), s_vr, n_vr, instead(control, vr, box), s_box, n_box).

feedback(control, [VR, Box], Col, FB)
 => FB = [ "The sign of the result matches the ",
           "negative ", \mmlm(Col, hyph(t, "ratio,")), " ",
           "with ", \mmlm(Col, color(control, VR)), " subtracted ",
           "from ", \mmlm(Col, [color(control, Box), "."])
         ].

hint(control, [VR, Box], Col, FB)
 => FB = [ \mmlm(Col, color(control, VR)), " ",
           "and ", \mmlm(Col, color(control, Box)), " should not be switched ",
           "in the numerator of the ", \mmlm(Col, hyph(t, "ratio."))
         ].

% Forgot to use square of standard deviation in pooled variance
buggy(stage(1), From, To, [step(buggy, square, [S_A, S_B])]) :-
    From = var_pool(S_A^2, N_A, S_B^2, N_B),
    To = dfrac((N_A-1) * omit_right(square, S_A^2) + (N_B-1) * omit_right(square, S_B^2), N_A + N_B - 2).

feedback(square, [S_A, S_B], Col, FB)
 => FB = [ "The expression for the pooled variance does not seem to include ",
           "the square of ", \mmlm(Col, color(square, S_A)), " ",
           "and ", \mmlm(Col, color(square, S_B)) ].

hint(square, [_S_A, _S_B], _Col, FB)
 => FB = [ "Do not forget to use the square of the standard deviations ",
	   "when calculating the pooled variance." ].

% 3) Forgot school math.
%buggy(stage(2), From, To, [step(buggy, school, [From, To])]) :-
%    From = 1 / N_VR + 1 / N_BOX,
%    To = color(school, frac(1, N_VR + N_BOX)).
%
%feedback(school, [From, To], Col, FB) =>
%    FB = [ "Keep in mind that ", 
%	   \mmlm(Col, color(school, From) =\= To) ].
%
%hint(school, [From, To], Col, FB) =>
%    FB = [ "Do not forget that ", 
%	   \mmlm(Col, color(school, From) =\= To) ].
%
%% 4) Forgot paranthesis around numerator in t-statistic.
%buggy(stage(2), From, To, [step(buggy, bug1, [VR, BOX, S2P, N_VR, N_BOX])]) :-
%    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
%    To = invent_left(bug1, VR - dfrac(BOX, sqrt(S2P * (1/N_VR + 1/N_BOX)))).
%%VR - dfrac(BOX, sqrt(s2p * (1/N_VR + 1/N_BOX))).
%
%feedback(bug1, [_VR, _BOX, S2P, N_VR, N_BOX], Col, FB) =>
%    FB = [ "You forgot the parentheses around the numerator of ",
%	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", overline("VR") - overline("BOX")))), 
%	   sqrt(S2P * (1/N_VR + 1/N_BOX) ) ) )
%	 ].
%
%hint(bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) =>
%    FB = [ "Remember to use parenthesis around numerator. ",
%	   "The correct formula for the ", \mmlm(Col, hyph(t, "ratio")), "is ", 
%	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
%	   sqrt(S2P * (1/N_VR + 1/N_BOX) ) ) )
%	 ].
%
%% 5) Forgot square root.
%buggy(stage(2), From, To, [step(buggy, nosr1, [A])]) :-
%    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
%    A = S2P * (1/N_VR + 1/N_BOX),
%    To = dfrac(VR - BOX, instead(nosr1, A, sqrt(A))).
%
%feedback(nosr1, [A], Col, FB) =>
%   FB = [ "You forgot to take the square root of ", \mmlm(Col, color(nosr1, A)) ].
%
%hint(nosr1, [A], Col, FB) =>
%    FB = [ "Keep in mind that you need to take the square root of ", 
%	   \mmlm(Col, color(nosr1, A)) ].
%
%
%% 6) Only took the square root of the first element of the denominator.
%buggy(stage(2), From, To, [step(buggy, nosr2, [S2P, N_VR, N_BOX])]) :-
%    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
%    A = sqrt(S2P * (1/N_VR + 1/N_BOX)),
%    To = dfrac(VR - BOX, instead(nosr2, sqrt(S2P) * (1/N_VR + 1/N_BOX), A)).
%
%feedback(nosr2, [_, _, _], Col, FB) =>
%   FB = [ "You took the square root of only ", \mmlm(Col, color(nosr2, s2p)),
%	  " instead of the square root of the entire denominator." ].
%
%hint(nosr2, [S2P, N_VR, N_BOX], Col, FB) =>
%   FB = [ "Keep in mind that you need to take the square root of ", 
%	   \mmlm(Col, color(nosr2, S2P * (1/N_VR + 1/N_BOX))), " in its entirety." ].

