:- module(chisq, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(chisq, "chi-square").
task(chisq).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(p_VR, subscript(p, "VR")).
math_hook(p_Box, subscript(p, "Box")).
math_hook(s_VR, subscript(s, "VR")).
math_hook(s_Box, subscript(s, "Box")).
math_hook(n_VR, subscript(n, "VR")).
math_hook(n_Box, subscript(n, "Box")).
math_hook(chi2, chi^2).
math_hook(p_pool, subscript(p, "pool")).

% R constants
r_hook(p_VR).
r_hook(p_VRx).
r_hook(s_VR).
r_hook(n_VR).
r_hook(p_Box).
r_hook(p_Boxx).
r_hook(s_Box).
r_hook(n_Box).
r_hook(z).
r_hook(chi2).
r_hook(p_pool).

% Task description
render(Flags)
--> { start(item(N_VR, S_VR, _P_VR, P_VRx, N_Box, S_Box, _P_Box, P_Boxx)) },
      html(
        div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Training of surgical skills"),
            p(class("card-text"),
              [ "Surgeons need special motor skills, especially for ",
                "endoscopic surgery through the belly. Nickel et al. (2015) ",
                "report the results of a study with two learning methods for ",
                "motor skill training. The VR group underwent a virtual ",
                "reality training (VR group), the Box group participated in ",
                "a mixture of online courses and classical training of motor ",
                "skill with the so-called Box-trainer. "
              ]),
            p(class("card-text"),
              [ "The primary dependent variable was the result on the OSATS ",
                "test (interval scaled, normally distributed, high scores = ",
                "good performance). A few more dependent variables were ",
                "assessed, including a knowledge test (interval scaled), ",
                "operation time (dichotomized, above or below 80 min), and ",
                "efficiency ratings (ordinal scale, 1=bad ... 5=good)."
              ]),
            p(class("card-text"),
              [ "Here we look at the speed of the operation. The publication ",
                "states the following:"
              ]),
            div(class("border-start border-4 rounded"), 
              div(class("card-body border"), p(class("card-text"),
                [ "“Laparoscopy-naïve medical students were randomized into ",
                  "two groups. (...) The VR group completed the operation ",
                  "more often within 80 min than the Box ",
                  "group ", \nowrap(["(", \mmlm(Flags, percent(r(P_VRx)))]), " ",
                  "vs. ", \nowrap([\mmlm(Flags, percent(r(P_Boxx))), ")."]), " ",
                  "The percentages correspond to ", \mmlm(Flags, r(S_VR)), " ",
                  "successes (out ",
                  "of ", \nowrap([\mmlm(Flags, r(N_VR)), ")"]), " in the VR ",
                  "group and ", \mmlm(Flags, r(S_Box)), " successes (out ",
                  "of ", \nowrap([\mmlm(Flags, r(N_Box)), ")"]), " in the Box ",
                  "group.”"
                ])))
	  ]))).

task(Flags, chisq)
--> { start(item(_N_VR, _S_VR, _P_VR, _P_VRx, _N_Box, _S_Box, _P_Box, _P_Boxx)),
      session_data(resp(chisq, chisq, Resp), resp(chisq, chisq, '#.##'))
    },
    html(\htmlform([ "Does VR training lead to faster surgery times than ",
        "traditional Box training? Please ",
        "determine the ", \nowrap([\mmlm(Flags, chi^2), "-statistic."]) ], chisq, Resp)).

% Chi-square Test
intermediate(chisq, item).
start(item(n_VR, s_VR, p_VR, p_VRx, n_Box, s_Box, p_Box, p_Boxx)).

% Extract the correct information for the pooled proportion of 
% successes and the z-statistic. Calculation of chi-square, by squaring z.
intermediate(chisq, ppool).
intermediate(chisq, zstat).
expert(chisq, stage(1), From, To, [step(expert, steps, [])]) :-
    From = item(N_VR, S_VR, P_VR, _, N_Box, S_Box, P_Box, _),
    To = { '<-'(p_pool, ppool(S_VR, S_Box, N_VR, N_Box)) ;
           '<-'(z, zstat(P_VR, P_Box, p_pool, N_VR, N_Box)) ;
	   '<-'(chi2, chi2ratio(z^2))
         }.

feedback(steps, [], _Col, F)
 => F = "Correctly identified the main steps of the calculation.".

hint(steps, Col, H)
 => H = [ "First determine the pooled success proportion, then ",
           "the ", \nowrap([\mmlm(Col, z), "-statistic."]), " Finally, ",
           "raise ", \mmlm(Col, z), " to the square to ",
           "obtain ", \nowrap([\mmlm(Col, chi^2), "."])
         ].

% Calculate the pooled proportion of successes
expert(chisq, stage(1), From, To, [step(expert, ppool, [S_VR, S_Box, N_VR, N_Box])]) :-
    From = ppool(S_VR, S_Box, N_VR, N_Box),
    To = dfrac(S_VR + S_Box, N_VR + N_Box).

feedback(ppool, [_S_VR, _S_Box, _N_VR, _N_Box], _Col, F)
 => F = "Correctly determined the pooled proportion of successes.".

hint(ppool, Col, H)
 => H = [ "The pooled proportion of successes is ",
          \nowrap([\mmlm(Col, p_pool = dfrac(s_vr + s_box, n_vr + n_box)), "."])
        ].

% Determine the z-statistic
expert(chisq, stage(2), From, To, [step(expert, zstat, [P_VR, P_Box, P_Pool, N_VR, N_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (frac(1, N_VR) + frac(1, N_Box)))).

feedback(zstat, [_P_VR, _P_Box, _P_Pool, _N_VR, _N_Box], Col, F)
 => F = [ "Correctly determined ",
          "the ", \nowrap([\mmlm(Col, z), "-statistic."]) 
        ].

hint(zstat, Col, H)
 => H = [ "The ", \nowrap([\mmlm(Col, z), "-statistic"]), " is ",
          \mmlm(Col, dfrac(p_vr - p_box, sqrt(p_pool * (1 - p_pool) * (frac(1, n_vr) + frac(1, n_box))))) 
        ].

% - instead of + for both parts of p_pool. Appeared 1-2x in 2018 exams.
% @Jeremy, please check if this works
%
% buggy(chisq, stage(1), From, To, [step(buggy, pdiff, [])]) :-
%     From = ppool(S_VR, S_Box, N_VR, N_Box),
%     To = dfrac(instead(pdiff, S_VR - S_Box, S_VR + S_Box),
%                instead(pdiff, N_VR - N_Box, N_VR + N_Box)).
%
% feedback(pdiff, [], Col, F)
%  => F = [ "The result matches the pooled proportion of successes with a ",
%           "difference instead of a sum in both the numerator and the denominator of ",
%           \mmlm(Col, color(pdiff, p_pool)), ". Please add the numbers in both the ",
%           "numerator and the denominator of ", \mmlm(Col, color(pdiff, p_pool))
%         ].
%
% hint(pdiff, Col, H)
%  => H = [ "Remember to add the numbers in both the numerator and the ",
%           "denominator when calculating the pooled proportion of successes ", 
%           \mmlm(Col, color(pdiff, p_pool))
%         ].

% Buggy-Rule: Forget to square z. 
buggy(chisq, stage(2), From, To, [step(buggy, square, [])]) :-
    From = chi2ratio(Z^2),
    To = chi2ratio(omit_right(square, Z^2)).

feedback(square, [], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, color(square, z)), "-statistic"]), " ",
          "instead of ", \nowrap([\mmlm(Col, [color(square, chi2)]), "."]), " ",
          "Please do not forget to raise ", \mmlm(Col, z), " to the square."
        ].

hint(square, Col, H) 
 => H = [ "Do not forget to raise ",
           "the ", \nowrap([\mmlm(Col, color(square, z)), "-value"]), " ",
           "to the square to obtain ",
           "the ", \nowrap([\mmlm(Col, color(square, chi2)), "-statistic."])
        ].

% Buggy-Rule: Forget to square z.
buggy(chisq, stage(2), From, To, [step(buggy, square2, [Z])]) :-
    From = chi2ratio(Z^2),
    To = chi2ratio(omit_right(square2, abs(Z)^2)).

feedback(square2, [Z], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, color(square, Z)), "-statistic"]), " ",
          "instead of ", \nowrap([\mmlm(Col, [color(square2, chi2)]), "."]), " ",
          "Please do not forget to raise ", \mmlm(Col, Z), " to the square."
        ].

hint(square2, Col, H)
 => H = [ "Do not forget to raise ",
           "the ", \nowrap([\mmlm(Col, color(square2, z)), "-value"]), " ",
           "to the square to obtain ",
           "the ", \nowrap([\mmlm(Col, color(square2, chi2)), "-statistic."])
        ].

% Add probabilities instead of subtracting them. 
buggy(chisq, stage(2), From, To, [step(buggy, zadd, [P_VR, P_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(instead(zadd, P_VR + P_Box, P_VR - P_Box), 
           sqrt(P_Pool * (1 - P_Pool) * (frac(1, N_VR) + frac(1, N_Box)))).

feedback(zadd, [P_VR, P_Box], Col, F)
 => F = [ "The results matches the ", \nowrap([\mmlm(Col, z), "-statistic"]), " where ",
          \mmlm(Col, color(zadd, P_Box)), " was added to ", 
          \mmlm(Col, color(zadd, P_VR)), ", rather than subtracted from it.",
          " Please use the difference of ", \mmlm(Col, color(zadd, P_Box)), " and ",
          \mmlm(Col, color(zadd, P_VR)), " in the numerator of the ",
          \nowrap([\mmlm(Col, z), "-statistic."])
        ].

hint(zadd, Col, H)
 => H = [ "The numerator of the test statistic includes the difference ",
          "between the success ",
          "proportions ", \mmlm(Col, color(zadd, p_vr)), " ",
          "and ", \mmlm(Col, color(zadd, p_box))
        ].

% Forgot parentheses around (1/N_VR + 1/N_Box). 
buggy(chisq, stage(2), From, To, [step(buggy, paren2, [N_VR, N_Box])]) :-
    From = A * B * (1 / N_VR + 1 / N_Box),
    To = A * B * color(paren2, 1) / color(paren2, N_VR) + color(paren2, 1) / color(paren2, N_Box).

feedback(paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "The results matches the ", \nowrap([\mmlm(Col, z), "-statistic"]), " without ",
	   "the parenthesis around ", 
	   \mmlm(Col, color(paren2, paren(color("#000000", frac(1, N_VR) + frac(1, N_Box))))),
	   ". Please do not forget the paranthesis around ",
	   \mmlm(Col, color(paren2, paren(color("#000000", frac(1, N_VR) + frac(1, N_Box))))),
           "." 
         ].

hint(paren2, Col, F)
 => F = [ "Do not forget to add parentheses around ", 
          \mmlm(Col, color(paren2,
            paren(color("#000000", frac(1, n_vr) + frac(1, n_box)))))
        ].

% Buggy-Rule: Forgot school math
buggy(chisq, stage(2), From, To, [step(buggy, school1, [N_VR, N_Box])]) :-
    dif(N_VR, N_Box),
    From = frac(1, N_VR) + frac(1, N_Box),
    To = instead(school1, color(school1, frac(1, N_VR + N_Box)),
           1 / N_VR + 1 / N_Box, frac(1, N_VR) + frac(1, N_Box)).

feedback(school1, [A, B], Col, F)
 => F = [ " Please keep in mind that ", 
          \mmlm(Col, [color(school, color("black", frac(1, A)) + color("black", frac(1, B)))
            =\= frac(1, color(school, color("black", A) + color("black", B))), "."])
        ].

hint(school1, Col, F)
 => F = [ "Please do not forget school ",
          "math, ", \mmlm(Col, [frac(1, color(school1, n_vr)) +
          frac(1, color(school1, n_box)) =\= frac(1, color(school1, n_vr + n_box)), "."])
        ].

% Buggy-Rule: Forgot parentheses around denominator in main formula. 
% Appeared 1-4 times in the 2018 exams.
/* buggy(chisq, stage(2), From, To, [step(buggy, paren3, [From])]) :-
    From = P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box),
    To = instead(paren3, P_Pool * 1 - P_Pool * 1 / N_VR + 1 / N_Box, From).

feedback(paren3, [From], Col, FB) =>
    FB = [ "The results matches the ", \nowrap([\mmlm(Col, z), "-statistic"]), " without ",
	   "the parenthesis around the different elements in ", 
	   \mmlm(Col, color(paren3, From)), ". Please do not forget the paranthesis ",
	   " in the denominator of ", \mmlm(Col, color(paren3, From))
	 ].

hint(paren3, Col, FB)
 => F = [ "Remember the parentheses around the different ",
          " elements in the denominator."
	]. 
*/

% Buggy-Rule: flipped nominator and denominator in main equation. 
% Appeared 3-5 times in the 2018 exams.
/* buggy(chisq, stage(2), From, To, [step(buggy, flip, [])]) :-
    From = '<-'(z, zstat(P_VR, P_Box, P_pool, N_VR, N_Box)),
    To = '<-'(z, add_left(flip, 1/zstat(P_VR, P_Box, P_pool, N_VR, N_Box))).

feedback(flip, [], Col, FB) =>
    FB = ["The result matches the reciprocal of the test statistic ", 
	  \mmlm(Col, ["(", color(flip, 1 / (color("#000000", z))), ")"]),
	  ". Please double check the nominator and denominator of the test statistic." 
	 ].

hint(flip, _Col, FB)
 => F = "Do not flip the numerator and denominator of the test statistic.".
*/
