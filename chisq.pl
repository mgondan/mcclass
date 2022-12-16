:- module(chisq, []).

:- use_module(rint).
:- use_module(table).
:- use_module(navbar).
navbar:page(chisq, "chi-square").

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(p_VR, sub(p, "VR")).
mathml_hook(p_Box, sub(p, "Box")).
mathml_hook(s_VR, sub(s, "VR")).
mathml_hook(s_Box, sub(s, "Box")).
mathml_hook(n_VR, sub(n, "VR")).
mathml_hook(n_Box, sub(n, "Box")).
mathml_hook(chi2, chi^2).
mathml_hook(p_pool, sub(p, "pool")).

% R constants
rint:r_hook(p_VR).
rint:r_hook(s_VR).
rint:r_hook(n_VR).
rint:r_hook(p_Box).
rint:r_hook(s_Box).
rint:r_hook(n_Box).
rint:r_hook(z).
rint:r_hook(chi2).
rint:r_hook(p_pool).

render(item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box), Form) -->
    { option(resp(R), Form, "#.##") },
    html(
      [ div(class(card), div(class("card-body"),
          [ h1(class("card-title"), "Training of surgical skills"),
            p(class("card-text"),
              [ "Surgeons need special motor skills, especially for ",
                "endoscopic surgery through the belly. Nickel et al. (2015) ",
                "report the results of a study with two learning methods for ",
                "motor skill training. One group underwent a virtual reality ",
                "training (VR group), the other group participated in a ",
                "mixture of online courses and classical training of motor ",
                "skill with the so-called Box-trainer (Box group). "
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
                  "two groups. (...) The VR group completed the operation more ",
                  "often within 80 min than the Box ",
                  "group ", \mmlm([digits(0)], ["(", (r(P_VR*100)), "%"]), " ",
                  "vs. ", \mmlm([digits(0)], [(r(P_Box*100)), "%)."]), " The ",
                  "percentages correspond to ", \mmlm(r(S_VR)), " ",
                  "people (out of ", \mmlm([r(N_VR), ")"]), " in ",
                  "the VR group and ", \mmlm(r(S_Box)), " people ",
                  "(out of ", \mmlm([r(N_Box), ")"]), " in the Box ",
                  "group.”"
              ])))
	      ])),
        \htmlform([ "Does VR training lead to faster surgery times than ",
            "traditional Box training? Please ",
            "determine the ", \mmlm(hyph(chi^2, "statistic.")) ], "#chisq", R)
      ]).

intermediate(item).
start(item(p_VR, s_VR, n_VR, p_Box, s_Box, n_Box)).

% Correct solution
intermediate(ppool).
intermediate(zstat).
expert(stage(1), From, To, [step(expert, steps, [])]) :-
    From = item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box),
    To = { '<-'(p_pool, ppool(S_VR, S_Box, N_VR, N_Box)) ;
           '<-'(z, zstat(P_VR, P_Box, p_pool, N_VR, N_Box)) ;
           '<-'(chi2, chi2ratio(z^2)) ;
           chi2
         }.

feedback(steps, [], _Col, FB) =>
    FB = [ "Correctly identified the main steps of the calculation." ].

hint(steps, [], Col, FB) =>
    FB = [ "First determine the pooled success proportion, then ",
           "the ", \mmlm(Col, hyph(z, "statistic,")), " lastly, square ",
           "it to obtain ", \mmlm(Col, chi^2)
         ].

% Pooled proportion of successes
expert(stage(1), From, To, [step(expert, ppool, [S_VR, S_Box, N_VR, N_Box])]) :-
    From = ppool(S_VR, S_Box, N_VR, N_Box),
    To = dfrac(S_VR + S_Box, N_VR + N_Box).

feedback(ppool, [_S_VR, _S_Box, _N_VR, _N_Box], _Col, FB) =>
    FB = "Correctly determined the pooled success proportion.".

hint(ppool, [S_VR, S_Box, N_VR, N_Box], Col, FB) =>
    FB = [ "The pooled proportion of successes ",
           "is ", \mmlm(Col, dfrac(S_VR + S_Box, N_VR + N_Box))
         ].

% 1) - instead of + for both parts of p_pool.
% Appeared 1-2 times in the 2018 exams.
%
% Matthias, todo: unclear why abs is needed. Mistake in interval?
% Vincent: Feedback doesn't show, no version with only this bug.
%buggy(stage(1), From, To, [step(buggy, pdiff, [])]) :-
%    From = ppool(S_VR, S_Box, N_VR, N_Box),
%    To = dfrac(instead(pdiff, S_VR - S_Box, S_VR + S_Box),
%               instead(pdiff, N_VR - N_Box, N_VR + N_Box)).

feedback(pdiff, [], Col, FB) =>
    FB = [ "Please add up the numbers in both the numerator and the ",
           "denominator of ", \mmlm(Col, color(pdiff, p_pool))
         ].

hint(pdiff, [], Col, FB) =>
    FB = [ "Remember to add up the numbers in both the numerator and the ",
           "denominator when calculating the pooled success proportion ", 
	   \mmlm(Col, color(pdiff, p_pool))
         ].

% Determine z-statistic
expert(stage(2), From, To, [step(expert, zstat, [P_VR, P_Box, P_Pool, N_VR, N_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box))).

feedback(zstat, [_P_VR, _P_Box, _P_Pool, _N_VR, _N_Box], Col, FB) =>
    FB = [ "Correctly determined the ", \mmlm(Col, hyph(z, "statistic.")) ].

hint(zstat, [P_VR, P_Box, P_Pool, N_VR, N_Box], Col, FB) =>
    FB = [ "The ", \mmlm(Col, hyph(z, "statistic")), " is ",
           \mmlm(Col, dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box)))) 
         ].

% 2) Forget to square z. 
% Appeared 41-49 times in the 2018 exams (upper end of interval represents results
% that could be caused by the listed error but erroneously rounded, 
% lower end is number of exact matches).
buggy(stage(2), From, To, [step(buggy, square, [])]) :-
    From = ('<-'(chi2, chi2ratio(Z^2)) ; chi2),
    To = (omit(square, '<-'(chi2, Z^2)) ; tstat(Z)).

feedback(square, [], Col, FB) =>
    FB = [ "The result matches ",
           "the ", \mmlm(Col, hyph(color(square, z), "statistic")), " instead ",
           "of the ", \mmlm(Col, [color(square, chi2), "."])
         ].

hint(square, [], Col, FB) =>
    FB = [ "Do not forget to raise ",
           "the ", \mmlm(Col, hyph(color(square, z), "value")), " ",
           "to the square to obtain ",
           "the ", \mmlm(Col, hyph(color(square, chi2), "statistic."))
         ].

% 3) Add probabilities instead of subtracting them. 
% Appeared 3-8 times in the 2018 exams.
buggy(stage(2), From, To, [step(buggy, zadd, [P_VR, P_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(instead(zadd, P_VR + P_Box, P_VR - P_Box), sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box))).

feedback(zadd, [P_VR, P_Box], Col, FB) =>
    FB = [ \mmlm(Col, color(zadd, P_Box)), " was added to ", 
           \mmlm(Col, color(zadd, P_VR)), ", rather than subtracted from it." 
         ].

hint(zadd, [P_VR, P_Box], Col, FB) =>
    FB = [ "The numerator of the test statistic includes the difference ",
           "between the success ",
           "proportions ", \mmlm(Col, color(zadd, P_VR)), " ",
           "and ", \mmlm(Col, color(zadd, P_Box))
         ].

% 4) Forgot parentheses around (1/N_VR + 1/N_Box). 
% Appeared 3-7 times in the 2018 exams.
buggy(stage(2), From, To, [step(buggy, paren2, [N_VR, N_Box])]) :-
    From = A * B * (1 / N_VR + 1 / N_Box),
    To = A * B * color(paren2, 1) / color(paren2, N_VR) + color(paren2, 1) / color(paren2, N_Box).

feedback(paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "The parenthesis around ", 
           \mmlm(Col, color(paren2, 
             paren(color("#000000", 1 / N_VR + 1 / N_Box)))), " has been ",
           "omitted." 
         ].

hint(paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "Do not forget to add parentheses around ", 
           \mmlm(Col, color(paren2,
             paren(color("#000000", 1 / N_VR + 1 / N_Box))))
         ].

% 5) Forgot parentheses around denominator in main formula. 
% Appeared 1-4 times in the 2018 exams.
buggy(stage(2), From, To, [step(buggy, paren3, [From])]) :-
    From = P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box),
    To = instead(paren3, P_Pool * 1 - P_Pool * 1 / N_VR + 1 / N_Box, From).

feedback(paren3, [From], Col, FB) =>
    FB = [ "Please remember the parentheses around the different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

hint(paren3, [From], Col, FB) =>
    FB = [ "Remember the parentheses around he different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

% 6) flipped nominator and denominator in main equation. 
% Appeared 3-5 times in the 2018 exams.
buggy(stage(2), From, To, [step(buggy, flip, [])]) :-
    From = '<-'(z, zstat(P_VR, P_Box, P_pool, N_VR, N_Box)),
    To = '<-'(z, invent_left(flip, 1/zstat(P_VR, P_Box, P_pool, N_VR, N_Box))).

feedback(flip, [], Col, FB) =>
    FB = ["The result matches the reciprocal of the test statistic ", 
	 \mmlm(Col, ["(", color(flip, 1 / (color("#000000", z))), ")"])
	 ].

hint(flip, [], _Col, FB) =>
    FB = "Do not flip the numerator and denominator of the test statistic.".
