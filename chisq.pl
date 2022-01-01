% Prettier symbols for mathematical rendering
mathml:hook(Flags, p_VR, [task(chisq) | Flags], sub(p, "VR")).
mathml:hook(Flags, p_Box, [task(chisq) | Flags], sub(p, "Box")).
mathml:hook(Flags, s_VR, [task(chisq) | Flags], sub(s, "VR")).
mathml:hook(Flags, s_Box, [task(chisq) | Flags], sub(s, "Box")).
mathml:hook(Flags, n_VR, [task(chisq) | Flags], sub(n, "VR")).
mathml:hook(Flags, n_Box, [task(chisq) | Flags], sub(n, "Box")).
mathml:hook(Flags, chi2, [task(chisq) | Flags], chi^2).
mathml:hook(Flags, p_pool, [task(chisq) | Flags], sub(p, "pool")).

% R constants
interval:r_hook(p_VR).
interval:r_hook(s_VR).
interval:r_hook(n_VR).
interval:r_hook(p_Box).
interval:r_hook(s_Box).
interval:r_hook(n_Box).
interval:r_hook(z).
interval:r_hook(chi2).
interval:r_hook(p_pool).

render(chisq, item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box), Form) -->
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
                  "group ", \mmlm([task(chisq), digits(0)], ["(", (r(P_VR*100)), "%"]), " ",
                  "vs. ", \mmlm([task(chisq), digits(0)], [(r(P_Box*100)), "%)."]), " The ",
                  "percentages correspond to ", \mmlm([task(chisq), digits(0)], r(S_VR)), " ",
                  "people (out of ", \mmlm([task(chisq), digits(0)], [r(N_VR), ")"]), " in ",
                  "the VR group and ", \mmlm([task(chisq), digits(0)], r(S_Box)), " people ",
                  "(out of ", \mmlm([task(chisq), digits(0)], [r(N_Box), ")"]), " in the Box ",
                  "group.”"
              ])))
	      ])),
        \htmlform([ "Does VR training lead to faster surgery times than ",
            "traditional Box training? Please ",
            "determine the ", \mmlm([task(chisq)], hyph(chi^2, "statistic.")) ], "#chisq", R)
      ]).

intermediate(_, item).
start(chisq, item(p_VR, s_VR, n_VR, p_Box, s_Box, n_Box)).

% Correct solution
intermediate(chisq, ppool).
intermediate(chisq, zstat).
expert(chisq, stage(1), From, To, [step(expert, steps, [])]) :-
    From = item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box),
    To = { '<-'(p_pool, ppool(S_VR, S_Box, N_VR, N_Box)) ;
           '<-'(z, zstat(P_VR, P_Box, p_pool, N_VR, N_Box)) ;
           '<-'(chi2, chi2ratio(z^2)) ;
           chi2
         }.

feedback(chisq, steps, [], _Col, FB) =>
    FB = [ "Correctly identified the main steps of the calculation." ].

hint(chisq, steps, [], Col, FB) =>
    FB = [ "First determine the pooled success proportion, then ",
           "the ", \mmlm(Col, hyph(z, "statistic,")), " then raise it to the ",
           "square to obtain the ", \mmlm(Col, chi^2)
         ].

% Pooled proportion of successes
expert(chisq, stage(1), From, To, [step(expert, ppool, [S_VR, S_Box, N_VR, N_Box])]) :-
    From = ppool(S_VR, S_Box, N_VR, N_Box),
    To = dfrac(S_VR + S_Box, N_VR + N_Box).

feedback(chisq, ppool, [_S_VR, _S_Box, _N_VR, _N_Box], _Col, FB) =>
    FB = "Correctly determined the pooled success proportion.".

hint(chisq, ppool, [S_VR, S_Box, N_VR, N_Box], Col, FB) =>
    FB = [ "The pooled proportion of successes ",
           "is ", \mmlm(Col, dfrac(S_VR + S_Box, N_VR + N_Box))
         ].

% 5) - instead of + for both parts of p_pool.
% Appeared 1-2 times in the 2018 exams.
%
% Matthias, todo: unclear why abs is needed. Mistake in interval?
buggy(chisq, stage(1), From, To, [step(buggy, pdiff, [])]) :-
    From = ppool(S_VR, S_Box, N_VR, N_Box),
    To = dfrac(instead(bug(pdiff), S_VR - S_Box, S_VR + S_Box),
               instead(bug(pdiff), N_VR - N_Box, N_VR + N_Box)).

feedback(chisq, pdiff, [], Col, FB) =>
    FB = [ "Please add up the numbers in both the numerator and the ",
           "denominator of ", \mmlm(Col, color(pdiff, p_pool))
         ].

hint(chisq, pdiff, [], Col, FB) =>
    FB = [ "Remember to add up the numbers in when calculating the pooled ",
           "success proportion ", \mmlm(Col, color(pdiff, p_pool))
         ].

% Determine z-statistic
expert(chisq, stage(2), From, To, [step(expert, zstat, [P_VR, P_Box, P_Pool, N_VR, N_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box))).

feedback(chisq, zstat, [_P_VR, _P_Box, _P_Pool, _N_VR, _N_Box], Col, FB) =>
    FB = [ "Correctly determined the ", \mmlm(Col, hyph(z, "statistic.")) ].

hint(chisq, zstat, [P_VR, P_Box, P_Pool, N_VR, N_Box], Col, FB) =>
    FB = [ "The ", \mmlm(Col, hyph(z, "statistic")), " is ",
           \mmlm(Col, dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box)))) 
         ].

% 1) Forget to square z. 
% Appeared 41-49 times in the 2018 exams (upper end of interval represents results
% that could be caused by the listed error but erroneously rounded, 
% lower end is number of exact matches).
buggy(chisq, stage(2), From, To, [step(buggy, square, [])]) :-
    From = ('<-'(chi2, chi2ratio(Z^2)) ; chi2),
    To = (omit(bug(square), '<-'(chi2, Z^2)) ; tratio(Z)).

feedback(chisq, square, [], Col, FB) =>
    FB = [ "The result matches ",
           "the ", \mmlm(Col, hyph(color(square, z), "statistic")), " instead ",
           "of the ", \mmlm(Col, [color(square, chi2), "."])
         ].

hint(chisq, square, [], Col, FB) =>
    FB = [ "Do not forget to raise ",
           "the ", \mmlm(Col, hyph(color(square, z), "value")), " ",
           "to the square to obtain ",
           "the ", \mmlm(Col, hyph(color(square, chi2), "statistic."))
         ].

% 2) Add probabilities instead of subtracting them. 
% Appeared 3-8 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, zadd, [P_VR, P_Box])]) :-
    From = zstat(P_VR, P_Box, P_Pool, N_VR, N_Box),
    To = dfrac(instead(bug(zadd), P_VR + P_Box, P_VR - P_Box), sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box))).

feedback(chisq, zadd, [P_VR, P_Box], Col, FB) =>
    FB = [ \mmlm(Col, color(zadd, P_Box)), " was added to ", 
           \mmlm(Col, color(zadd, P_VR)), ", rather than subtracted from it." 
         ].

hint(chisq, zadd, [P_VR, P_Box], Col, FB) =>
    FB = [ "The numerator of the test statistic includes the difference ",
           "between the success ",
           "proportions ", \mmlm(Col, color(zadd, P_VR)), " ",
           "and ", \mmlm(Col, color(zadd, P_Box))
         ].

% 3) Forgot parentheses around (1/N_VR + 1/N_Box). 
% Appeared 3-7 times in the 2018 exams.
%buggy(chisq, stage(2), From, To, [step(buggy, paren2, [N_VR, N_Box])]) :-
%    From = A * B * (1 / N_VR + 1 / N_Box),
%    To = A * B * color(paren2, 1) / color(paren2, N_VR) + color(paren2, 1) / color(paren2, N_Box).

feedback(chisq, paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "The parenthesis around ", 
           \mmlm(Col, color(paren2, 
             paren(color("#000000", 1 / N_VR + 1 / N_Box)))), " has been ",
           "omitted." 
         ].

hint(chisq, paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "Do not forget to add parentheses around ", 
           \mmlm(Col, color(paren2,
             paren(color("#000000", 1 / N_VR + 1 / N_Box))))
         ].

% 4) Forgot parentheses around denominator in main formula. 
% Appeared 1-4 times in the 2018 exams.
%buggy(chisq, stage(2), From, To, [step(buggy, paren3, [From])]) :-
%    From = P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box),
%    To = instead(bug(paren3), P_Pool * 1 - P_Pool * 1 / N_VR + 1 / N_Box, From).

feedback(chisq, paren3, [From], Col, FB) =>
    FB = [ "You forgot the parentheses around the different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

hint(chisq, paren3, [From], Col, FB) =>
    FB = [ "Remember the parentheses around he different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

% 6) flipped nominator and denominator in main equation. 
% Appeared 3-5 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, flip, [])]) :-
    From = '<-'(z, zstat(P_VR, P_Box, P_pool, N_VR, N_Box)),
    To = '<-'(z, invent_left(bug(flip), 1/zstat(P_VR, P_Box, P_pool, N_VR, N_Box))).

feedback(chisq, flip, [], _Col, FB) =>
    FB = "The result matches the reciprocal of the test statistic.".

hint(chisq, flip, [], _Col, FB) =>
    FB = "Do not flip the numerator and denominator of the test statistic.".

