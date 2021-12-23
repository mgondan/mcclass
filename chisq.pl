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
interval:hook(pl, p_VR, r(p_VR)).
interval:hook(pl, s_VR, r(s_VR)).
interval:hook(pl, n_VR, r(n_VR)).
interval:hook(pl, p_Box, r(p_Box)).
interval:hook(pl, s_Box, r(s_Box)).
interval:hook(pl, n_Box, r(n_Box)).
interval:hook(pl, chi2, r(chi2)).
interval:hook(pl, p_pool, r(p_pool)).

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
                  "group ", \mmlm([round(0)], ["(", (r(P_VR*100)), "%"]), " ",
                  "vs. ", \mmlm([round(0)], [(r(P_Box*100)), "%)."]), " The ",
                  "percentages correspond to ", \mmlm([round(0)], r(S_VR)), " ",
                  "people (out of ", \mmlm([round(0)], [r(N_VR), ")"]), " in ",
                  "the VR group and ", \mmlm([round(0)], r(S_Box)), " people ",
                  "(out of ", \mmlm([round(0)], [r(N_Box), ")"]), " in the Box ",
                  "group.”"
              ])))
	      ])),
        \htmlform([ "Does VR training lead to faster surgery times than ",
            "traditional Box training? Please ",
            "determine ", \mmlm([], hyph(chi^2, "statistic.")) ], "#chisq", R)
      ]).

intermediate(_, item).
start(chisq, item(p_VR, s_VR, n_VR, p_Box, s_Box, n_Box)).

% Correct soultion.
expert(chisq, stage(2), From, To, [step(expert, allinone, [])]) :-
    From = item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box),
    To = { '<-'(p_pool, dfrac(S_VR + S_Box, N_VR + N_Box)) ;
	   '<-'(chi2, dfrac((P_VR - P_Box) ^ 2, 
		p_pool * (1 - p_pool) * (1 / N_VR + 1 / N_Box)));
	    chi2
	 }.

feedback(chisq, allinone, [], Col, FB) =>
    FB = [ "Good job, you correctly calculated the ", \mmlm(Col, hyph(chi^2, "statistic.")) ].

hint(chisq, allinone, [], _Col, FB) =>
    FB = [ "Try to do everything correctly." ].

% 1) Forgot to square z. 
% Appeared 41-49 times in the 2018 exams (upper end of interval represents results
% that could be caused by the listed error but erroneously rounded, 
% lower end is number of exact matches).
buggy(chisq, stage(2), From, To, [step(buggy, z, [])]) :-
    From = dfrac((P_VR - P_Box) ^ 2, 
		P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box)),
    To = dfrac(P_VR - P_Box, sqrt(P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box))).

feedback(chisq, z, [], Col, FB) =>
    FB = [ "You calculated ", \mmlm(Col, color(z, z)), " rather than ", 
	   \mmlm(Col, [color(z, chi2), "."]),
	   " Square your answer to get the correct result." ].

hint(chisq, z, [], Col, FB) =>
    FB = [ "Your goal is to calculate ", \mmlm(Col, color(z, chi^2)), 
	   " you can get there by squaring ", \mmlm(Col, color(z, z)) ].

% 2) first instead of second binomial formula. 
% Appeared 3-8 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, firstbin, [P_VR, P_Box])]) :-
    From = (P_VR - P_Box) ^ 2,
    To = instead(bug(firstbin), (P_VR + P_Box) ^ 2, From).

feedback(chisq, firstbin, [P_VR, P_Box], Col, FB) =>
    FB = [ \mmlm(Col, color(firstbin, P_Box)), " was added to ", 
	   \mmlm(Col, color(firstbin, P_VR)), ", rather than subtracted from it." ].

hint(chisq, firstbin, [P_VR, P_Box], Col, FB) =>
    FB = [ "Try subtracting ", \mmlm(Col, color(firstbin, P_VR)), " from ",
	   \mmlm(Col, color(firstbin, P_Box)), " instead." ].

% 3) Forgot parentheses around (1/N_VR + 1/N_Box). 
% Appeared 3-7 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, paren2, [N_VR, N_Box])]) :-
    From = A * B * (1 / N_VR + 1 / N_Box),
    X = paren2,
    To = A * B * color(X, 1) / color(X, N_VR) + color(X, 1) / color(X, N_Box).

feedback(chisq, paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "The parenthesis around ", 
	   \mmlm(Col, color(paren2, ["(", 1 / N_VR + 1 / N_Box, ")"])),
	   "has been omitted." ].

hint(chisq, paren2, [N_VR, N_Box], Col, FB) =>
    FB = [ "Do not forget to add parenthesis around ", 
	   \mmlm(Col, color(paren2, ["(", 1 / N_VR + 1 / N_Box, ")"])) ].

% 4) Forgot parentheses around denominator in main formula. 
% Appeared 1-4 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, paren3, [From])]) :-
    From = P_Pool * (1 - P_Pool) * (1 / N_VR + 1 / N_Box),
    To = instead(bug(paren3), P_Pool * 1 - P_Pool * 1 / N_VR + 1 / N_Box, From).

feedback(chisq, paren3, [From], Col, FB) =>
    FB = [ "You forgot the parentheses around the different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

hint(chisq, paren3, [From], Col, FB) =>
    FB = [ "Remember the parentheses around he different ",
	   " elements in ", \mmlm(Col, color(paren3, From)) ].

% 5) - instead of + for both parts of p_pool. 
% Appeared 1-2 times in the 2018 exams.
buggy(chisq, stage(2), From, To, Flags) :-
    Flags = [step(buggy, subt, [])],
    From = dfrac(S_VR + S_Box, N_VR + N_Box),
    To = color(subt, dfrac(S_VR - S_Box, N_VR - N_Box)).

feedback(chisq, subt, [], Col, FB) =>
    FB = [ "Please use addition rather than subtraction in both numerator ",
	   "and denominator when calculating ",
	   \mmlm(Col, color(subt, p_pool)) ].

hint(chisq, subt, [], Col, FB) =>
    FB = [  "Try using addition instead of subtraction in both elements of ", 
	    \mmlm(Col, color(subt, p_pool)) ].

% 6) flipped nominator and denominator in main equation. 
% Appeared 3-5 times in the 2018 exams.
buggy(chisq, stage(2), From, To, [step(buggy, flip, [])]) :-
    From = dfrac((P_VR - P_Box) ^ 2, 
	   p_pool * (1 - p_pool) * (1 / N_VR + 1 / N_Box)),
    To = instead(bug(flip), dfrac(p_pool * (1 - p_pool) * (1 / N_VR + 1 / N_Box), 
	 (P_VR - P_Box) ^ 2), From).

feedback(chisq, flip, [], Col, FB) =>
    FB = [ "You flipped the numerator and denominator of ", 
	   \mmlm(Col, color(flip, chi2)) ].

hint(chisq, flip, [], Col, FB) =>
    FB = [ "Flip the numerator and denominator to get the correct ",
	   \mmlm(Col, hyph(chi2, "Formula")) ].
