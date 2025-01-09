:- module(mathmltest, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module('/home/jeremyirilli/interval/prolog/mcclass.pl').
:- use_module('/home/jeremyirilli/interval/prolog/rint.pl').
:- use_module('/home/jeremyirilli/interval/prolog/r.pl').
%:- use_module(r).
%:- use_module(rint).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(mathmltest, ["Mathml playground"]).

task(tratio).
/* task(pvalue).
task(cipaired). */

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(d, overline('D')).
math_hook(s_d, subscript(s, 'D')).
math_hook(n, 'N').
math_hook(t0, overline("T0")).
math_hook(s_t0, subscript(s, "T0")).
math_hook(eot, overline("EOT")).
math_hook(s_eot, subscript(s, "EOT")).
math_hook(s2p, subscript(s, "pool")^2).
math_hook(paired(D, Mu, S_D, N), fn("paired", [D, Mu, S_D, N])).
math_hook(alpha, greek("alpha")).

% R definitions
rint:r_hook(var_pool(_N1, _V1, _N2, _V2)).
rint:r_hook(t).
rint:r_hook(d).
rint:r_hook(mu).
rint:r_hook(s_d).
rint:r_hook(n).
rint:r_hook(t0).
rint:r_hook(s_t0).
rint:r_hook(eot).
rint:r_hook(s_eot).
rint:r_hook(lo).
rint:r_hook(pt(_T, _DF)).
rint:r_hook(qt(_P, _DF)).
 
interval:monotonical(pt(+, /)).


% Task description
render
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, D, S_D, _N, Mu, _Alpha)) },
    html(
      div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Mathml playground"),
          p(class('card-text'),
            ["omit/1: ", \mmlm(omit(D - Mu))]),
          p(class('card-text'),
            ["omit/2: ", \mmlm([error(highlight)], omit(bug1, D - Mu))]),
          p(class('card-text'),
            ["omit_left/1: ", \mmlm(omit_left(D - Mu))]),
          p(class('card-text'),
            ["omit_left/2: ", \mmlm([error(highlight)], omit_left(bug1, D - Mu))]),
          p(class('card-text'),
            ["omit_right/1: ", \mmlm(omit_right(D - Mu))]),
          p(class('card-text'),
            ["omit_right/2 [error(highlight)]: ", \mmlm([error(highlight)], omit_right(bug1, D - Mu))]),
          p(class('card-text'),
            ["omit_right/2 [error(fix)]: ", \mmlm([error(fix)], omit_right(bug1, D - Mu))]),
          p(class('card-text'),
            ["omit_right/2 [error(show)]: ", \mmlm([error(show)], omit_right(bug1, D - Mu))]),
          p(class('card-text'),
            ["drop_right [error(highlight)]: ", \mmlm([error(highlight)], drop_right(bug1, D - Mu))]),
          p(class('card-text'),
            ["drop_right [error(fix)]: ", \mmlm([error(fix)], drop_right(bug1, D - Mu))]),
          p(class('card-text'),
            ["drop_left/2: ", \mmlm(drop_left(bug1, D - Mu))]),
          p(class('card-text'),
            ["instead/1: ", \mmlm(instead(D, S_D))]),
          p(class('card-text'),
            ["instead/2: ", \mmlm([error(highlight)], instead(bug1, D, S_D))]),
          p(class('card-text'),
            ["add_left/1: ", \mmlm(add_left(D+S_D))]),
          p(class('card-text'),
            ["add_left/2: ", \mmlm([error(highlight)], add_left(bug1, D+S_D))]),
          p(class('card-text'),
            ["add_right/1: ", \mmlm(add_right(D+S_D))]),
          p(class('card-text'),
            ["add_right/2: ", \mmlm([error(highlight)], add_right(bug1, D+S_D))]),
          div(class(container),
            div(class("row justify-content-md-center"),
              div(class("col-6"),
                \htmltable(
                  [ em("Table 1. "), "Observed HDRS scores at T0, EOT, ",
                    "and ", \mmlm('D' = "T0" - "EOT") ],
                  [ "Average", "SD" ],
                  [ "HDRS", "T0", "EOT", \mmlm(d) ],
                  [ [ \mmlm([digits(1)], r(t0)),
                      \mmlm([digits(1)], r(eot)),
                      \mmlm([digits(1)], r(d1)) ],
                    [ \mmlm([digits(1)], r(s_t0)),
                      \mmlm([digits(1)], r(s_eot)),
                      \mmlm([digits(1)], r(s1_d)) ]
                  ])))),
          \download(tpaired)
        ]))).

% Question for the t-ratio
task(tratio)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, Mu, _Alpha)),
      session_data(resp(tpaired, tratio, Resp), resp(tpaired, tratio, '#.##'))
    },
    html(\htmlform([ "Does rfCBT lead to a relevant reduction (i.e., more ",
        "than ", \mmlm([digits(1)], Mu = r(Mu)), " units) in mean HDRS ",
        "scores between baseline (T0) and End of Treatment (EOT)? ",
        "Please report the ", \mmlm(hyph(t, "ratio.")) ], tratio, Resp)).

/* % Question for the p-value
task(pvalue)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, Mu, _Alpha)),
      session_data(resp(tpaired, pvalue, Resp), resp(tpaired, pvalue, '.###'))
    },
    html(\htmlform([ "Does rfCBT lead to a relevant reduction (i.e., more ",
        "than ", \mmlm([digits(1)], Mu = r(Mu)), " units) in mean HDRS ",
        "scores between baseline (T0) and End of Treatment (EOT)? ",
        "Please report the ", \mmlm(hyph(p, "value?")) ], pvalue, Resp)).

% Question for the confidence interval
task(cipaired)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, _Mu, _Alpha)),
      session_data(resp(tpaired, cipaired, Resp), resp(tpaired, cipaired, '#.# to #.#'))
    },
    html(\htmlform([ "Determine the confidence interval for the change in ",
        "the patients\u0027 HDRS scores." ], cipaired, Resp)). */

%
%% Expert rules for the t-ratio task
%
% t-test for paired samples
intermediate(tratio, item).
start(item(t0, s_t0, eot, s_eot, d, s_d, n, mu, alpha)).

% First step: Extract the correct information for a paired t-test from the task
% description
intermediate(tratio, paired).
expert(tratio, stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, Mu, _Alpha),
    Y = { '<-'(t, paired(D, Mu, S_D, N)) }.

feedback(paired, [], Col, F)
 => F = [ "Correctly recognised the problem as a ",
          \mmlm(Col, hyph(t, "test")), " for paired samples." 
        ].

hint(paired, [], Col, F)
 => F = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for paired ",
          "samples." 
        ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
expert(tratio, stage(2), X, Y, [step(expert, tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(dfrac(D - Mu, S_D / sqrt(N))).

feedback(tratio, [_D, _Mu, _S_D, _N], Col, F)
 => F = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")), " for ",
          "paired samples." 
        ].

hint(tratio, [D, Mu, S_D, N], Col, F)
 => F = [ "The ", \mmlm(Col, hyph(t, "ratio")), " ",
          "is ", \mmlm(Col, [dfrac(D - Mu, S_D / sqrt(N)), "."])
        ].

% Another correct result
expert(tratio, stage(2), X, Y, [step(expert, abs_tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(abs(dfrac(D - Mu, S_D / sqrt(N)))).

feedback(abs_tratio, [_D, _Mu, _S_D, _N], Col, F)
 => F = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")), " for ",
          "paired samples." 
        ].

hint(abs_tratio, [D, Mu, S_D, N], Col, F)
 => F = [ "The ", \mmlm(Col, hyph(t, "ratio")),
          " is ", \mmlm(Col, [abs(dfrac(D - Mu, S_D / sqrt(N))), "."])
        ].

%
%% Buggy-Rules for the for the t-ratio task
%
% Buggy-Rule: Omit the null hypothesis Mu
% Misconception: Run the paired t-test against zero, that is, just test for a
% decrease in symptoms. This is a frequent misconception.
% The problem is known as "regression to the mean": 
% Scores at T0 tend to be systematically too low (patients present themselves
% at the hospital when they feel particularly ill). At EOT, the measurement 
% is not biased by self-selection. Therefore, we tend to see an improvement 
% even in the absence of any therapeutical effect. 
% This misconception is even built into SPSS, because the paired samples t-test
% in SPSS only allows for mu = 0.  
buggy(tratio, stage(2), X, Y, [step(buggy, mu, [Mu])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(dfrac(omit_right(mu, D - Mu), S_D / sqrt(N))).

feedback(mu, [Mu], Col, F)
 => F = [ "[omit_right] The result matches the ", \mmlm(hyph(t, "ratio,")), " when the null",
	   " hypothesis ", \mmlm(Col, color(mu, Mu)), " has been omitted.",
	   " Please do not forget ", \mmlm(Col, color(mu, Mu)), " in the ",
	   \mmlm(hyph(t, "ratio."))
        ].

hint(mu, [Mu], Col, F)
 => F = [ "Do not omit the null hypothesis ", \mmlm(Col, color(mu, Mu)),
          " in the ", \mmlm(hyph(t, "ratio.")) 
        ].

  
buggy(tratio, stage(2), X, Y, [step(buggy, mu2, [Mu])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(dfrac(drop_right(mu2, D - Mu), S_D / sqrt(N))).

feedback(mu2, [Mu], Col, F)
 => F = [ "[drop_right] The result matches the ", \mmlm(hyph(t, "ratio,")), " when the null",
	   " hypothesis ", \mmlm(Col, color(mu2, Mu)), " has been omitted.",
	   " Please do not forget ", \mmlm(Col, color(mu2, Mu)), " in the ",
	   \mmlm(hyph(t, "ratio."))
        ].

hint(mu2, [Mu], Col, F)
 => F = [ "Do not omit the null hypothesis ", \mmlm(Col, color(mu2, Mu)),
          " in the ", \mmlm(hyph(t, "ratio.")) 
        ].

% Buggy-Rule: t-test for independent samples
% Misconception: Run the t-test for independent samples despite the correlated
% measurements.
% First step of the Buggy-Rule: t-test for independent samples
% gathering the important data needed to solve the t-test for independent samples
% (both correct and incorrect solutions) from the task description
intermediate(tratio, indep).
buggy(tratio, stage(2), X, Y, [step(buggy, indep, [])]) :-
    X = item(T0, S_T0, EOT, S_EOT, D, S_D, N, Mu, _Alpha),
    Y = { '<-'(t, instead(indep, indep(T0, S_T0, N, EOT, S_EOT, N), 
            paired(D, Mu, S_D, N))) 
        }.

feedback(indep, [], Col, F)
 => F = [ "The problem was mistakenly identified as a ",
          \mmlm(Col, hyph(t, "test")), " for independent samples." 
        ].

hint(indep, [], Col, F)
 => F = [ "Do not calculate a ", \mmlm(Col, hyph(t, "test")), " for ",
          "independent samples here." 
        ].

% Second step of the Buggy-Rule: t-test for independent samples
% Determine the test statistic for the t-test for independent samples.
% The step itself is correct, although it is only needed
% if a wrong decision has been made before (bug indep).
expert(tratio, stage(2), X, Y, 
        [step(expert, tratio_indep, [T0, S_T0, N, EOT, S_EOT])]) :-
    X = indep(T0, S_T0, N, EOT, S_EOT, N),
    P = denote(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    Y = tstat(dfrac(T0 - EOT, sqrt(P * (1/N + 1/N)))).

feedback(tratio_indep, [_T0, _S_T0, _N, _EOT, _S_EOT], Col, F)
 => F = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")),
          " for independent samples." 
        ].

hint(tratio_indep, [T0, S_T0, N, EOT, S_EOT], Col, F)
 => P = denote(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    F = [ "The ", \mmlm(Col, hyph(t, "ratio")), " for independent samples ",
          "would be ", \mmlm(Col, [dfrac(T0 - EOT, sqrt(P * (1/N + 1/N))), "."]) ].

% The following mistake cannot occur in the paired t-test, but is again only
% possible if the student has already made the wrong decision to calculate the
% t-test for independent samples. We need it anyway, to be able to diagnose the
% numeric result.

% Buggy-Rule: Forgot school math [1/N1 + 1/N2 is not 1/(N1 + N2)]
% For mysterious reasons, everyone falls into this trap at least once,
% including me and the student assistants. I have coined it "school",
% since it is an example in which the person has forgotten school math.
buggy(tratio, stage(2), X, Y, [step(buggy, school1, [N1, N2])]) :-
    dif(N1, N2),
    X = 1/N1 + 1/N2,
    Y = frac(1, color(school1, N1 + N2)).

feedback(school1, [A, B], Col, F)
 => F = [ "The result matches the expression for the ",
	   \mmlm(Col, hyph(t, "ratio"))," for independent samples with ",
	   \mmlm(Col, [frac(1, color(school, color("black", A) + color("black", B))), "."]), 
	   " Please keep in mind that ", 
	   \mmlm(Col, [color(school, color("black", frac(1, A)) + color("black", frac(1, B)))
		 =\= frac(1, color(school, color("black", A) + color("black", B))), "."])
        ].

hint(school1, [N1, N2], Col, F)
 => F = [ "Please do not forget school ",
          "math, ", \mmlm(Col, [frac(1, color(school1, N1)) +
            frac(1, color(school1, N2)) =\= frac(1, color(school1, N1+N2)), "."]) 
        ].

% Buggy-Rule: Forgot school math (Same for N1 = N2)
buggy(tratio, stage(2), X, Y, [step(buggy, school2, [N])]) :-
    X = 1/N + 1/N,
    Y = frac(1, color(school2, 2*N)).

feedback(school2, [N], Col, F)
 => F = [ "The result matches the expression for the ", 
	   \mmlm(Col, hyph(t, "ratio")), " for independent samples with ",
	   \mmlm(Col, [frac(1, color(school2, 2*N)), "."]), " Please keep in mind that ",
	   \mmlm(Col, [frac(1, color(school2, N)) + frac(1, color(school2, N)) =\= frac(1, color(school2, 2*N)), "."]) 
        ].

hint(school2, [N], Col, F)
 => F = [ "Please do not forget school math, ",
          \mmlm(Col, [frac(1, color(school2, N)) + frac(1, color(school2, N)) =\= frac(1, color(school2, 2*N)), "."])
        ].

% Buggy-Rule: Forgot parentheses
% Forget parentheses in numerator and denominator of X / Y, with X = A - B and
% Y = C / D. That is, calculate A - (B / C) / D instead of (A - B) / (C / D).
% 
% This is the first buggy rule that ever came to my attention, therefore the
% name, bug1.
buggy(tratio, stage(2), X, Y, [step(buggy, bug1, [D, Mu, S, SQRT_N])]) :-
    X = dfrac(D - Mu, S / SQRT_N),
    M0 = drop_left(bug1, D - Mu),
    S0 = drop_right(bug1, S / SQRT_N),
    Y = add_left(bug1, 
        D - add_right(bug1, dfrac(M0, S0) / SQRT_N)).

feedback(bug1, [D, Mu, S, SQRT_N], Col, F)
 => F = [ "[drop] The result matches the fraction without parentheses around the ", 
	   "numerator and the denominator, ", \mmlm([error(correct) | Col], 
	       [dfrac(color(bug1, paren(color("#000000", D - Mu))), 
	           color(bug1, paren(color("#000000", S / SQRT_N)))), "."]),
	   " Please do not forget the parentheses around the numerator and the ",
	   "denominator of a fraction."
        ].

hint(bug1, [D, Mu, S, SQRT_N], Col, F)
 => F = [ "Do not forget the parentheses around the numerator and ",
          "the denominator of a fraction, ",
          \mmlm([error(correct) | Col],
            [dfrac(color(bug1, paren(color("#000000", D - Mu))), 
              color(bug1, paren(color("#000000", S / SQRT_N)))), "."]) 
        ].

buggy(tratio, stage(2), X, Y, [step(buggy, bug1_2, [D, Mu, S, SQRT_N])]) :-
    X = dfrac(D - Mu, S / SQRT_N),
    /*  M0 = drop_left(bug1_2, D - Mu),
    S0 = drop_right(bug1_2, S / SQRT_N), */
    Y = add_left(bug1_2, 
        D - add_right(bug1_2, dfrac(Mu, S) / SQRT_N)).

feedback(bug1_2, [D, Mu, S, SQRT_N], Col, F)
 => F = [ "[without drop] The result matches the fraction without parentheses around the ", 
	   "numerator and the denominator, ", \mmlm([error(correct) | Col], 
	       [dfrac(color(bug1_2, paren(color("#000000", D - Mu))), 
	           color(bug1_2, paren(color("#000000", S / SQRT_N)))), "."]),
	   " Please do not forget the parentheses around the numerator and the ",
	   "denominator of a fraction."
        ].

hint(bug1_2, [D, Mu, S, SQRT_N], Col, F)
 => F = [ "Do not forget the parentheses around the numerator and ",
          "the denominator of a fraction, ",
          \mmlm([error(correct) | Col],
            [dfrac(color(bug1_2, paren(color("#000000", D - Mu))), 
              color(bug1_2, paren(color("#000000", S / SQRT_N)))), "."]) 
        ].

% One challenging aspect of word problems ("Textaufgaben") is that students
% have trouble to extract the correct information from the task description.
 
% Buggy-Rule: Use the mean of TO instead of the mean of D
% The depends means: This bug is limited to the paired t-test and co-occurs
% with s_t0.
buggy(tratio, stage(1), X, Y, 
        [step(buggy, t0, [d, t0]), depends(s_t0), depends(paired)]) :-
    X = d,
    Y = instead(t0, t0, d).

feedback(t0, [D, T0], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " with the", 
	   " T0 average ", \mmlm(Col, color(t0, T0)), " instead of the average", 
	   " change score ", \mmlm(Col, [color(t0, D), "."]), " Please insert the average",
	   " change score ", \mmlm(Col, color(t0, D)), " into the ",
	   \mmlm(Col, hyph(t, "ratio."))
	].

hint(t0, [_D, T0], Col, F)
 => F = [ "Do not insert the T0 average ", \mmlm(Col, color(t0, T0)), " ",
          "into the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change ",
          "scores instead." 
	].

% Buggy-Rule: Use SD of T0 instead of SD of D
buggy(tratio, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_t0, [s_d, s_t0]), depends(paired)],
    X = s_d,
    Y = instead(s_t0, s_t0, s_d).

feedback(s_t0, [S, S_T0], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), 
	   " with the standard deviation for T0 ", \mmlm(Col, color(s_t0, S_T0)),
	   " instead of the standard deviation of the change score ", 
	   \mmlm(Col, [color(s_t0, S), "."]), " Please insert the standard deviation of the", 
	   " change score ", \mmlm(Col, color(s_t0, S)), " into the ", 
	   \mmlm(Col, hyph(t, "ratio."))
	].

hint(s_t0, [_S, S_T0], Col, F)
 => F = [ "Do not insert the standard deviation for ",
          "T0 ", \mmlm(Col, color(s_t0, S_T0)), " into ",
          "the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change scores ",
          "instead." 
	].

% Buggy-Rule: Use mean EOT instead of mean D
buggy(tratio, stage(1), X, Y, [step(buggy, eot, [d, eot]), 
        depends(s_eot), depends(paired)]) :-
    X = d,
    Y = instead(eot, eot, d).

feedback(eot, [D, EOT], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " with the",
           " EOT average ", \mmlm(Col, color(eot, EOT)), " instead of the ", 
           "average change score ", \mmlm(Col, [color(eot, D), "."]), " Please insert",
           " the average change score ",\mmlm(Col, color(eot, D)), " into the ",
	   \mmlm(Col, hyph(t, "ratio."))
	].

hint(eot, [_D, EOT], Col, F)
 => F = [ "Do not insert the EOT average ", \mmlm(Col, color(eot, EOT)), " ",
          "into the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change ",
          "scores instead." 
	].

% Buggy-Rule: Use SD of EOT instead of SD of D
buggy(tratio, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_eot, [s_d, s_eot]), depends(paired)],
    X = s_d,
    Y = instead(s_eot, s_eot, s_d).

feedback(s_eot, [S, S_EOT], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " with the",
	   " standard deviation for EOT ", \mmlm(Col, color(s_eot, S_EOT)), 
	   " instead of the standard deviation of the change score ", 
	   \mmlm(Col, [color(s_eot, S), "."]), " Please insert the standard deviation of the",
	   " change score ", \mmlm(Col, color(s_eot, S)), " into the ", 
	   \mmlm(Col, hyph(t, "ratio."))
	].

hint(s_eot, [_S, S_EOT], Col, F)
 => F = [ "Do not insert the standard deviation for ",
          "EOT ", \mmlm(Col, color(s_eot, S_EOT)), " into ",
          "the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change scores ",
          "instead." 
	      ].

% Buggy-Rule: Use of n instead of sqrt(n)
buggy(tratio, stage(2), X, Y, [step(buggy, sqrt1, [n])]) :-
    X = sqrt(n),
    Y = omit_right(sqrt1, n^(1/2)).

feedback(sqrt1, [N], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " without", 
	  " square root around ", \mmlm(Col, [color(sqrt1, N), "."]), " Please do not",
	  " forget the square root around ", \mmlm(Col, [color(sqrt1, N), "."])
        ].

hint(sqrt1, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \mmlm(Col, [color(sqrt1, N), "."])
        ].

/* % Buggy-Rule: Use of N instead of sqrt(N)
buggy(tratio, stage(2), X, Y, [step(buggy, sqrt2, [N])]) :-
    X = sqrt(N),
    dif(N, n),
    Y = omit_right(sqrt2, N^(1/2)).

feedback(sqrt2, [N], Col, F)
 => F = [ "The result matches the ", \mmlm(Col, hyph(t, "ratio")), " without", 
	   " square root around ", \mmlm(Col, [color(sqrt2, N), "."]), " Please do not",
	   " forget the square root around ", \mmlm(Col, [color(sqrt2, N), "."])
        ].

hint(sqrt2, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \mmlm(Col, [color(sqrt2, N), "."])
        ]. */
/* 
%
% Expert-Rules for the p-value task
%
% Gathering the important data needed to solve the t-test for paired samples
% (both correct and incorrect solutions) from the task description.
intermediate(pvalue, item).

% First step: Extract the correct information for a paired t-test from the task
% description
intermediate(pvalue, paired).
intermediate(pvalue, twotailed).
expert(pvalue, stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, Mu, _Alpha),
    Y = { '<-'(t, paired(D, Mu, S_D, N)) ;
          '<-'(p, twotailed(t, N-1))
        }.

% feedback(paired, [], Col, F)
%  => F = [ "Correctly recognised the problem as ",
%           "a ", \mmlm(Col, hyph(t, "test")), " for paired samples."
%         ].

% hint(paired, [], Col, F)
%  => F = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for paired ",
%           "samples."
%         ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
intermediate(pvalue, tratio).
expert(pvalue, stage(2), X, Y, [step(expert, tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(D - Mu, S_D / sqrt(N)).

% feedback(tratio, [_D, _Mu, _S_D, _N], Col, F)
%  => F = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")), " for ",
%           "paired samples."
%         ].

% hint(tratio, [D, Mu, S_D, N], Col, F)
%  => F = [ "The ", \mmlm(Col, hyph(t, "ratio")), " ",
%           "is ", \mmlm(Col, [dfrac(D - Mu, S_D / sqrt(N)), "."])
%         ].

% Third step: Determine the two-tailed p-value
expert(pvalue, stage(2), X, Y, [step(expert, pvalue, [])]) :-
    X = twotailed(T, DF),
    Y = pval(2*pt(-abs(T), DF)).

feedback(pvalue, [], Col, F)
 => F = [ "Correctly determined the two-tailed ", \mmlm(Col, hyph(p, "value.")) ].

hint(pvalue, [], Col, F)
 => F = [ "The two-tailed ", \mmlm(Col, hyph(p, "value")), " must be determined." ].


%
%% Buggy-Rules for the p-value task
%



%
%% Expert Rules for the confidence interval task
%
% t-test and confidence intervall for paired samples 
intermediate(cipaired, item).

% First step: Extract the correct information for a paired t-test and 
% the associated confidence interval from the task description
intermediate(cipaired, paired).
expert(cipaired, stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, Mu, Alpha),
    Y = paired(D, Mu, S_D, N, Alpha).

feedback(paired, [], Col, F)
 => F = [ "Correctly recognised the problem as ",
          "a ", \mmlm(Col, hyph(t, "test")), " for paired samples and that ",
          "a confidence interval for the mean value has to be calculated." 
        ].

hint(paired, [], Col, H)
 => H = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for paired ",
          "samples. Calculate the confidence interval for the mean difference." 
        ].

% Second step: Apply the formula for the confidence interval for a mean value.
intermediate(cipaired, quant).
expert(cipaired, stage(2), X, Y, [step(expert, ci_lower, [D, S_D, N, Alpha])]) :-
    X = paired(D, Mu, S_D, N, Alpha),
    Y = hdrs(pm(D, dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)))).

feedback(ci_lower, [_D, _S_D, _N, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for a mean value." 
        ].

hint(ci_lower, [D, S_D, N, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval is ",
	  \mmlm(Col, [pm(D, qt(1 - Alpha/2, N-1) * S_D / sqrt(N)), "."])
        ].

% Third step: Choose the correct quantile of the t-distribution
expert(cipaired, stage(2), X, Y, [step(expert, tquant, [N, Alpha])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = qt(1 - Alpha/2, N-1).

feedback(tquant, [_N, Alpha], Col, F)
 => F = [ "Correctly used the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution."))
        ].

hint(tquant, [_N, Alpha], Col, H)
 => H = [ "Make sure to use the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution."))
        ].


%
%% Buggy-Rules for the confidence interval task
%
% Buggy-Rule: Use t-statistic instead of t-quantile
buggy(cipaired, stage(2), X, Y, [step(buggy, tstat, [D, S_D, N, Mu, Alpha])]) :-
    X = quant(D, Mu, S_D, N, Alpha),
    P = denote(t, dfrac(D - Mu, S_D / sqrt(N)), ["the observed", space, t, "-statistic."]),
    Y = P. 

feedback(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the observed ",
          \mmlm(Col, hyph(t, "statistic.")), " Please use the quantile ",
           "of the ", \mmlm(Col, hyph(t, "distribution")), " instead."
        ].

hint(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, H)
 => H = [ "Do not insert the observed ", \mmlm(Col, hyph(t, "statistic ")),
          "into the formula for the confidence interval. Use the quantile of ", 
	  "the ", \mmlm(Col, hyph(t, "distribution")), " instead."
        ].

% Buggy-Rule: Use z-quantile instead of t-quantile. 
% This rule may be dropped because we might not be able to distinguish the results.
buggy(cipaired, stage(2), X, Y, [step(buggy, qnorm, [N, Alpha])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = instead(qt, qnorm(1 - Alpha/2) , qt(1 - Alpha/2, N - 1)).

feedback(qnorm, [N, Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the standard ",
          "Normal distribution. ",
          "Please insert the quantile of the ", \mmlm(Col, hyph(t, "distribution")),
          \mmlm(Col, color(qnorm, qt(1 - Alpha/2, N - 1))), " into ",
          "the formula for the confidence interval."
        ].

hint(qnorm, [_N, _Alpha], Col, H)
 => H = [ "Do not insert the quantile of the ", \mmlm(Col, hyph(z, "distribution ")), 
          "into the formula for the confidence interval. Use the quantile of the ", 			
	  \mmlm(Col, hyph(t, "distribution")), "instead."
        ].

% Buggy-Rule: Calculating the confidence intervall with SPSS
% and forgetting to add Mu to the results of the bounds in the end.
buggy(cipaired, stage(2), X, Y, [step(buggy, spss, [Mu]), excludes(qnorm), excludes(tstat), excludes(sqrt1)]) :-
    X = paired(D, Mu, S_D, N, Alpha),
    Y = hdrs(pm(add_right(spss, D - Mu), dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)))).

feedback(spss, [Mu], Col, F)
 => F = [ "The result matches the upper and lower bound calculated by SPSS. ",
	  "Please do not forget to add ", \mmlm(Col, Mu), " to the upper and lower ",
	  "bound of the confidence interval, if you calculate it with SPSS."
        ].

hint(spss, [Mu], Col, H)
 => H = [ "If you calculate the confindence intervall with SPSS, keep in mind",
	  " that SPSS subtracts ", \mmlm(Col, Mu), " from the two bounds of",
          " the CI (which must be undone)."
        ].

% Buggy-Rule: Use of N instead of sqrt(N)				
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt1, [N])]) :-			
    X = dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)),
    Y = dot(quant(D, Mu, S_D, N, Alpha), S_D / omit_right(sqrt1, (2*N)^(1/2))).

feedback(sqrt1, [N], Col, F)
 => F = [ "The result matches the confidence interval without square root around ", 
          \mmlm(Col, [color(sqrt1, N), "."]), " Please do not forget the square root",
          " around ", \mmlm(Col, [color(sqrt1, N), "."])
        ].

hint(sqrt1, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \mmlm(Col, [color(sqrt1, N), "."])
        ].

% Buggy-Rule: Use of N instead of sqrt(N) in the t-ratio
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt2, [N])]) :-
    X = dfrac(D - Mu, S_D / sqrt(N)),
    Y = dfrac(D - Mu, S_D / omit_right(sqrt2, (3*N)^(1/2))).

feedback(sqrt2, [N], Col, FB)
 => FB = [ "The result matches the confidence interval based on the observed ",
	   \mmlm(Col, hyph(t, "statistic.")), " without square root around ",
	   \mmlm(Col, [color(sqrt2, N), "."]), " Please do not forget the square root around ",
           \mmlm(Col, [color(sqrt2, N), "."])
         ].

hint(sqrt2, [N], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, [color(sqrt2, N), "."])
         ]. 

 */


