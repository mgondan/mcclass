:- module(tpaired, [sol/3, hints/3, hints/4, wrong/3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).
:- use_module(steps).

:- dynamic sol/3, hints/3, hints/4, wrong/3.

:- use_module(navbar).
navbar:page(tpaired, ["paired ", i(t), "-test"]).

task(tratio).
task(pvalue).
task(cipaired).

label(tratio, [math(mi(t)), "-ratio"]).
label(pvalue, [math(mi(p)), "-value"]).
label(cipaired, "Confidence interval").

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
math_hook(paired(_D, _Mu, _S_D, _N), ["a paired", hyph(t, "test")]).
math_hook(alpha, greek("alpha")).
math_hook(var_pool(V1, N1, V2, N2), X) :-
    X = dfrac((N1 - 1)*V1 + (N2 - 1)*V2, N1 + N2 - 2).

% R definitions
r_hook(t).
r_hook(p).
r_hook(d).
r_hook(mu).
r_hook(s_d).
r_hook(n).
r_hook(t0).
r_hook(s_t0).
r_hook(eot).
r_hook(s_eot).
r_hook(lo).

r_hook(var_pool/4). 
mono((var_pool)/4, [+, /, +, /]).

% Task description
render(Flags)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, _Mu, _Alpha)) },
    html(
      div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Phase II clinical study"),
          p(class('card-text'),
            [ "Consider a clinical study on rumination-focused Cognitive ",
              "Behavioral Therapy (rfCBT) with ",
              \mmlm(Flags, N = r(N)), " patients. The primary ",
              "outcome is the score on the Hamilton Rating Scale for ", 
              "Depression (HDRS, range from best = 0 to worst = 42). ",
              "The significance level is set to ",
              \mmlm(Flags, alpha = percent(0.05)), " two-tailed."]),
          div(class(container),
            div(class("row justify-content-md-center"),
              div(class("col-6"),
                \htmltable(
                  [ em("Table 1. "), "Observed HDRS scores at T0, EOT, ",
                    "and ", \mmlm('D' = "T0" - "EOT") ],
                  [ "Average", "SD" ],
                  [ "HDRS", "T0", "EOT", \mmlm(Flags, 'D') ],
                  [ [ \mmlm([digits(1) | Flags], r(t0x)),
                      \mmlm([digits(1) | Flags], r(eotx)),
                      \mmlm([digits(1) | Flags], r(dx)) ],
                    [ \mmlm([digits(1) | Flags], r(sx_t0)),
                      \mmlm([digits(1) | Flags], r(sx_eot)),
                      \mmlm([digits(1) | Flags], r(sx_d)) ]
                  ])))),
          \download(tpaired)
        ]))).

% Question for the t-ratio
task(Flags, tratio)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, Mu, _Alpha)),
      session_data(resp(tpaired, tratio, Resp), resp(tpaired, tratio, '#.##'))
    },
    html(\htmlform([ "Does rfCBT lead to a relevant reduction (i.e., more ",
        "than ", \mmlm([digits(1) | Flags], Mu = r(Mu)), " units) in mean HDRS ",
        "scores between baseline (T0) and End of Treatment (EOT)? ",
	"Please report ",
	"the ", \nowrap([\mmlm(Flags, t), "-ratio."]) ], 
	tratio, Resp)).

% Question for the p-value
task(Flags, pvalue)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, Mu, _Alpha)),
      session_data(resp(tpaired, pvalue, Resp), resp(tpaired, pvalue, '.###'))
    },
    html(\htmlform([ "Does rfCBT lead to a relevant reduction (i.e., more ",
        "than ", \mmlm([digits(1) | Flags], Mu = r(Mu)), " units) in mean HDRS ",
        "scores between baseline (T0) and End of Treatment (EOT)? ",
        "Please report ",
        "the two-tailed ", \nowrap([\mmlm(Flags, p), "-value."]) ], 
	pvalue, Resp)).

% Question for the confidence interval
task(_Flags, cipaired)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, _Mu, _Alpha)),
      session_data(resp(tpaired, cipaired, Resp), resp(tpaired, cipaired, '#.# to #.#'))
    },
    html(\htmlform([ "Determine the confidence interval for the average ",
        "change in the patients’ HDRS scores." ], cipaired, Resp)).

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
 => F = [ "Correctly recognised the problem as ",
          "a ", \nowrap([\mmlm(Col, t), "-test"]), " for ",
	  "paired samples." 
        ].

hint(paired, [], Col, F)
 => F = [ "This is a ", \nowrap([\mmlm(Col, t), "-test"]), 
          " for paired samples." 
        ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
expert(tratio, stage(2), X, Y, [step(expert, tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(dfrac(D - Mu, S_D / sqrt(N))).

feedback(tratio, [_D, _Mu, _S_D, _N], Col, F)
 => F = [ "Correctly identified ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
	  "for paired samples." 
        ].

hint(tratio, [D, Mu, S_D, N], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "is ", \mmlm(Col, [dfrac(D - Mu, S_D / sqrt(N)), "."])
        ].

% Another correct result
expert(tratio, stage(2), X, Y, [step(expert, abs_tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = tstat(abs(dfrac(D - Mu, S_D / sqrt(N)))).

feedback(abs_tratio, [_D, _Mu, _S_D, _N], Col, F)
 => F = [ "Correctly identified ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
	  "for paired samples." 
        ].

hint(abs_tratio, [D, Mu, S_D, N], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "is ", \mmlm(Col, [abs(dfrac(D - Mu, S_D / sqrt(N))), "."])
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
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "with the null hypothesis ", \mmlm(Col, color(mu, Mu)), " omitted. ",
          "Please do not forget ", \mmlm(Col, color(mu, Mu)), " in the ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."])
        ].

hint(mu, [Mu], Col, F)
 => F = [ "Do not omit the null hypothesis ", \mmlm(Col, color(mu, Mu)), " ",
          "in the ", \nowrap([\mmlm(Col, t), "-ratio."]) 
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
 => F = [ "The problem was mistakenly identified as ",
          "a ", \nowrap([\mmlm(Col, t), "-test"]), " for ",
	  "independent samples." 
        ].

hint(indep, [], Col, F)
 => F = [ "Do not calculate ",
          "a ", \nowrap([\mmlm(Col, t), "-test"]), " for ",
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
 => F = [ "Correctly identified ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " for ",
          "independent samples." 
        ].

hint(tratio_indep, [T0, S_T0, N, EOT, S_EOT], Col, F)
 => P = denote(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    F = [ "The ", \nowrap([\mmlm(Col, t), "-ratio"]), " for ",
          "independent samples ",
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
 => F = [ "The result matches the expression for ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
	  "for independent samples with ", 
	  \mmlm(Col, frac(1, color(school, color("black", A) + color("black", B)))), 
	  " under the square root. Please keep in mind that ", 
	  span(class('text-\nowrap'), 
	    [\mmlm(Col, color(school, color("black", frac(1, A)) + color("black", frac(1, B)))
              =\= frac(1, color(school, color("black", A) + color("black", B)))), "."])
        ].

hint(school1, [N1, N2], Col, F)
 => F = [ "Please do not forget school math, ", 
          span(class('text-\nowrap'), 
            [\mmlm(Col, frac(1, color(school1, N1)) + frac(1, color(school1, N2))
              =\= frac(1, color(school1, N1+N2))), "."]) 
        ].

% Buggy-Rule: Forgot school math (Same for N1 = N2)
buggy(tratio, stage(2), X, Y, [step(buggy, school2, [N])]) :-
    X = 1/N + 1/N,
    Y = frac(1, color(school2, 2*N)).

feedback(school2, [N], Col, F)
 => F = [ "The result matches the expression for ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
	  "for independent samples ",
          "with ", \mmlm(Col, frac(1, color(school2, 2*N))), " under the ",
	  "square root. Please keep in mind that ",
          span(class('text-\nowrap'), 
            [\mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N))
              =\= frac(1, color(school2, 2*N))), "."]) 
        ].

hint(school2, [N], Col, F)
 => F = [ "Please do not forget school math, ",
          span(class('text-\nowrap'),
            [\mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N))
              =\= frac(1, color(school2, 2*N))), "."])
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
 => F = [ "The result matches the fraction without parentheses around the ", 
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
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "with the T0 average ", \mmlm(Col, color(t0, T0)), " instead of ",
          "the average change score ", 
          \nowrap([\mmlm(Col, color(t0, D)), "."]), " ",
          "Please insert the average change ",
          "score ", \mmlm(Col, color(t0, D)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."])
	].

hint(t0, [_D, T0], Col, F)
 => F = [ "Do not insert the T0 average ", \mmlm(Col, color(t0, T0)), " ",
          "into the ", \nowrap([\mmlm(Col, t), "-ratio."]),
          "Use the change scores instead." 
	].

% Buggy-Rule: Use SD of T0 instead of SD of D
buggy(tratio, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_t0, [s_d, s_t0]), depends(paired)],
    X = s_d,
    Y = instead(s_t0, s_t0, s_d).

feedback(s_t0, [S, S_T0], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "with the standard deviation for ",
          "T0 ", \mmlm(Col, color(s_t0, S_T0)), " instead of the standard ",
          "deviation of the change score ", 
          \nowrap([\mmlm(Col, color(s_t0, S)), "."]), " ",
          "Please insert the standard deviation of the change ",
          "score ", \mmlm(Col, color(s_t0, S)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."])
	].

hint(s_t0, [_S, S_T0], Col, F)
 => F = [ "Do not insert the standard deviation for ",
          "T0 ", \mmlm(Col, color(s_t0, S_T0)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."]), " ",
          "Use the change scores instead." 
	].

% Buggy-Rule: Use mean EOT instead of mean D
buggy(tratio, stage(1), X, Y, [step(buggy, eot, [d, eot]), 
        depends(s_eot), depends(paired)]) :-
    X = d,
    Y = instead(eot, eot, d).

feedback(eot, [D, EOT], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "with the EOT average ", \mmlm(Col, color(eot, EOT)), " ",
	  "instead of the average change score ",
          \nowrap([\mmlm(Col, color(eot, D)), "."]), " ",
          "Please insert the average change ",
          "score ", \mmlm(Col, color(eot, D)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."])
	].

hint(eot, [_D, EOT], Col, F)
 => F = [ "Do not insert the EOT average ", \mmlm(Col, color(eot, EOT)), " ",
          "into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."]), " ",
	  "Use the change scores instead." 
	].

% Buggy-Rule: Use SD of EOT instead of SD of D
buggy(tratio, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_eot, [s_d, s_eot]), depends(paired)],
    X = s_d,
    Y = instead(s_eot, s_eot, s_d).

feedback(s_eot, [S, S_EOT], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "with the standard deviation for ",
          "EOT ", \mmlm(Col, color(s_eot, S_EOT)), " instead of the standard ",
	  "deviation of the change score ", 
	  \nowrap([\mmlm(Col, color(s_eot, S)), "."]), " ",
          "Please insert the standard deviation of the change ",
	  "score ", \mmlm(Col, color(s_eot, S)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."])
	].

hint(s_eot, [_S, S_EOT], Col, F)
 => F = [ "Do not insert the standard deviation for ",
          "EOT ", \mmlm(Col, color(s_eot, S_EOT)), " into ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio."]), " ",
          "Use the change scores instead." 
        ].

% Buggy-Rule: Use of n instead of sqrt(n)
buggy(tratio, stage(2), X, Y, [step(buggy, sqrt1, [n])]) :-
    X = sqrt(n),
    Y = omit_right(sqrt1, n^(1/2)).

feedback(sqrt1, [N], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "without the square root around ", 
          \nowrap([\mmlm(Col, color(sqrt1, N)), "."]), " ",
          "Please do not forget the square root around ",
          \nowrap([\mmlm(Col, color(sqrt1, N)), "."])
        ].

hint(sqrt1, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \nowrap([\mmlm(Col, color(sqrt1, N)), "."])
        ].

% Buggy-Rule: Forget square root around pooled variance
buggy(tratio, stage(2), X, Y, [step(buggy, sqrt2, [S_T0, S_EOT, N])]) :-
    P = denote(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    X = sqrt(P * (1/N + 1/N)),
    Y = omit_right(sqrt2, (P * (1/N + 1/N))^(1/2)).

feedback(sqrt2, [_S_T0, _S_EOT, N], Col, F)
 => F = [ "The result matches ",
          "the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "without the square root in the denominator. Please do not forget ",
	  "to take the square root of the error variance ",
          \nowrap([\mmlm(Col, color(sqrt2, sqrt(s2p * (1/N + 1/N)))), "."])
        ].

hint(sqrt2, [_S_T0, _S_EOT, N], Col, F)
 => F = [ "Do not forget the square root around the error variance",
          \nowrap([\mmlm(Col, color(sqrt2, sqrt(s2p * (1/N + 1/N)))), "."])
        ].

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
          '<-'(p, twotailed(t, N-1)) ;
	  pval(p)
        }.

feedback(paired, [], Col, F)
 => F = [ "Correctly recognised the problem as ",
          "a ", \nowrap([\mmlm(Col, t, "test")]), " for paired samples."
        ].

hint(paired, [], Col, F)
 => F = [ "This is a ", \nowrap([\mmlm(Col, t), "-test"]), " for paired ",
          "samples."
        ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
intermediate(pvalue, tratio).
expert(pvalue, stage(2), X, Y, [step(expert, tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(D - Mu, S_D / sqrt(N)).

feedback(tratio, [_D, _Mu, _S_D, _N], Col, F)
 => F = [ "Correctly identified the ", 
          \nowrap([\mmlm(Col, t), "-ratio"]), " for ",
          "paired samples."
        ].

hint(tratio, [D, Mu, S_D, N], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "is ", \nowrap([\mmlm(Col, dfrac(D - Mu, S_D / sqrt(N))), "."])
        ].

% Third step: Determine the two-tailed p-value
expert(pvalue, stage(3), X, Y, [step(expert, pvalue, [])]) :-
    X = twotailed(T, DF),
    Y = pt(dist('T', T, "two.sided"), DF, tail("two.sided")).

feedback(pvalue, [], Col, F)
 => F = [ "Correctly determined the two-tailed ", 
           \nowrap([\mmlm(Col, p), "-value."])
        ].

hint(pvalue, [], Col, F)
 => F = [ "The two-tailed ", 
          \nowrap([\mmlm(Col, p), "-value"]),
          " must be determined." 
        ].

%
% Buggy-Rules for the p-value task
%
% Lower tail
buggy(pvalue, stage(3), X, Y, [step(buggy, lower, [])]) :-
    X = twotailed(T, DF),
    Y = pt(instead(lower, dist('T', T, "lower"), dist('T', T, "two.sided")),
          DF, instead(lower, tail("lower"), tail("two.sided"))).

feedback(lower, [], Col, F)
 => F = [ "The result matches the lower ",
          "one-tailed ", \nowrap([\mmlm(Col, p), "-value."])
        ].

hint(lower, [], Col, F)
 => F = [ "Do not report the lower one-tailed ",
          \nowrap([\mmlm(Col, p), "-value."])
        ].

% Upper tail
buggy(pvalue, stage(3), X, Y, [step(buggy, upper, [])]) :-
    X = twotailed(T, DF),
    Y = pt(instead(upper, dist('T', T, "upper"), dist('T', T, "two.sided")),
          DF, instead(upper, tail("upper"), tail("two.sided"))).

feedback(upper, [], Col, F)
 => F = [ "The result matches the upper ",
          "one-tailed ", \nowrap([\mmlm(Col, p), "-value."])
        ].

hint(upper, [], Col, F)
 => F = [ "Do not report the upper one-tailed ",
          \nowrap([\mmlm(Col, p), "-value."])
        ].

% Density instead of distribution
buggy(pvalue, stage(3), X, Y, [step(buggy, density, [])]) :-
    X = twotailed(T, DF),
    Y = pt(instead(density, dist('T', T, "density"), dist('T', T, "two.sided")),
          DF, instead(density, tail("density"), tail("two.sided"))).

feedback(density, [], Col, F)
 => F = [ "The result matches the density of ",
          "the ", \nowrap([\mmlm(Col, t), "-distribution."])
        ].

hint(density, [], Col, F)
 => F = [ "Do not report the density of the ",
          \nowrap([\mmlm(Col, t), "-distribution."])
        ].

%
% Confidence interval for a difference of paired samples 
%
intermediate(cipaired, item).

% First step: Extract the correct information for a paired t-test and 
% the associated confidence interval from the task description
intermediate(cipaired, paired).
expert(cipaired, stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, Mu, Alpha),
    Y = paired(D, Mu, S_D, N, Alpha).

feedback(paired, [], Col, F)
 => F = [ "Correctly recognised the problem as ",
          "a ", \nowrap([\mmlm(Col, t), "-test"]), " for paired samples and that ",
          "a confidence interval for the mean value has to be calculated." 
        ].

hint(paired, [], Col, H)
 => H = [ "This is a ", \nowrap([\mmlm(Col, t), "-test"]), " for paired ",
          "samples. Calculate the confidence interval for the mean difference." 
        ].

% Second step: Apply the formula for the confidence interval for a mean value.
intermediate(cipaired, quant).
expert(cipaired, stage(2), X, Y, [step(expert, ci_lower, [D, S_D, N, Alpha])]) :-
    X = paired(D, Mu, S_D, N, Alpha),
    Y = { hdrs(pm(D, dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)))) }.

feedback(ci_lower, [_D, _S_D, _N, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for a mean value." 
        ].

hint(ci_lower, [D, S_D, N, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval is ",
	  span(class('text-\nowrap'), 
	    [\mmlm(Col, pm(D, qt(1 - Alpha/2, N-1) * S_D / sqrt(N))), "."])
        ].

% Third step: Choose the correct quantile of the t-distribution
expert(cipaired, stage(2), X, Y, [step(expert, tquant, [N, Alpha])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = qt(1 - Alpha/2, N - 1).

feedback(tquant, [_N, Alpha], Col, F)
 => F = [ "Correctly used the ", 
          \nowrap([\mmlm(Col, 1 - Alpha/2), "-quantile"]),
          " of the ", 
          \nowrap([\mmlm(Col, t), "-distribution."])
        ].

hint(tquant, [_N, Alpha], Col, H)
 => H = [ "Make sure to use the ", 
          \nowrap([\mmlm(Col, 1 - Alpha/2), "-quantile"]),
          " of the ", 
          \nowrap([\mmlm(Col, t), "-distribution."])
        ].

%
%% Buggy-Rules for the confidence interval task
%
% Buggy-Rule: Use t-statistic instead of t-quantile
buggy(cipaired, stage(2), X, Y, [step(buggy, tstat, [D, S_D, N, Mu, Alpha])]) :-
    X = quant(D, Mu, S_D, N, Alpha),
    T = denote(t, abs(dfrac(D - Mu, S_D / sqrt(N))), mrow(["the observed ", math(mi(t)), "-statistic"])),
    Y = instead(tstat, T, qt(1 - Alpha/2, N - 1)).

feedback(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the observed ",
          \nowrap([\mmlm(Col, t), "-statistic."]), " ",
          "Please use the quantile of the ", 
          \nowrap([\mmlm(Col, t), "-distribution"]), " instead."
        ].

hint(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, H)
 => H = [ "Do not insert the observed ", 
          \nowrap([\mmlm(Col, t), "-statistic"]), " ",
          "into the formula for the confidence interval. Use the quantile of ", 
          "the ", \nowrap([\mmlm(Col, t), "-distribution"]),
          " instead."
        ].

% Buggy-Rule: Use z-quantile instead of t-quantile. 
% This rule may be dropped because we might not be able to distinguish the results.
buggy(cipaired, stage(2), X, Y, [step(buggy, qnorm, [])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = instead(qnorm, qnorm(1 - Alpha/2), qt(1 - Alpha/2, N - 1)).

feedback(qnorm, [], Col, F)
 => F = [ "The result matches the confidence interval based on the standard ",
          "Normal distribution. Please insert the quantile of ",
	  "the ", \nowrap([\mmlm(Col, t), "-distribution"]), " ",
          "into the formula for the confidence interval."
        ].

hint(qnorm, [], Col, H)
 => H = [ "Determine the confidence interval based on the quantiles of ",
          "the ", \nowrap([\mmlm(Col, t), "-distribution."])
        ].

% Buggy-Rule: Calculating the confidence interval with SPSS
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
 => H = [ "If you calculate the confindence interval with SPSS, keep in mind",
	  " that SPSS subtracts ", \mmlm(Col, Mu), " from the two bounds of",
          " the CI (which must be undone)."
        ].

% Buggy-Rule: Use of N instead of sqrt(N)
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt3, [N])]) :-
    X = dfrac(Q, S_D / sqrt(N)),
    Y = dfrac(Q, S_D / omit_right(sqrt3, N^(1/2))).

feedback(sqrt3, [N], Col, F)
 => F = [ "The result matches the ", \nowrap([\mmlm(Col, t), "-ratio"]), " ",
          "without the square root ",
          "around ", \nowrap([\mmlm(Col, color(sqrt3, N)), "."])
        ].

hint(sqrt3, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \nowrap([\mmlm(Col, color(sqrt3, N)), "."])
        ].

% Buggy-Rule: Use of N instead of sqrt(N)
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt4, [N])]) :-
    X = dot(Q, S_D / sqrt(N)),
    Y = dot(Q, S_D / omit_right(sqrt4, N^(1/2))).

feedback(sqrt4, [N], Col, F)
 => F = [ "The result matches the confidence interval without square root around ", 
          \nowrap([\mmlm(Col, color(sqrt4, N)), "."]), " Please ",
	  "do not forget the square root",
          " around ", \nowrap([\mmlm(Col, color(sqrt4, N)), "."])
        ].

hint(sqrt4, [N], Col, H)
 => H = [ "Do not forget the square root around ",
          \nowrap([\mmlm(Col, color(sqrt4, N)), "."])
        ].

