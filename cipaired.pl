:- module(cipaired, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(rint).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(cipaired, ["Confidence interval for paired samples"]).

task(cipaired).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(d, overline('D')).
mathml_hook(s_d, sub(s, 'D')).
mathml_hook(n, 'N').
mathml_hook(t0, overline("T0")).
mathml_hook(s_t0, sub(s, "T0")).
mathml_hook(eot, overline("EOT")).
mathml_hook(s_eot, sub(s, "EOT")).
mathml_hook(s2p, sub(s, "pool")^2).
mathml_hook(paired(D, Mu, S_D, N), fn("paired", [D, Mu, S_D, N])).
mathml_hook(alpha, greek("alpha")).

% R definitions
rint:r_hook(var_pool(_N1, _V1, _N2, _V2)).
rint:r_hook(lo).
rint:r_hook(d).
rint:r_hook(mu).
rint:r_hook(s_d).
rint:r_hook(n).
rint:r_hook(t0).
rint:r_hook(s_t0).
rint:r_hook(eot).
rint:r_hook(s_eot).
rint:r_hook(qt(_P, _DF)).

% Task description
render
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, _Mu, _Alpha)) },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Efficiency of self-confidence training"),
            p(class('card-text'),
            [ "You organize a self-confidence training and want to know ",
              "whether it improves the self-confidence of the participants. ",
              "For this purpose, you measure the self-confidence ",
              "of ", \mmlm(N = r(N)),  "participants before and after the ",
              "training. The training is considered effective if the ",
              "self-confidence has increased by more than 5 units after the ",
              "training. Higher values mean higher self-confidence."]),
            div(class('container'),
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
            \download(cipaired)]))]).

task(cipaired, Form)
--> { start(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, _N, _Mu, _Alpha)),
      option(resp(R), Form, '#.##') 
    },
    html(\htmlform(["Determine the confidence interval for the change in participants' self-confidence. The alpha level is ", \mmlm(alpha = perc(0.05))
           ], "cipaired", R)).

% t-test and confidence intervall for paired samples 
intermediate(cipaired, item).
start(item(t0, s_t0, eot, s_eot, d, s_d, n, mu, alpha)).

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
	  \mmlm(Col, pm(D, qt(1 - Alpha/2, N-1) * S_D / sqrt(N)))
        ].

% Third step: Choose the correct quantile of the t-distribution
expert(cipaired, stage(2), X, Y, [step(expert, tquant, [N, Alpha])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = qt(1 - Alpha/2, N-1).

feedback(tquant, [_N, Alpha], Col, F)
 => F = [ "Correctly used the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution"))
        ].

hint(tquant, [_N, Alpha], Col, H)
 => H = [ "Make sure to use the ", \mmlm(Col, hyph(1 - Alpha/2, "quantile")),
          "of the ", \mmlm(Col, hyph(t, "distribution"))
        ].

% Buggy-Rule: Use t-statistic instead of t-quantile
buggy(cipaired, stage(2), X, Y, [step(buggy, tstat, [D, S_D, N, Mu, Alpha])]) :-
    X = quant(D, Mu, S_D, N, Alpha),
    P = abbrev(t, dfrac(D - Mu, S_D / sqrt(N)), ["the observed", space, t, "-statistic."]),
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
    Y = hdrs(pm(invent_right(spss, D - Mu), dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)))).

feedback(spss, [Mu], Col, F)
 => F = [ "The result matches the upper and lower bound calculated by SPSS. ",
	  "Please do not forget to add ", \mmlm(Col, Mu), " to the upper and lower ",
	  "bound of the confidence interval, if you calculate it with SPSS."
        ].

hint(spss, [Mu], Col, H)
 => H = [ "If you calculate the confindence intervall with SPSS keep in mind,",
	  " that SPSS subtracts ", \mmlm(Col, Mu), " from the two bounds of",
          " the CI (which must be undone)."
        ].

% Buggy-Rule: Use of N instead of sqrt(N)				
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt1, [N])]) :-			
    X = dot(quant(D, Mu, S_D, N, Alpha), S_D / sqrt(N)),
    Y = dot(quant(D, Mu, S_D, N, Alpha), S_D / omit_right(sqrt1, (2*N)^(1/2))).

feedback(sqrt1, [N], Col, F)
 => F = [ "The result matches the confidence interval without square root around ", 
          \mmlm(Col, color(sqrt1, N)), ". Please do not forget the square root",
          " around ", \mmlm(Col, color(sqrt1, N)), "."
        ].

hint(sqrt1, [N], Col, F)
 => F = [ "Do not forget the square root around ",
          \mmlm(Col, color(sqrt1, N))
        ].

% Buggy-Rule: Use of N instead of sqrt(N) in the t-ratio
buggy(cipaired, stage(2), X, Y, [step(buggy, sqrt2, [N])]) :-
    X = dfrac(D - Mu, S_D / sqrt(N)),
    Y = dfrac(D - Mu, S_D / omit_right(sqrt2, (3*N)^(1/2))).

feedback(sqrt2, [N], Col, FB)
 => FB = [ "Please do not forget the square root around ",
           \mmlm(Col, color(sqrt2, N))
         ].

hint(sqrt2, [N], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, color(sqrt2, N))
         ]. 
