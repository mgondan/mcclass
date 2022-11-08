:- module(cipaired, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(cipaired, ["Confidence interval for paired samples"]).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

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
interval:r_hook(var_pool(_N1, _V1, _N2, _V2)).
interval:r_hook(lo).
interval:r_hook(d).
interval:r_hook(mu).
interval:r_hook(s_d).
interval:r_hook(n).
interval:r_hook(t0).
interval:r_hook(s_t0).
interval:r_hook(eot).
interval:r_hook(s_eot).
interval:r_hook(qt(_P, _DF)).

render(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, _Mu, _Alpha), Form) -->
    { option(resp(R), Form, '#.##') },
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
            \download(cipaired)
          ])),
        \htmlform(["Determine the confidence interval for the change in participants' self-confidence. The alpha level is ", \mmlm(alpha = perc(0.05))
                ], '#cipaired', R)
    ]).
 %Sollte noch eine Vorgabe für die Eingabe in der Aufgabenstellung angegeben werden?

% t-test and confidence intervall for paired samples 
intermediate(item).
start(item(t0, s_t0, eot, s_eot, d, s_d, n, mu, alpha)).

% First step: Extract the correct information for a paired t-test and 
% associated confidence interval from the task description
intermediate(paired).
expert(stage(2), X, Y, [step(expert, paired, [])]) :-
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
intermediate(quant).
expert(stage(2), X, Y, [step(expert, ci_lower, [D, S_D, N, Alpha])]) :-
    X = paired(D, Mu, S_D, N, Alpha),
    Y = hdrs(pm(D, quant(D, Mu, S_D, N, Alpha) * frac(S_D, sqrt(N)))).

feedback(ci_lower, [_D, _S_D, _N, _Alpha], _Col, F)					
 => F = [ "Correctly identified the formula for the upper and lower bound of ",
           "the confidence interval for a mean value." 
        ].

hint(ci_lower, [D, S_D, N, Alpha], Col, H)
 => H = [ "The formula to calculate the lower and upper bound of the ",
          "confidence interval is ",
	  \mmlm(Col, pm(D, qt(1 - Alpha/2, N-1) * frac(S_D, sqrt(N))))
        ].

% Third step: Choose the correct quantile of the t-distribution
expert(stage(2), X, Y, [step(expert, tquant, [N, Alpha])]) :-
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

% Use t-statistic instead of t-quantile
buggy(stage(2), X, Y, [step(buggy, tstat, [D, S_D, N, Mu, Alpha])]) :-
    X = quant(D, Mu, S_D, N, Alpha),
    P = abbrev(t, dfrac(D - Mu, S_D / sqrt(N)), ["the observed", space, t, "-statistic."]),		%Ist es möglich, die t-statistik hier auch kursiv zu schrebien?
    Y = P. 

feedback(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the observed ",
          \mmlm(Col, hyph(t, "statistic.")), " Please use the quantile ",
           "of the ", \mmlm(Col, hyph(t, "distribution")), " instead."
        ].

hint(tstat, [_D, _S_D, _N, _Mu, _Alpha], Col, H)
 => H = [ "Do not insert the ", \mmlm(Col, hyph(t, "statistic ")), "into the ",
          "formula for the confidence interval. Use the quantile of the ",
	   \mmlm(Col, hyph(t, "distribution")), " instead."
        ].

% Buggy-Rule: Use z-quantile instead of t-quantile. This rule may be dropped
% because we might not be able to distinguish the results.
buggy(stage(2), X, Y, [step(buggy, qt, [N, Alpha])]) :-
    X = quant(_D, _Mu, _S_D, N, Alpha),
    Y = instead(qt, qnorm(1 - Alpha/2) , qt(1 - Alpha/2, N - 1)).

feedback(qt, [N, Alpha], Col, F)
 => F = [ "The result matches the confidence interval based on the standard ",
          "Normal distribution. ",
          "Please insert the quantile of the ", \mmlm(Col, hyph(t, "distribution")),
          \mmlm(Col, color(qt, qt(1 - Alpha/2, N - 1))), " into ",
          "the formula for the confidence interval."
        ].

hint(qt, [_N, _Alpha], Col, H)
 => H = [ "Do not insert the quantile of the ", \mmlm(Col, hyph(z, "distribution ")), 
          "into the formula for the confidence interval. Use the quantile of the ", 			
	  \mmlm(Col, hyph(t, "distribution")), "instead."
        ].


% 3. Buggy rule SPSS
% 6. in tpaired-celina reinschauen, ob Ihr noch irgendwelche bugs findet/dort noch irgendwelche stehen: https://www.dropbox.com/scl/fo/4npx22e6wowo4zad3riau/h?dl=0&rlkey=51k1yq6a6u66uef5vf727qmgz

% buggy rule/spss: mu
buggy(stage(2), X, Y, [step(buggy, spss, [Mu])]) :-
    X = paired(D, Mu, S_D, N, Alpha),
    Y = hdrs(pm(invent_right(spss, D - Mu), quant(D, Mu, S_D, N, Alpha) * frac(S_D, sqrt(N)))).

feedback(spss, [Mu], Col, F)
 => F = [ "The result matches...(bitte weiter) Please do not forget to add ", \mmlm(Col, Mu), " to the upper and lower bound",
	  " of the confidence interval, if you calculate the bounds with SPSS."
        ].

hint(spss, [Mu], Col, H)
 => H = [ "If you calculate the bounds of the confindence intervall with SPSS ",
	  "keep in mind, that SPSS subtracts", \mmlm(Col, Mu), " from the two ",
          "bounds of the CI (which must be undone)."
        ].

% Buggy-Rule: Use of N instead of sqrt(N)				% Durch Buggy-Rule mit t-statistik zweimal Fehler mit Wurzel vergessen möglich,
buggy(stage(2), X, Y, [step(buggy, sqrt1, [N])]) :-			% sollte dies im Text oder farblich unterschieden werden?
    X = quant(D, Mu, S_D, N, Alpha) * frac(S_D, sqrt(N)),
    Y = quant(D, Mu, S_D, N, Alpha) * frac(S_D, omit_right(sqrt1, (2*N)^(1/2))).

feedback(sqrt1, [N], Col, FB)
 => FB = [ "Please do not forget the square root around ", 
           \mmlm(Col, color(sqrt1, N))
         ].

hint(sqrt1, [N], Col, FB)
 => FB = [ "Do not forget the square root around ",
           \mmlm(Col, color(sqrt1, N))
	     ].

% Same mistake in the t-ratio
buggy(stage(2), X, Y, [step(buggy, sqrt2, [N])]) :-
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
