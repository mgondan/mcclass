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
interval:r_hook(qnorm).
interval:r_hook(qt).

render(item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, _Mu, _Alpha), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Efficency of self-confidence traning"),
            p(class('card-text'),
            [ "You organize a self-confidence training and want to know whether it improves the self-confidence of the participants. For this purpose, you measure the self-confidence of", \mmlm(N = r(N)),  "participants before and after the training. The training is considered effective if the self-confidence has increased by more than 5 units after the training. Higher values mean higher self-confidence."]),
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
        \htmlform(["Determine the confidence interval for the change in participants' self-confidence. The alpha level is", \mmlm(alpha = perc(0.05))
                ], '#cipaired', R)
    ]).

% t-test for paired samples
intermediate(item).
start(item(t0, s_t0, eot, s_eot, d, s_d, n, mu, alpha)).

% First step: Extract the correct information for a paired t-test from the task
% description
intermediate(paired).
expert(stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, _Mu, Alpha),
    Y = { '<-'(lo, paired(D, S_D, N, Alpha)) }.

feedback(paired, [], Col, FB) =>
    FB = [ "Correctly recognised the problem as ",
           "a ", \mmlm(Col, hyph(t, "test")), " for paired samples." ].

hint(paired, [], Col, Hint) =>
    Hint = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for paired ",
           "samples." ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal) % todo: correct comment
expert(stage(2), X, Y, [step(expert, ci_lower, [D, S_D, N, Alpha])]) :-
    X = paired(D, S_D, N, Alpha),
    Y = tstat(D + qnorm(Alpha/2) * S_D / sqrt(N)). % to do: one decimal place

% Aufgabe
% 0) Aufgabentext anpassen
% 1a) 1.96 -> qnorm(0.975)
% 1b) Dazu muss auch eine r_hook für qnorm definiert werden, damit das Programm weiß, dass qnorm in R berechnet werden muss. Hint: in anderen Blättern nachschauen.
% 1c) Dann in mathml.pl eine schöne Darstellung für qnorm(P) definieren, etwa so: z_P, hier also z_0.975 (Tiefstellung geht mit sub(z, P)).
% 2) "minus" qnorm(0.975) -> "plus" qnorm(0.025) ändern und Euch klarmachen, dass das das gleiche ist.
% 3) qnorm(0.025) -> qnorm(Alpha/2) und Alpha zu den Aufgabenparametern hinzufügen. -> zu dem "item"
% 4a) qnorm(Alpha/2) -> qt(Alpha/2, N-1)
% 4b) r_hook für qt
% 4c) mathml.pl eine schöne Darstellung für qt(P, DF), z.B. T_P(DF), fn(sub('T', P), DF)

feedback(ci_lower, [_D, _S_D, _N, _Alpha], Col, FB) =>
    FB = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")), " for ",
           "paired samples." ].

hint(ci_lower, [D, S_D, N, Alpha], Col, Hint)
 => Hint = [ "The lower bound of the confidence interval ",
         "is ", \mmlm(Col, D - qnorm(Alpha/2) * S_D / sqrt(N)) ].
