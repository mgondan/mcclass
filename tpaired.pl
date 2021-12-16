:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile download/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

download(tpaired, File) :-
    session_tmpfile(File),
    r_task(tpaired, download(File)).

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, d, Flags, overline('D')).
mathml:hook(Flags, s_d, Flags, sub(s, 'D')).
mathml:hook(Flags, n, Flags, 'N').
mathml:hook(Flags, t0, Flags, overline("T0")).
mathml:hook(Flags, s_t0, Flags, sub(s, "T0")).
mathml:hook(Flags, eot, Flags, overline("EOT")).
mathml:hook(Flags, s_eot, Flags, sub(s, "EOT")).
mathml:hook(Flags, s2p, Flags, sub(s, "pool")^2).

% R definitions
interval:hook(pl, var_pool(N1, V1, N2, V2), r(var_pool(N1, V1, N2, V2))).

interval:hook(pl, d, r(d)).
interval:hook(pl, mu, r(mu)).
interval:hook(pl, s_d, r(s_d)).
interval:hook(pl, n, r(n)).
interval:hook(pl, t0, r(t0)).
interval:hook(pl, s_t0, r(s_t0)).
interval:hook(pl, eot, r(eot)).
interval:hook(pl, s_eot, r(s_eot)).

render(tpaired, item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, _Mu), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Phase II clinical study"),
            p(class('card-text'),
            [ "Consider a clinical study on rumination-focused Cognitive ",
              "Behavioral Therapy (rfCBT) with ",
              \mmlm(N = r(n)), " patients. The primary ",
              "outcome is the score on the Hamilton Rating Scale for ", 
              "Depression (HDRS, range from best = 0 to worst = 42). ",
              "The significance level is set to ",
              \mmlm(alpha = [5, "%"]), " two-tailed."]),
            div(class('container'),
              div(class("row justify-content-md-center"),
                div(class("col-6"),
                  \htmltable(
                    [ em("Table 1. "), "Observed HDRS scores at T0, EOT, ",
                      "and ", \mmlm('D' = "T0" - "EOT") ],
                    [ "Average", "SD" ],
                    [ "HDRS", "T0", "EOT", \mmlm('D') ],
                    [ [ \mmlm([round(1)], r(t0)),
                        \mmlm([round(1)], r(eot)),
                        \mmlm([round(1)], r(d1)) ],
                      [ \mmlm([round(1)], r(s_t0)),
                        \mmlm([round(1)], r(s_eot)),
                        \mmlm([round(1)], r(s1_d)) ]
                    ])))),
              form(method('POST'),
                  button([ class('btn btn-secondary'), name(download), value(tpaired) ], "Download data"))
          ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "Does rfCBT lead to a relevant reduction (i.e., more ",
                "than ", \mmlm([round(1)], mu = r(mu)), " units) in mean HDRS ",
                "scores between baseline (T0) and End of Treatment (EOT)? ",
                "Please report the ", \mmlm(hyph(t, "ratio."))
              ]),
            form([class(form), method('POST'), action('#tpaired-tratio')],
              [ div(class("input-group mb-3"),
                  [ div(class("input-group-prepend"), 
                      span(class("input-group-text"), "Response")),
                    input([class("form-control"), type(text), name(resp), value(R)]),
                      div(class("input-group-append"),
                        button([class('btn btn-primary'), type(submit)], "Submit"))
              ])])
          ]))
      ]).

% t-test for paired samples
intermediate(_, item).
start(tpaired, item(t0, s_t0, eot, s_eot, d, s_d, n, mu)).

% First step: Extract the correct information for a paired t-test from the task
% description
intermediate(tpaired, paired).
expert(tpaired, stage(2), X, Y, [step(expert, paired, [])]) :-
    X = item(_, _, _, _, D, S_D, N, Mu),
    Y = paired(D, Mu, S_D, N).

feedback(tpaired, paired, [], Col, FB) :-
    FB = [ "Correctly recognised the problem as ",
           "a ", \mmlm(Col, hyph(t, "test")), " for paired samples." ].

hint(tpaired, paired, [], Col, Hint) :-
    Hint = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for paired ",
           "samples." ].

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
expert(tpaired, stage(2), X, Y, [step(expert, tratio, [D, Mu, S_D, N])]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(D - Mu, S_D / sqrt(N)).

feedback(tpaired, tratio, [_D, _Mu, _S_D, _N], Col, FB) :-
    FB = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio.")) ].

hint(tpaired, tratio, [D, Mu, S_D, N], Col, Hint) :-
    Hint = [ "The ", \mmlm(Col, hyph(t, "ratio")), " ",
         "is ", \mmlm(Col, dfrac(D - Mu, S_D / sqrt(N))) ].

% Misconception: Run the paired t-test against zero, that is, just test for a
% decrease in symptoms. This is a frequent misconception, the problem is known
% as "regression to the mean": Scores at T0 tend to be systematically too low
% (patients present themselves at the hospital when they feel particularly 
% ill). At EOT, the measurement is not biased by self-selection. Therefore, we
% tend to see an improvement even in the absence of any therapeutical effect.
% This misconception is even built into SPSS, because the paired samples t-test
% in SPSS only allows for mu = 0. 
buggy(tpaired, stage(2), X, Y, [step(buggy, mu, [Mu])]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(omit_right(bug(mu), D - Mu), S_D / sqrt(N)).

feedback(tpaired, mu, [Mu], Col, FB) :-
    FB = [ "In the ", \mmlm(hyph(t, "ratio,")), " the null ",
           "hypothesis ", \mmlm(Col, color(mu, Mu)), " has been omitted." ].

hint(tpaired, mu, [Mu], Col, FB) :-
    FB = [ "Do not omit the null hypothesis ", \mmlm(Col, color(mu, Mu)), " ",
           "in the ", \mmlm(hyph(t, "ratio.")) ].

% Misconception: Run the t-test for independent samples despite the correlated
% measurements.
intermediate(tpaired, indep).
buggy(tpaired, stage(2), X, Y, [step(buggy, indep, [])]) :-
    X = item(T0, S_T0, EOT, S_EOT, _, _, N, _),
    Y = indep(T0, S_T0, N, EOT, S_EOT, N).

feedback(tpaired, indep, [], Col, FB) :-
    FB = [ "The problem was mistakenly identified as ",
           "a ", \mmlm(Col, hyph(t, "test")), " ",
           "for independent samples." ].

hint(tpaired, indep, [], Col, FB) :-
    FB = [ "Do not calculate a ", \mmlm(Col, hyph(t, "test")), " for ",
           "independent samples here." ].

% This step is used to determine the test statistic for the t-test for
% independent samples. The step itself is correct, although it is only needed
% if a wrong decision has been made before [bug(indep)].
expert(tpaired, stage(2), X, Y, [step(expert, tratio_indep, [T0, S_T0, N, EOT, S_EOT])]) :-
    X = indep(T0, S_T0, N, EOT, S_EOT, N),
    P = abbrev(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    Y = dfrac(T0 - EOT, sqrt(P * (1/N + 1/N))).

feedback(tpaired, tratio_indep, [_T0, _S_T0, _N, _EOT, _S_EOT], Col, FB) :-
    FB = [ "Correctly identified the ", \mmlm(Col, hyph(t, "ratio")), " ",
           "for independent samples." ].

hint(tpaired, tratio_indep, [T0, S_T0, N, EOT, S_EOT], Col, FB) :-
    P = abbrev(s2p, var_pool(S_T0^2, N, S_EOT^2, N), "the pooled variance"),
    FB = [ "The ", \mmlm(Col, hyph(t, "ratio")), " for independent samples ",
           "would be ", \mmlm(Col, dfrac(T0 - EOT, sqrt(P * (1/N + 1/N)))) ].

% The following mistake cannot occur in the paired t-test, but is again only
% possible if the student has already made the wrong decision to calculate the
% t-test for independent samples. We need it anyway, to be able to diagnose the
% numeric result.
% 
% Forgot school math: 1/N1 + 1/N2 is not 1/(N1 + N2). For mysterious reasons,
% everyone falls into this trap at least once, including me and the student
% assistants. I have coined it "bug(school)", since it is an example in which
% the person has forgotten school math.
buggy(tpaired, stage(2), X, Y, [step(buggy, school1, [N1, N2])]) :-
    dif(N1, N2),
    X = 1/N1 + 1/N2,
    Y = frac(1, color(school1, N1 + N2)).

feedback(tpaired, school1, [N1, N2], Col, FB) :-
    FB = [ "Please do not forget school ",
           "math, ", \mmlm(Col, frac(1, color(school1, N1)) + 
             frac(1, color(school1, N2)) =\= frac(1, color(school1, N1+N2))) ].

hint(tpaired, school1, [N1, N2], Col, FB) :-
    FB = [ "Please do not forget school ",
           "math, ", \mmlm(Col, frac(1, color(school1, N1)) +
             frac(1, color(school1, N2)) =\= frac(1, color(school1, N1+N2))) ].

% Same for N1 = N2
buggy(tpaired, stage(2), X, Y, [step(buggy, school2, [N])]) :-
    X = 1/N + 1/N,
    Y = frac(1, color(school2, 2*N)).

feedback(tpaired, school2, [N], Col, FB) :-
    FB = [ "Please do not forget school ",
           "math, ", \mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N)) =\= frac(1, color(school2, 2*N))) ].

hint(tpaired, school2, [N], Col, FB) :-
    FB = [ "Please do not forget school ",
           "math, ", \mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N)) =\= frac(1, color(school2, 2*N))) ].

% Forget parentheses in numerator and denominator of X / Y, with X = A - B and
% Y = C / D. That is, calculate A - (B / C) / D instead of (A - B) / (C / D).
% 
% This is the first buggy rule that ever came to my attention, therefore the
% name, bug1.
buggy(tpaired, stage(2), X, Y, [step(buggy, bug1, [D, Mu, S, SQRT_N])]) :-
    X = dfrac(D - Mu, S / SQRT_N),
    DD = drop_left(bug(bug1), D - Mu),
    SS = drop_right(bug(bug1), S / SQRT_N),
    Y = invent_left(bug(bug1), D - invent_right(bug(bug1), dfrac(DD, SS) / SQRT_N)).

feedback(tpaired, bug1, [D, Mu, S, SQRT_N], Col, FB) :-
    FB = [ "Please do not forget the parentheses around the numerator and ",
           "the denominator of a fraction, ", 
           \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", D - Mu))), color(bug1, paren(color("#000000", S / SQRT_N))))) 
         ].

hint(tpaired, bug1, [D, Mu, S, SQRT_N], Col, FB) :-
    FB = [ "Do not forget the parentheses around the numerator and ",
           "the denominator of a fraction, ",
           \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", D - Mu))), color(bug1, paren(color("#000000", S / SQRT_N)))))
         ].

% One challenging aspect of word problems ("Textaufgaben") is that students
% have trouble to extract the correct information from the task description.
% Here, the student uses the mean T0 instead of mean D. 
% 
% The depends means: This bug is limited to the paired t-test and co-occurs
% with s_t0.
buggy(tpaired, stage(1), X, Y, [step(buggy, t0, [d, t0]), depends(s_t0), depends(paired)]) :-
    X = d,
    Y = instead(bug(t0), t0, d).

feedback(tpaired, t0, [D, _T0], Col, FB) :-
    FB = [ "Please insert the average change ",
           "score ", \mmlm(Col, color(t0, D)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")) ].

hint(tpaired, t0, [_D, T0], Col, FB) :-
    FB = [ "Do not insert the T0 average ", \mmlm(Col, color(t0, T0)), " ",
           "into the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change ",
           "scores instead." ].

% Use SD of T0 instead of SD of D
buggy(tpaired, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_t0, [s_d, s_t0]), depends(paired)],
    X = s_d,
    Y = instead(bug(s_t0), s_t0, s_d).

feedback(tpaired, s_t0, [S, _S_T0], Col, FB) :-
    FB = [ "Please insert the standard deviation of the change ",
           "scores ", \mmlm(Col, color(s_t0, S)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")) ].

hint(tpaired, s_t0, [_S, S_T0], Col, FB) :-
    FB = [ "Do not insert the standard deviation for ",
           "T0 ", \mmlm(Col, color(s_t0, S_T0)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change scores ",
           "instead." ].

% Use mean EOT instead of mean D
buggy(tpaired, stage(1), X, Y, [step(buggy, eot, [d, eot]), depends(s_eot), depends(paired)]) :-
    X = d,
    Y = instead(bug(eot), eot, d).

feedback(tpaired, eot, [D, _EOT], Col, FB) :-
    FB = [ "Please insert the average change ",
           "score ", \mmlm(Col, color(eot, D)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")) ].

hint(tpaired, eot, [_D, EOT], Col, FB) :-
    FB = [ "Do not insert the EOT average ", \mmlm(Col, color(eot, EOT)), " ",
           "into the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change ",
           "scores instead." ].

% Use SD of EOT instead of SD of D
buggy(tpaired, stage(1), X, Y, Flags) :-
    Flags = [step(buggy, s_eot, [s_d, s_eot]), depends(paired)],
    X = s_d,
    Y = instead(bug(s_eot), s_eot, s_d).

feedback(tpaired, s_eot, [S, _S_EOT], Col, FB) :-
    FB = [ "Please insert the standard deviation of the change ",
           "scores ", \mmlm(Col, color(s_eot, S)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")) ].

hint(tpaired, s_eot, [_S, S_EOT], Col, FB) :-
    FB = [ "Do not insert the standard deviation for ",
           "EOT ", \mmlm(Col, color(s_eot, S_EOT)), " into ",
           "the ", \mmlm(Col, hyph(t, "ratio.")), " Use the change scores ",
           "instead." ].

