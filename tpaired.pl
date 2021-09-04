:- module(tpaired, [start/2, intermediate/2, expert/5, buggy/5, render//3]).

:- use_module(library(http/html_write)).
:- use_module(r).

:- multifile start/2, intermediate/2, expert/5, buggy/5, render//3.

init(tpaired) :-
    r_init,
    d <- 4.5,
    mu <- 3.0,
    s_d <- 8.2,
    n <- 33,
    t0 <- 23.2,
    s_t0 <- 11.7,
    eot <- 17.7,
    s_eot <- 13.2,
    tails <- "two-tailed",
    alpha <- 0.05.

render(tpaired, item(_T0, _S_T0, _EOT, _S_EOT, _D, _S_D, N, Mu), Form) -->
    { 
      option(resp(R), Form, '#.##')
    },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Phase II clinical study"),
            p(class('card-text'),
            [ "Consider a clinical study on rumination-focused Cognitive ",
              "Behavioral Therapy (rfCBT) with ",
              math(mrow([mi(N), mo(=), mn(\r(n))])), " patients. The primary ",
              "outcome is the score on the Hamilton Rating Scale for ", 
              "Depression (HDRS, range from best = 0 to worst = 42). The ",
              "significance level is set to ",
              math(mrow([mi(&(alpha)), mo(=), mn(5), mtext('%')])), " two-tailed."])
          ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "Does rfCBT lead to a relevant reduction (i.e., more than ",
                math(mrow([mi(&(Mu)), mo(=), mn(\r(mu))])),
                " units) in mean HDRS scores between ",
                "baseline (T0) and End of Treatment (EOT)?"
              ]),
            form([class(form), method('POST'), action('#tpaired-tratio')],
              [ p(class('card-text'), "Question"),
                div(class("input-group mb-3"),
                  [ div(class("input-group-prepend"), 
                      span(class("input-group-text"), "Response")),
                    input([class("form-control"), type(text), name(resp), value(R)]),
                      div(class("input-group-append"),
                        button([class('btn btn-primary'), type(submit)], "Submit"))
              ])])
          ]))
      ]).

% Prolog warns if the rules of a predicate are not adjacent. This would not
% help us here, so the definitions for intermediate, expert and buggy are
% declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% t-test for paired samples
intermediate(_, item).
start(tpaired, item(t0, s_t0, eot, s_eot, d, s_d, n, mu)) :-
    init(tpaired).

% First step: Extract the correct information for a paired t-test from the task
% description
intermediate(tpaired, paired).
expert(tpaired, stage(2), X, Y, [name(paired)]) :-
    X = item(_, _, _, _, D, S_D, N, Mu),
    Y = paired(D, Mu, S_D, N).

% Second step: Apply the formula for the t-ratio. dfrac/2 is a fraction in
% "display" mode (a bit larger font than normal)
expert(tpaired, stage(2), X, Y, [name(tratio)]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(D - Mu, S_D / sqrt(N)).

% Misconception: Run the paired t-test against zero, that is, just test for a
% decrease in symptoms. This is a frequent misconception, the problem is known
% as "regression to the mean": Scores at T0 tend to be systematically too low
% (patients present themselves at the hospital when they feel particularly 
% ill). At EOT, the measurement is not biased by self-selection. Therefore, we
% tend to see an improvement even in the absence of any therapeutical effect.
% This misconception is even built into SPSS, because the paired samples t-test
% in SPSS only allows for mu = 0. 
buggy(tpaired, stage(2), X, Y, [bug(mu)]) :-
    X = paired(D, Mu, S_D, N),
    Y = dfrac(omit_right(D - Mu), S_D / sqrt(N)).

% Misconception: Run the t-test for independent samples despite the correlated
% measurements.
intermediate(tpaired, indep).
buggy(tpaired, stage(2), X, Y, [bug(indep)]) :-
    X = item(T0, S_T0, EOT, S_EOT, _, _, N, _),
    Y = indep(T0, S_T0, N, EOT, S_EOT, N).

% This step is used to determine the test statistic for the t-test for
% independent samples. The step itself is correct, although it is only needed
% if a wrong decision has been made before [bug(indep)].
expert(tpaired, stage(2), X, Y, [name(tratio)]) :-
    X = indep(T0, S_T0, N, EOT, S_EOT, N),
    P = var_pool(S_T0^2, N, S_EOT^2, N),
    Y = dfrac(T0 - EOT, sqrt(P * (1/N + 1/N))).

% The following mistake cannot occur in the paired t-test, but is again only
% possible if the student has already made the wrong decision to calculate the
% t-test for independent samples. We need it anyway, to be able to diagnose the
% numeric result.
% 
% Forgot school math: 1/N1 + 1/N2 is not 1/(N1 + N2). For mysterious reasons,
% everyone falls into this trap at least once, including me and the student
% assistants. I have coined it "bug(school)", since it is an example in which
% the person has forgotten school math.
buggy(tpaired, stage(2), X, Y, [bug(school)]) :-
    dif(N1, N2),
    X = 1/N1 + 1/N2,
    Y = frac(1, N1 + N2).

% Same for N1 = N2
buggy(tpaired, stage(2), X, Y, [bug(school)]) :-
    X = 1/N + 1/N,
    Y = frac(1, 2*N).

% Forget parentheses in numerator and denominator of X / Y, with X = A - B and
% Y = C / D. That is, calculate A - (B / C) / D instead of (A - B) / (C / D).
% 
% This is the first buggy rule that ever came to my attention, therefore the
% name, bug1.
buggy(tpaired, stage(2), X, Y, [bug(bug1)]) :-
    X = dfrac(D - Mu, S / SQRT_N),
    Y = D - dfrac(Mu, S) / SQRT_N.

% One challenging aspect of word problems ("Textaufgaben") is that students
% have trouble to extract the correct information from the task description.
% Here, the student uses the mean T0 instead of mean D. 
% 
% The depends means: This bug is limited to the paired t-test and co-occurs
% with s_t0 (these flags are ignored, but will be activated in later versions).
buggy(tpaired, stage(1), X, Y, [bug(d_t0), depends(s_t0), depends(paired)]) :-
    X = d,
    Y = instead(t0, d).

% Use SD of T0 instead of SD of D
buggy(tpaired, stage(1), X, Y, [bug(s_t0), depends(d_t0), depends(paired)]) :-
    X = s_d,
    Y = instead(s_t0, s_d).

% Use mean EOT instead of mean D
buggy(tpaired, stage(1), X, Y, [bug(d_eot), depends(s_eot), depends(paired)]) :-
    X = d,
    Y = instead(eot, d).

% Use SD of EOT instead of SD of D
buggy(tpaired, stage(1), X, Y, [bug(s_eot), depends(d_eot), depends(paired)]) :-
    X = s_d,
    Y = instead(s_eot, s_d).

