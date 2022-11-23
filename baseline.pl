% Adjustment for baseline covariates
:- module(baseline, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(baseline, ["baseline covariates"]).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(m_T0, overline("T0")).
mathml_hook(m_EOT, overline("EOT")).
mathml_hook(s_T0, sub(s, "T0")).
mathml_hook(s_EOT, sub(s, "EOT")).

% Hier Definitionen von R einfuegen?
interval:r_hook(ancova_f(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)).

%r:pl2r_hook(add(_, P), R) :-
%    maplist(r:pl2r, P, R).
%
%r:pl2r_hook(omit(_, _), 'NULL').

% my_subset(+List, -Subset, -Difference)
my_subset([], [], []).
my_subset([X | L], [X | S], D) :-
    my_subset(L, S, D).
my_subset(L, [H | S], [H | D]) :-
    my_subset(L, S, D).

render(item(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy), Form) -->
    { option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Training of surgical skills"),
          p(class('card-text'),
            [ "Surgeons need special motor skills, especially for ",
              "endoscopic surgery through the belly. Nickel et al. (2015) ",
              "report the results of a study with two learning methods for ",
              "motor skill training. One group underwent a virtual reality ",
              "training (VR group), the other group participated in a ",
              "mixture of online courses and classical training of motor ",
              "skill with the so-called Box-trainer (Box group). ",
              "The primary dependent variable is the result on the OSATS ",
              "test (interval scaled, normally distributed, high scores = ",
              "good performance). A few more dependent variables were ",
              "assessed, including a knowledge test (interval scaled), ",
              "operation time (dichotomized, above or below 80 min), and ",
              "efficiency ratings (ordinal scale, 1=bad ... 5=good)."
            ]),
	  p(class('card-text'),
            [ "Please check the following text from the publication ",
              "(40 +/- 10 means average 40, standard deviation 10):"
	    ]),
          div(class(card),
            div(class('card-body'),
              p(class('card-text'),
                [ "Laparoscopy-naive medical students were randomized into ",
                  "was judged higher by the VR group than by the Box group."
                ]))),
          \download(baseline)
          ])),
        \htmlform(
              [ "Is VR training superior to traditional Box training? ",
                "Please report the ", \mmlm(hyph('F', "ratio,")), " using Box ",
                "as the control intervention."
              ], "#fratio", R)
          ]).

%
%
%item(baseline, Response) -->
%    { r_init(baseline),
%      N <- 'N',
%      Lid_T0 <- sprintf("%.1f (%.1f)", m_T0["Lidcombe"],
%      s_T0["Lidcombe"]),
%      TAU_T0  <- sprintf("%.1f (%.1f)", m_T0["TAU"], s_T0["TAU"]),
%      Lid_EOT <- sprintf("%.1f (%.1f)", m_EOT["Lidcombe"],
%      s_EOT["Lidcombe"]),
%      TAU_EOT <- sprintf("%.1f (%.1f)", m_EOT["TAU"], s_EOT["TAU"]),
%      Lid_FU <- sprintf("%.1f (%.1f)", m_FU["Lidcombe"],
%      s_FU["Lidcombe"]),
%      TAU_FU  <- sprintf("%.1f (%.1f)", m_FU["TAU"], s_FU["TAU"]),
%      Tails   <- tails,
%      Alpha   <- alpha,
%      maplist(mathml, ["Syllables %, Mean (SD)", "Lidcombe", "TAU"],
%      H),
%      maplist(mathml, ["Baseline", Lid_T0, TAU_T0], T0),
%      maplist(mathml, ["EOT", Lid_EOT, TAU_EOT], EOT),
%      maplist(mathml, ["FU", Lid_FU, TAU_FU], FU)
%    },
%    html(
%      [ div(class(card), div(class('card-body'),
%        [ h1(class('card-title'), "Treatment of early stuttering"),
%        p(class('card-text'), [ "Jones et al. (2005) investigated the
%        efficacy of the ",
%"so-called Lidcombe therapy for the treatment of stuttering ",
%"in early childhood. The study is a randomized trial ",
%"on ", \mml('N' = N), " children, comparing Lidcombe ",
%"with treatment as usual(TAU). The significance level ",
%"is ", \mmlm(alpha = perc(0.05)), " two-tailed. ",
%"Please analyze the data and draw the correct conclusions." ]),
%  \table(H, [T0, EOT, FU]),
%    p(class('card-text'),
%	"The ficticious results can be downloaded below."),
%	 ul(
%     [li("ID: Patient number"),
%	li("Sex: F, M (stratification factor)"),
%	li("AgeMo: Age in months at inclusion"),
%	li("T0: Percentage of stuttered syllables at baseline"),
%	li("Therapy: Lidcombe, TAU"),
%	li(["Fidel: Treatment fidelity in percent, summarizing ",
%		            "different indicators of adherence: ",
%		            "0% (none)...100% (perfect)"]),
%       li(["EOT: Percentage of stuttered syllables 9 months", "after ",
%		            "randomization (primary endpoint)"]),
%       li(["FU: Percentage of stuttered syllables 15 months" "after ",
%		            "randomization (secondary endpoint)"])
%              ]),
%            \download(baseline)
%          ])),
%%Unsicher, wie ich die Tabelle formatieren sollte
%  \htmlform([ "Does the Lidcombe therapy lead to a",
%"reduction in stuttered syllables compared to TAU?",
%    "Please report the ", \mmlm(hyph(F, "ratio.")) ], "#Fratio", R)
%      ]).



% baseline adjusted ANCOVA
intermediate(item).
start(item("EOT", ["T0"], ["Sex"], ["Fidel", "FU"], [], [], "Therapy")).

% Step 1: Extract the correct information for a ANCOVA from the
% task description
intermediate(baseline1).
expert(stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline1(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, [], _Col, H)
 => H = [ "This is a group comparison with covariate adjustment." ].

% Step 2: Use the correct covariate(s)
intermediate(baseline2).
expert(stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
    X = baseline1(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline2(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(covariates, [Cov], Col, F)
 => F = [ "The correct covariates ", \mmlm(Col, list(+, Cov)), " were included ",
          "in the model."
        ].

hint(covariates, [Cov], Col, H)
 => H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Omit relevant covariates (ex. forget to include baseline (T0))
buggy(stage(2), X, Y, [step(buggy, covariates, [Cov, [R | Removed]])]) :-
    X = baseline1(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline2(Prim, Subset, Strata, Other, Int, Exclude, Therapy).

feedback(covariates, [_Cov, Removed], Col, F)
 => F = [ "The covariates ", \mmlm(Col, list(+, Removed)), " were forgotten ",
          "in the model."
        ].

hint(covariates, [Cov, _Removed], Col, H)
 => H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Step 3: Use the correct stratification variable(s)
intermediate(baseline3).
expert(stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
    X = baseline2(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline3(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(stratification, [Strata], Col, F)
 => F = [ "The correct strata ", \mmlm(Col, list(+, Strata)), " were included ",
          "in the model."
        ].

hint(stratification, [Strata], Col, H)
 => H = [ "The strata ", \mmlm(Col, list(+, Strata)), " should be included ",
          "in the statistical model."
        ].

% Misconception: Exclude Strata variable(s)
buggy(stage(2), X, Y, [step(buggy, misstrata, [Strata, [R | Removed]])]) :-
    X = baseline2(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Strata, [R | Removed]),
    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline3(Prim, Cov, Subset, Other, Int, Exclude, Therapy).

feedback(misstrata, [_Strata, Removed], Col, F)
 => F = [ "The relevant stratification variable(s) ", \mmlm(Col, list(+, Removed)), " were excluded ",
          "in the statistical model."].

hint(misstrata, [Strata, _Removed], Col, H)
 => H = [ "The stratification variable(s) ", \mmlm(Col, list(+, Strata)), " should be included ",
          "in the statistical model."].

% Step 4: Ignore distractors
intermediate(baseline4).
expert(stage(2), X, Y, [step(expert, distractors, [Other])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline4(Prim, Cov, Strata, [], Int, Exclude, Therapy).

feedback(distractors, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(distractors, [Other], Col, H)
 => H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

%Step 4: Potential Misconception: add distractor variables to the model
buggy(stage(2), X, Y, [step(buggy, distractors, [Other, [R | Removed]])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Other, [R | Removed]),
    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline4(Prim, Cov, Strata, Subset, Int, Exclude, Therapy).

feedback(distractors, [_Other, Removed], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+,Removed)), " were successfully ",
          "excluded in the model."
        ].

hint(distractors, [Other, _Removed], Col, H)
 => H = [ "The distractor variable(s) ", \mmlm(Col, list(+,Other)), " should be excluded ",
          "in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(baseline5).
expert(stage(2), X, Y, [step(expert, noint, [Int])]) :-
    X = baseline4(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline5(Prim, Cov, Strata, Other, [], Exclude, Therapy).

feedback(noint, [_Int], _Col, F)
 => F = [ "Correctly excluded any treatment-by-covariate interactions from ",
          "the analysis."
        ].

hint(noint, [_Int], _Col, H)
 => H = [ "Do not put any treatment-by-covariate interactions in the ",
          "statistical model."
        ].

% Allow for treatment-by-covariate-interactions
atomics_to_string_sep(Sep, List, String) :-
  atomics_to_string(List, Sep, String).

buggy(stage(2), X, Y, [step(buggy, interactions, [Int])]) :-
    X = baseline4(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    % T0, Sex
    append(Strata, Cov, Covariates),
    % [[T0], [Sex], [T0, Sex]]
    findall([H | T], my_subset([H | T], Covariates, _), Subsets),
    reverse(Subsets, Rev),
    % [[T0], [T0, Sex]]
    my_subset([S | Subset], Rev, _),
    % [[Therapy, T0], [Therapy, T0, Sex]]
    maplist(append([Therapy]), [S | Subset], Interactions),
    % [Therapy:T0, Therapy:T0:Sex]
    maplist(atomics_to_string_sep(:), Interactions, Colon),
    append(Colon, Int, New),
    Y = baseline5(Prim, Cov, Strata, Other, New, Exclude, Therapy).

feedback(interactions, [_Int], _Col, F)
 => F = [ "The statistical model should not include treatment-by-covariate ",
          "interactions"
        ].

hint(interactions, [_Int], _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ].

% Step 6: Apply linear regression
expert(stage(2), X, Y, [step(expert, ancova, [Therapy])]) :-
    X = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = ancova_f(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(ancova, [Therapy], Col, F)
 => F = [ "The main effect for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, [Therapy], Col, H)
 => H = [ "Report the main effect for ", \mmlm(Col, Therapy), "."
        ].
