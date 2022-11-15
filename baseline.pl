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
interval:r_hook(ancova_f(_Data, _Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)).

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

render(item(_Data, _Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy), Form) -->
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
start(item(data, "EOT", ["T0"], ["Sex"], ["Fidel", "FU"], [], [], "Therapy")).

% Step 1: Extract the correct information for a ANCOVA from the
% task description
intermediate(baseline1).
expert(stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline1(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, [], _Col, H)
 => H = [ "This is a group comparison with covariate adjustment." ].

% Step 2: Use the correct covariate(s)
intermediate(baseline2).
expert(stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
    X = baseline1(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline2(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(covariates, [Cov], Col, F)
 => F = [ "The correct covariates ", \mmlm(Col, Cov), " were included ",
          "in the model."
        ].

hint(covariates, [Cov], Col, H)
 => H = [ "The covariate(s) ", \mmlm(Col, Cov), " should be included ",
          "in the statistical model."
        ].

% Step 3: Use the correct stratification variable(s)
intermediate(baseline3).
expert(stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
    X = baseline2(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline3(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(stratification, [Strata], Col, F)
 => F = [ "The correct strata ", \mmlm(Col, Strata), " were included ",
          "in the model."
        ].

hint(stratification, [Strata], Col, H)
 => H = [ "The strata ", \mmlm(Col, Strata), " should be included ",
          "in the statistical model."
        ].


%%2. step: Potential Misconception:
%%Omit relevant covariates (ex. forget to include baseline (T0).
% buggy(stage(2), X, Y, [step(buggy, [T0, AgeMo, Sex, Fidel, Therapy])]) :-
%    X = baseline(T0, AgeMo, Sex, Fidel, Therapy),
%    Y = anova_test(omit_covariate(T0, EOT ~ T0 + AgeMo + Sex + Fidel +
%    Therapy).
%
%feedback(T0, [T0], Col, FB) =>
%    FB = [ "In the ANCOVA the baseline covariate (T0) ",
%		"has been omitted." ].
%
%hint(T0, [TO], Col, FB) =>
%    FB = [ "Do not omit the baseline covariate (T0)", \mmlm(Col,                       color(t0,
%                                                                                       T0)),
%                                                                                       "
%                                                                                       ",
%
%           "in the ANCOVA."].

%
%% Omit one or more stratification variables
% buggy(stage(2), X, Y, [step(buggy, Data, Prim, Cov, , Other, Int, Exclude, Therapy])]) :-
%   X = baseline(Data, Prim, Cov, , Other, Int, Exclude, Therapy),
%    Y = anova_test(EOT ~ Data, Prim, Cov, [Strata], Other, Int,                                                        Exclude,
%                                                                                                                       Therapy).
%
%
%feedback(Strata, [Strata], Col, FB) =>
%    FB = [ "In the ANCOVA the stratification variable(s)",
%		"has been excluded" ].
%
%hint(Strata, [Strata], Col, FB) =>
%    FB = [ "Do not exclude the stratification variable(s)(Strata)",            \mmlm(Col,
%                                                                               color(Strata,
%                                                                               Strata)),
%                                                                               "
%                                                                               ",
%
%           "in the ANCOVA."].

% Step 4: Ignore distractors
intermediate(baseline4).
expert(stage(2), X, Y, [step(expert, distractors, [Other])]) :-
    X = baseline3(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline4(Data, Prim, Cov, Strata, [], Int, Exclude, Therapy).

feedback(distractors, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, Other), " were ",
          "correctly excluded from the statistical model."
        ].

hint(distractors, [Other], Col, H)
 => H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, Other), " in the statistical model."
        ].

%% Add one or more wrong predictors
% buggy(stage(2), X, Y, [step(buggy, Data, Prim, Cov, Other, Int, Exclude, Therapy]) :-
%    X = baseline(Data, Prim, Cov, , Other, Int, Exclude, Therapy),
%    Y = anova_test(EOT ~ Data, Prim, Cov, Strata, Other, Int,                                          Exclude,
%                                                                                                       Therapy).
%
%
%feedback([Other], Other, Col, FB) =>
%    FB = [ "The distractor variable(s) should not be used in",
%		the analysis." ].
%
%hint([Other], Other,  Col, FB) =>
%    FB = [ "The distractor variable(s) \mml(Flags, Other),"
%		"are not used."].
%

% Step 5: No treatment-by-covariate interactions
intermediate(baseline5).
expert(stage(2), X, Y, [step(expert, interactions, [Int])]) :-
    X = baseline4(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline5(Data, Prim, Cov, Strata, Other, [], Exclude, Therapy).

feedback(interactions, [_Int], _Col, F)
 => F = [ "Correctly excluded any treatment-by-covariate interactions from ",
          "the analysis."
        ].

hint(interactions, [_Int], _Col, H)
 => H = [ "Do not put any treatment-by-covariate interactions in the ",
          "statistical model."
        ].

%atomics_to_string_sep(Sep, List, String) :-
%    atomics_to_string(List, Sep, String).
%
%% Allow for treatment-by-covariate interactions
%buggy(baseline: interactions, From >> To, Flags, Feed, Trap) :-
%    From = ancova_ffff(Data, Prim, Cov, Strata, Other, Int, Exclude,
%    Therapy),
%    % T0, Sex
%    append(Strata, Cov, Covariates),
%    % [[T0], [Sex], [T0, Sex]]
%    findall([H | T], my_subset([H | T], Covariates, _), Subsets),
%    reverse(Subsets, Rev),
%    % [[T0], [T0, Sex]]
%    my_subset([S | Subset], Rev, _),
%    % [[Therapy, T0], [Therapy, T0, Sex]]
%    maplist(append([Therapy]), [S | Subset], Interactions),
%    % [Therapy:T0, Therapy:T0:Sex]
%    maplist(atomics_to_string_sep(:), Interactions, Colon),
%    To   = ancova_fffff(Data, Prim, Cov, Strata, Other,
%             [add(interactions, Colon) | Int], Exclude, Therapy),
%    Feed = [ "The statistical model should not include
%    treatment-by-covariate ",
%             "interactions ", \nowrap([\mml(Flags, Colon), "."])
%           ],
%    Trap = [ "The statistical model should not include any ",
%	             "treatment-by-covariate interactions."].

% Step 6: Apply linear regression
expert(stage(2), X, Y, [step(expert, ancova, [Int])]) :-
    X = baseline5(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = ancova_f(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(ancova, [Therapy], Col, F)
 => F = [ "The main effect for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, [Therapy], Col, H)
 => H = [ "Report the main effect for ", \mmlm(Col, Therapy), "."
        ].



