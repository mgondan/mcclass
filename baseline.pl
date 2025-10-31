% Adjustment for baseline covariates
:- module(baseline, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).
:- use_module(navbar).
navbar:page(baseline, ["baseline covariates"]).

task(fratio).
task(pvalue).
task(cibase).

label(fratio, [math(mi('F')), "-ratio"]).
label(pvalue, [math(mi(p)), "-value"]).
label(cibase, "Confidence interval").

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(m_T0, overline("T0")).
math_hook(m_EOT, overline("EOT")).
math_hook(s_T0, subscript(s, "T0")).
math_hook(s_EOT, subscript(s, "EOT")).

math_hook(ancova_f(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy), M) :-
      Row1 = '##'(subscript('H', 0) := lm(~(Outcome, Cov + Strata + Other + Interaction + Exclude))), 
      Row2 = '##'(subscript('H', 1) := lm(~(Outcome, Therapy + Cov + Strata + Other + Interaction + Exclude))), 
	    Row3 = '##'(fn('F', [subscript('H', 1) - subscript('H', 0)])),
      M = table([Row1, Row2, Row3]).

math_hook(ancova_p(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy), M) :-
      Row1 = '##'(subscript('H', 0) := lm(~(Outcome, Cov + Strata + Other + Interaction + Exclude))), 
      Row2 = '##'(subscript('H', 1) := lm(~(Outcome, Therapy + Cov + Strata + Other + Interaction + Exclude))), 
	    Row3 = '##'(fn('F', [subscript('H', 1) - subscript('H', 0)])),
	    Row4 = '##'(fn('pval', ['F'])),
      M = table([Row1, Row2, Row3, Row4]).

math_hook(ancova_ci(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy), M) :-
      Row1 = '##'(subscript('H', 0) := lm(~(Outcome, Cov + Strata + Other + Interaction + Exclude))), 
      Row2 = '##'(subscript('H', 1) := lm(~(Outcome, Therapy + Cov + Strata + Other + Interaction + Exclude))), 
	    Row3 = '##'(fn('F', [subscript('H', 1) - subscript('H', 0)])),
	    Row4 = '##'(fn('ci', ['F'])),
      M = table([Row1, Row2, Row3, Row4]).

math_hook(X := Y, list(:, [X, Y])).
math_hook("age", "Age").
math_hook("sex", "Sex").

/* macro(ancova_f/7, all, [/, /, /, /, /, /, /], [pattern([string, list, list, list, list, list, string])]).
macro(ancova_p/7, all, [/, /, /, /, /, /, /], [pattern([string, list, list, list, list, list, string])]).
macro(ancova_ci/7, all, [/, /, /, /, /, /, /], [pattern([string, list, list, list, list, list, string])]). */

% Task description
render(Flags)
--> { start(item(_Outcome, _Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)) },
    html(
      div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Treatment of early stuttering"),
          p(class('card-text'),
            [ "Jones et al. (2005) investigated the efficacy of the ",
              "so-called Lidcombe therapy for the treatment of stuttering ",
              "in early childhood. The study is a randomized trial ",
              "on ", \mmlm(Flags, r('n')), " children, comparing Lidcombe ",
              "with treatment as usual (TAU). The significance level ",
              "is set at ", \mmlm(Flags, alpha = percent(0.05)), " two-tailed. "
            ]),
          div(class('container'),
            div(class("row justify-content-md-center"),
              p(class("col-6"),
                \htmltable(
                    [ em("Table 1. "), "Mean (SD) percentage of stuttered syllables" ],
                    [ "T0", "EOT" ],
                    [ "", "Lidcombe", "TAU" ],
                    [ [ \mmlm(Flags, r('Lidcombe_T0')), \mmlm(Flags, r('TAU_T0')) ],
                      [ \mmlm(Flags, r('Lidcombe_EOT')), \mmlm(Flags, r('TAU_EOT')) ]
                    ])))),
          p(class('card-text'),
             "Fictitious data can be downloaded below."),
	  ul(
            [ li("ID: patient number"),
              li("Sex: F, M (stratification factor)"),
              li("Age: age in months at inclusion"),
              li("T0: percentage of stuttered syllables at baseline"),
              li("Therapy: Lidcombe, TAU"),
              li(["Fidel: treatment fidelity in percent, summarizing ",
                "different indicators of adherence: ",
                "0\u0025 (none)...100\u0025 (perfect)"]),
              li(["EOT: percentage of stuttered syllables 9 months after ",
                "randomization (primary endpoint)"]),
              li(["FU: percentage of stuttered syllables 15 months after ",
                "randomization (secondary endpoint)"])]), 
          \download(baseline)
        ]))).

% Question for F-Ratio task
task(Flags, fratio)
--> { start(item(_Outcome, _Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
      session_data(resp(baseline, fratio, Resp), resp(baseline, fratio, '#.##'))
    },
    html(\htmlform([ "Does the Lidcombe therapy lead to a relevant reduction ",
      "in stutterd syllables compared to TAU? ",
      "Please report the ", \nowrap([\mmlm(Flags, 'F'), "-ratio."]) ], fratio, Resp)).

% Question for p-value task
task(Flags, pvalue)
--> { start(item(_Outcome, _Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
      session_data(resp(baseline, pvalue, Resp), resp(baseline, pvalue, '.###'))
    },
    html(\htmlform([ "Does the Lidcombe therapy lead to a relevant reduction ",
      "in stuttered syllables compared to TAU? ",
      "Please report the ", \nowrap([\mmlm(Flags, p), "-value."]) ], pvalue, Resp)).

% Question for CI task
task(_Flags, cibase)
--> { start(item(_Outcome, _Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
      session_data(resp(baseline, cibase, Resp), resp(baseline, cibase, '.###-.###'))
    },
    html(\htmlform([ "Does the Lidcombe therapy lead to a relevant reduction ",
      "in stutterd syllables compared to TAU? ",
      "Please report the confidence interval."], cibase, Resp)).

%
% Expert rules for the F-ratio task
%
% baseline adjusted ANCOVA
intermediate(fratio, item).
start(item("EOT", ["T0", "age"], ["sex"], ["FU"], [], [], "Therapy")).

% Step 1: Extract the correct information for an ANCOVA from the
% task description
intermediate(fratio, baseline1).
expert(fratio, stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, _Col, H)
 => H = "This is a group comparison with covariate adjustment.".

% Step 2: Use the correct covariate(s)
intermediate(fratio, baseline2).
expert(fratio, stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates, [Cov], Col, F)
 => F = [ "The correct covariates ", \mmlm(Col, list(+, Cov)), " were included ",
          "in the model."
        ].

hint(covariates, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Step 3: Use the correct stratification variable(s)
intermediate(fratio, baseline3).
expert(fratio, stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(stratification, [Strata], Col, F)
 => F = [ "The correct strata ", \mmlm(Col, list(+, Strata)), " were included ",
          "in the model."
        ].

hint(stratification, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The stratification variable(s) ", \mmlm(Col, list(+, Strata)), " ",
          "should be included in the statistical model."
        ].

% Step 4: Ignore distractors
intermediate(fratio, baseline4).
expert(fratio, stage(2), X, Y, [step(expert, ignore, [Other])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline4(Outcome, Cov, Strata, [], Interaction, Exclude, Therapy).

feedback(ignore, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include any post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(fratio, baseline5).
expert(fratio, stage(2), X, Y, [step(expert, noint, [Interaction])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline5(Outcome, Cov, Strata, Other, [], Exclude, Therapy).

feedback(noint, [_Interaction], _Col, F)
 => F = [ "Correctly excluded any treatment-by-covariate interactions from ",
          "the analysis."
        ].

hint(noint, _Col, H)
 => H = [ "Do not put any treatment-by-covariate interactions in the ",
          "statistical model."
        ].

% Step 6: Calculating the model
expert(fratio, stage(2), X, Y, [step(expert, ancova, [Therapy])]) :-
    X = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = { ancova_f(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy) }.

feedback(ancova, [Therapy], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, 'F'), "-ratio"]), " for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, Col, H)
 => start(item(_Outcome, _Cov, _Strata, _Other, _Interaction, _Exclude, Therapy)),
    H = [ "Report the ", \nowrap([\mmlm(Col, 'F'), "-ratio"]), " ",
          "for ", \nowrap([\mmlm(Col, Therapy), "."])
        ].

%
% Buggy-Rules for the F-ratio task
%
% Buggy-Rule: Omit relevant covariates (ex. forget to include baseline (T0))
buggy(fratio, stage(2), X, Y, [step(buggy, covariates, [Cov, [R | Removed]])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline2(Outcome, Subset, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates, [_Cov, Removed], Col, F)
 => F = [ "The covariates ", \mmlm(Col, list(+, Removed)), " were erroneously excluded ",
          "from the model."
        ].

hint(covariates, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Buggy-Rule: Exclude Strata variable(s)
buggy(fratio, stage(2), X, Y, [step(buggy, misstrata, [Strata, [R | Removed]])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Strata, [R | Removed]),
    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline3(Outcome, Cov, Subset, Other, Interaction, Exclude, Therapy).

feedback(misstrata, [_Strata, Removed], Col, F)
 => F = [ "The relevant stratification variable(s) ", \mmlm(Col, list(+, Removed)), " were erroneously ",
          " excluded from the statistical model."].

hint(misstrata, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not forget to include the stratification ",
          "variable(s) ", \mmlm(Col, list(+, Strata)), " in the statistical ",
          "model."
        ].

% Buggy-Rule: add distractor variables to the model
buggy(fratio, stage(2), X, Y, [step(buggy, distractors, [Other, [S | Subset]])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset([S | Subset], Other, Difference),
    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
    append(Exclude0, Distractors, Exclude),
    Y = baseline4(Outcome, Cov, Strata, Difference, Interaction, Exclude, Therapy).

feedback(distractors, [_Other, Dist], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
          "included in the model."
        ].

hint(distractors, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include other outcomes ", \mmlm(Col, list(+, Other)), " as ",
          "covariates in the statistical model."
        ].

% Allow for treatment-by-covariate-interactions
atomics_to_string_sep(Sep, List, String) :-
  atomics_to_string(List, Sep, String).

% Buggy-Rule
buggy(fratio, stage(2), X, Y, [step(buggy, interactions, [Colon])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Int0, Exclude, Therapy),
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
    findall(add(interactions, C), member(C, Colon), Invented),
    append(Invented, Int0, Interaction),
    Y = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(interactions, [Interaction], Col, F)
 => F = [ "The following treatment-by-covariate interaction(s) were erroneously included in the model: ", 
          \nowrap([\mmlm(Col, list(+, Interaction)), "."])
        ].

hint(interactions, _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ]. 

%
% Expert Rules for the p-value task
%
intermediate(pvalue, item).

% Step 1: Extract the correct information for an ANCOVA from the
% task description
intermediate(pvalue, baseline1).
expert(pvalue, stage(1), X, Y, [step(expert, baseline1, [])]) :-
    X = item(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(baseline1, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline1, _Col, H)
 => H = "This is a group comparison with covariate adjustment.".

% Step 2: Use the correct covariate(s)
intermediate(pvalue, baseline2).
expert(pvalue, stage(2), X, Y, [step(expert, covariates1, [Cov])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates1, [Cov], Col, F)
 => F = [ "The correct covariates ", \mmlm(Col, list(+, Cov)), " were included ",
          "in the model."
        ].

hint(covariates1, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Step 3: Use the correct stratification variable(s)
intermediate(pvalue, baseline3).
expert(pvalue, stage(2), X, Y, [step(expert, stratification1, [Strata])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(stratification1, [Strata], Col, F)
 => F = [ "The correct strata ", \mmlm(Col, list(+, Strata)), " were included ",
          "in the model."
        ].

hint(stratification1, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not forget to include the stratification ",
          "variable(s) ", \mmlm(Col, list(+, Strata)), " in the statistical ",
          "model."
        ].

% Step 4: Ignore distractors
intermediate(pvalue, baseline4).
expert(pvalue, stage(2), X, Y, [step(expert, ignore1, [Other])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline4(Outcome, Cov, Strata, [], Interaction, Exclude, Therapy).

feedback(ignore1, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore1, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(pvalue, baseline5).
expert(pvalue, stage(2), X, Y, [step(expert, noint1, [Interaction])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline5(Outcome, Cov, Strata, Other, [], Exclude, Therapy).

feedback(noint1, [_Interaction], _Col, F)
 => F = [ "Correctly excluded any treatment-by-covariate interactions from ",
          "the analysis."
        ].

hint(noint1, _Col, H)
 => H = [ "Do not put any treatment-by-covariate interactions in the ",
          "statistical model."
        ].

% Step 6: Calculating the model
expert(pvalue, stage(2), X, Y, [step(expert, ancova1, [])]) :-
    X = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = ancova_p(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(ancova1, [], Col, F)
 => F = [ "The ", \nowrap([\mmlm(Col, p), "-value"]), " for the ", \nowrap([\mmlm(Col, 'F'), "-ratio"]), " has been reported."
        ].

hint(ancova1, Col, H)
 => H = [ "Report the", \nowrap([\mmlm(Col, p), "-value"]), " for the", \nowrap([\mmlm(Col, 'F'), "-ratio"])
        ].

% Buggy-Rules for the for the p-value task
%
% Buggy-Rule: Omit relevant covariates (ex. forget to include baseline (T0))
buggy(pvalue, stage(2), X, Y, [step(buggy, covariates1, [Cov, [R | Removed]])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline2(Outcome, Subset, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates1, [_Cov, Removed], Col, F)
 => F = [ "The covariates ", \mmlm(Col, list(+, Removed)), " were erroneously excluded ",
          "from the model."
        ].

hint(covariates1, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Buggy-Rule: Exclude Strata variable(s)
buggy(pvalue, stage(2), X, Y, [step(buggy, misstrata1, [Strata, [R | Removed]])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Strata, [R | Removed]),
    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline3(Outcome, Cov, Subset, Other, Interaction, Exclude, Therapy).

feedback(misstrata1, [_Strata, Removed], Col, F)
 => F = [ "The relevant stratification variable(s) ", \mmlm(Col, list(+, Removed)), " were erroneously ",
          " excluded from the statistical model."].


hint(misstrata1, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The stratification variable(s) ", \mmlm(Col, list(+, Strata)), " should be included ",
          "in the statistical model."
        ].

% Buggy-Rule: add distractor variables to the model
buggy(pvalue, stage(2), X, Y, [step(buggy, distractors1, [Other, [S | Subset]])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset([S | Subset], Other, Difference),
    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
    append(Exclude0, Distractors, Exclude),
    Y = baseline4(Outcome, Cov, Strata, Difference, Interaction, Exclude, Therapy).

feedback(distractors1, [_Other, Dist], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
          "included in the model."
        ].

hint(distractors1, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include the distractor variable(s) ", \mmlm(Col, list(+, Other)),
          "in the statistical model."
        ].

% Buggy-Rule
buggy(pvalue, stage(2), X, Y, [step(buggy, interactions1, [Colon])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Int0, Exclude, Therapy),
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
    findall(add(interactions, C), member(C, Colon), Invented),
    append(Invented, Int0, Interaction),
    Y = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(interactions1, [Interaction], Col, F)
 => F = [ "The following treatment-by-covariate interaction(s) were erroneously included in the model: ", 
          \nowrap([\mmlm(Col, list(+, Interaction)), "."])
        ].

hint(interactions1, _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ]. 

%
% Expert Rules for the confidence interval task
%
% baseline adjusted ANCOVA
intermediate(cibase, item).

% Step 1: Extract the correct information for an ANCOVA from the
% task description
intermediate(cibase, baseline1).
expert(cibase, stage(1), X, Y, [step(expert, baseline2, [])]) :-
    X = item(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(baseline2, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline2, _Col, H)
 => H = "This is a group comparison with covariate adjustment.".

% Step 2: Use the correct covariate(s)
intermediate(cibase, baseline2).
expert(cibase, stage(2), X, Y, [step(expert, covariates2, [Cov])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates2, [Cov], Col, F)
 => F = [ "The correct covariates ", \mmlm(Col, list(+, Cov)), " were included ",
          "in the model."
        ].

hint(covariates2, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Step 3: Use the correct stratification variable(s)
intermediate(cibase, baseline3).
expert(cibase, stage(2), X, Y, [step(expert, stratification2, [Strata])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(stratification2, [Strata], Col, F)
 => F = [ "The correct strata ", \mmlm(Col, list(+, Strata)), " were included ",
          "in the model."
        ].

hint(stratification2, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The strata ", \mmlm(Col, list(+, Strata)), " should be included ",
          "in the statistical model."
        ].

% Step 4: Ignore distractors
intermediate(cibase, baseline4).
expert(cibase, stage(2), X, Y, [step(expert, ignore2, [Other])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline4(Outcome, Cov, Strata, [], Interaction, Exclude, Therapy).

feedback(ignore2, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore2, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(cibase, baseline5).
expert(cibase, stage(2), X, Y, [step(expert, noint2, [Interaction])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = baseline5(Outcome, Cov, Strata, Other, [], Exclude, Therapy).

feedback(noint2, [_Interaction], _Col, F)
 => F = [ "Correctly excluded any treatment-by-covariate interactions from ",
          "the analysis."
        ].

hint(noint2, _Col, H)
 => H = [ "Do not put any treatment-by-covariate interactions in the ",
          "statistical model."
        ].

% Step 6: Calculating the model
expert(cibase, stage(2), X, Y, [step(expert, ancova2, [])]) :-
    X = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy),
    Y = { ancova_ci(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy) }.

feedback(ancova2, [], _Col, F)
 => F = [ "The confidence interval for the group contrast has been reported."
        ].

hint(ancova2, _Col, H)
 => H = [ "Report the confidence intervals for the group contrast."
        ].

% Buggy-Rules for the for the confidence interval task
%
% Buggy-Rule: Omit relevant covariates (ex. forget to include baseline (T0))
buggy(cibase, stage(2), X, Y, [step(buggy, covariates2, [Cov, [R | Removed]])]) :-
    X = baseline1(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline2(Outcome, Subset, Strata, Other, Interaction, Exclude, Therapy).

feedback(covariates2, [_Cov, Removed], Col, F)
 => F = [ "The covariates ", \mmlm(Col, list(+, Removed)), " were erroneously excluded ",
          "from the model."
        ].

hint(covariates2, Col, H)
 => start(item(_Outcome, Cov, _Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The covariate(s) ", \mmlm(Col, list(+, Cov)), " should be included ",
          "in the statistical model."
        ].

% Buggy-Rule: Exclude Strata variable(s)
buggy(cibase, stage(2), X, Y, [step(buggy, misstrata2, [Strata, [R | Removed]])]) :-
    X = baseline2(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset(Subset, Strata, [R | Removed]),
    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
    append(Exclude0, Omitted, Exclude),
    Y = baseline3(Outcome, Cov, Subset, Other, Interaction, Exclude, Therapy).

feedback(misstrata2, [_Strata, Removed], Col, F)
 => F = [ "The relevant stratification variable(s) ", \mmlm(Col, list(+, Removed)), " were erroneously ",
          " excluded from the statistical model."].


hint(misstrata2, Col, H)
 => start(item(_Outcome, _Cov, Strata, _Other, _Interaction, _Exclude, _Therapy)),
    H = [ "The stratification variable(s) ", \mmlm(Col, list(+, Strata)), " should be included ",
          "in the statistical model."
        ].

% Buggy-Rule: add distractor variables to the model
buggy(cibase, stage(2), X, Y, [step(buggy, distractors2, [Other, [S | Subset]])]) :-
    X = baseline3(Outcome, Cov, Strata, Other, Interaction, Exclude0, Therapy),
    my_subset([S | Subset], Other, Difference),
    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
    append(Exclude0, Distractors, Exclude),
    Y = baseline4(Outcome, Cov, Strata, Difference, Interaction, Exclude, Therapy).

feedback(distractors2, [_Other, Dist], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
          "included in the model."
        ].

hint(distractors2, Col, H)
 => start(item(_Outcome, _Cov, _Strata, Other, _Interaction, _Exclude, _Therapy)),
    H = [ "Do not include the distractor variable(s) ", \mmlm(Col, list(+, Other)),
          "in the statistical model."
        ].

% Buggy-Rule
buggy(cibase, stage(2), X, Y, [step(buggy, interactions2, [Colon])]) :-
    X = baseline4(Outcome, Cov, Strata, Other, Int0, Exclude, Therapy),
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
    findall(add(interactions, C), member(C, Colon), Invented),
    append(Invented, Int0, Interaction),
    Y = baseline5(Outcome, Cov, Strata, Other, Interaction, Exclude, Therapy).

feedback(interactions2, [Interaction], Col, F)
 => F = [ "The following treatment-by-covariate interaction(s) were erroneously included in the model: ", 
          \nowrap([\mmlm(Col, list(+, Interaction)), "."])
        ].

hint(interactions2, _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ]. 
