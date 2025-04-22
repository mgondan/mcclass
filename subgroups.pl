% Adjustment for baseline covariates
:- module(subgroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(interval/interval).
:- use_module(mathml).
:- use_module(navbar).

navbar:page(subgroups, ["subgroups"]).

task(fratio).
task(pvalue).
task(cibase).

label(fratio, [math(mi('F')), "-ratio"]).
label(pvalue, [math(mi(p)), "-value"]).
label(cibase, "Confidence interval").

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(m_T0, overline("T0")).
math_hook(m_EOT, overline("EOT")).
math_hook(s_T0, subscript(s, "T0")).
math_hook(s_EOT, subscript(s, "EOT")).

r_hook(ancova_f/7).
r_hook(ancova_p/7).
r_hook(ancova_ci/7).

mono(ancova_f/7, [/, /, /, /, /, /, /]).
mono(ancova_p/7, [/, /, /, /, /, /, /]).
mono(ancova_ci/7, [/, /, /, /, /, /, /]).

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


render(Flags)
--> { start(item(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)) },
    html(
      div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Subgroups - Treatment of early stuttering"),
          p(class('card-text'),
            [ "Jones et al. (2005) investigated the efficacy of the ",
              "so-called Lidcombe therapy for the treatment of stuttering ",
              "in early childhood. The study is a randomized trial ",
              "on ", \mmlm(Flags, r('N')), "children, comparing Lidcombe ",
              "with treatment as usual (TAU). The significance level ",
              "is ", \mmlm(Flags, alpha = percent(0.05)), " two-tailed. ",
	      "In this Task analyze the impact of the biological Sex of the children on the Therapy-effect, the subgroup-interaction of Sex:Therapy. ",
              "Please analyze the data and draw the correct conclusions." 
            ]),
          div(class('container'),
            div(class("row justify-content-md-center"),
              p(class("col-6"),
                \htmltable(
                    [ em("Table 1. "), "Descriptive statistics" ],
                    [ "T0", "EOT" ],
                    [ "", "Lidcombe", "TAU" ],
                    [ [ \mmlm(Flags, r('Lidcombe_T0')), \mmlm(Flags, r('TAU_T0')) ],
                      [ \mmlm(Flags, r('Lidcombe_EOT')), \mmlm(Flags, r('TAU_EOT')) ]
                    ])))),
          p(class('card-text'),
             "Ficticious data can be downloaded below."),
	  ul(
            [ li("ID: Patient number"),
              li("Sex: F, M (stratification factor)"),
              li("AgeMo: Age in months at inclusion"),
              li("T0: Percentage of stuttered syllables at baseline"),
              li("Therapy: Lidcombe, TAU"),
              li(["Fidel: Treatment fidelity in percent, summarizing ",
                "different indicators of adherence: ",
                "0% (none)...100% (perfect)"]),
              li(["EOT: Percentage of stuttered syllables 9 months after ",
                "randomization (primary endpoint)"]),
              li(["FU: Percentage of stuttered syllables 15 months after ",
                "randomization (secondary endpoint)"])]), 
          \download(baseline)
        ]))).

% Question for F-Ratio
task(Flags, fratio)
--> { start(item(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)),
      session_data(resp(baseline, fratio, Resp), resp(baseline, fratio, '#.##'))
    },
	html(\htmlform([ "Does the therapy effect differ between boys and girls? ",
	"Include the interaction and analyze its impact. Please report the ", \nowrap([\mmlm(Flags, 'F'), "-ratio"]), " for the interaction." ], fratio, Resp)).

% Question for p-value
task(Flags, pvalue)
--> { start(item(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)),
      session_data(resp(baseline, pvalue, Resp), resp(baseline, pvalue, '.###'))
    },
	html(\htmlform([ "Does the therapy effect differ between boys and girls? Include the interaction and analyze its impact. ",
	"Please report the ", \nowrap([\mmlm(Flags, p), "-value"]), " for the interaction." ], pvalue, Resp)).

% Question for CI
task(_Flags, cibase)
--> { start(item(_Prim, _Cov, _Strata, _Other, _Int, _Exclude, _Therapy)),
      session_data(resp(baseline, cibase, Resp), resp(baseline, cibase, '.###-.###'))
    },
	html(\htmlform([ "Does the Lidcombe therapy lead to a relevant reduction ",
	"in stutterd syllables compared to TAU? ",
	"Please report the confidence intervall (lower & upper)"], cibase, Resp)).


% baseline adjusted ANCOVA
intermediate(fratio, item).
start(item("EOT", ["T0", "AgeMo"], ["Sex"], ["FU"], ["Sex:Therapy"], [], "Therapy")).

% Step 1: Extract the correct information for a ANCOVA from the
% task description
intermediate(fratio, baseline1).
expert(fratio, stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline1(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, [], _Col, H)
 => H = [ "This is a group comparison with covariate adjustment." ].

% Step 2: Use the correct covariate(s)
intermediate(fratio, baseline2).
expert(fratio, stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
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
buggy(fratio, stage(2), X, Y, [step(buggy, covariates, [Cov, [R | Removed]])]) :-
    X = baseline1(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
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
intermediate(fratio, baseline3).
expert(fratio, stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
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
buggy(fratio, stage(2), X, Y, [step(buggy, misstrata, [Strata, [R | Removed]])]) :-
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
intermediate(fratio, baseline4).
expert(fratio, stage(2), X, Y, [step(expert, ignore, [Other])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline4(Prim, Cov, Strata, [], Int, Exclude, Therapy).

feedback(ignore, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore, [Other], Col, H)
 => H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

% Step 4: Potential Misconception: add distractor variables to the model
buggy(fratio, stage(2), X, Y, [step(buggy, distractors, [Other, [S | Subset]])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset([S | Subset], Other, Difference),
    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
    append(Exclude0, Distractors, Exclude),
    Y = baseline4(Prim, Cov, Strata, Difference, Int, Exclude, Therapy).

feedback(distractors, [_Other, Dist], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
          "included in the model."
        ].

hint(distractors, [Other, _Dist], Col, H)
 => H = [ "Do not include the distractor variable(s) ", \mmlm(Col, list(+, Other)),
          "in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(fratio, baseline5).
expert(fratio, stage(2), X, Y, [step(expert, noint, [Int])]) :-
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

atomics_to_string_sep(Sep, List, String) :-
    atomics_to_string(List, Sep, String).

% Allow for treatment-by-covariate-interactions
atomics_to_string_sep(Sep, List, String) :-
  atomics_to_string(List, Sep, String).

buggy(fratio, stage(2), X, Y, [step(buggy, interactions, [Colon])]) :-
    X = baseline4(Prim, Cov, Strata, Other, Int0, Exclude, Therapy),
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
    append(Invented, Int0, Int),
    Y = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(interactions, [Int], Col, F)
 => F = [ "The statistical model should not include the treatment-by-covariate ",
          "interactions", \mmlm(Col, [list(+, Int), "."])
        ].

hint(interactions, [_Int], _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ].



% Step 6: Apply linear regression
expert(fratio, stage(2), X, Y, [step(expert, ancova, [Therapy])]) :-
    X = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = ancova_f(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(ancova, [Therapy], Col, F)
 => F = [ "The main effect for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, [Therapy], Col, H)
 => H = [ "Report the main effect for ", \mmlm(Col, [Therapy, "."])
        ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for the p-value task
% Copy of fratio code above


intermediate(pvalue, item).

% Step 1: Extract the correct information for a ANCOVA from the
% task description
intermediate(pvalue, baseline1).
expert(pvalue, stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline1(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, [], _Col, H)
 => H = [ "This is a group comparison with covariate adjustment." ].

% Step 2: Use the correct covariate(s)
intermediate(pvalue, baseline2).
expert(pvalue, stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
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
buggy(pvalue, stage(2), X, Y, [step(buggy, covariates, [Cov, [R | Removed]])]) :-
    X = baseline1(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
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
intermediate(pvalue, baseline3).
expert(pvalue, stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
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
buggy(pvalue, stage(2), X, Y, [step(buggy, misstrata, [Strata, [R | Removed]])]) :-
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
intermediate(pvalue, baseline4).
expert(pvalue, stage(2), X, Y, [step(expert, ignore, [Other])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline4(Prim, Cov, Strata, [], Int, Exclude, Therapy).

feedback(ignore, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore, [Other], Col, H)
 => H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

% Step 4: Potential Misconception: add distractor variables to the model
buggy(pvalue, stage(2), X, Y, [step(buggy, distractors, [Other, [S | Subset]])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset([S | Subset], Other, Difference),
    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
    append(Exclude0, Distractors, Exclude),
    Y = baseline4(Prim, Cov, Strata, Difference, Int, Exclude, Therapy).

feedback(distractors, [_Other, Dist], Col, F)
 => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
          "included in the model."
        ].

hint(distractors, [Other, _Dist], Col, H)
 => H = [ "Do not include the distractor variable(s) ", \mmlm(Col, list(+, Other)),
          "in the statistical model."
        ].

% Step 5: No treatment-by-covariate interactions
intermediate(pvalue, baseline5).
expert(pvalue, stage(2), X, Y, [step(expert, noint, [Int])]) :-
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



buggy(pvalue, stage(2), X, Y, [step(buggy, interactions, [Colon])]) :-
    X = baseline4(Prim, Cov, Strata, Other, Int0, Exclude, Therapy),
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
    append(Invented, Int0, Int),
    Y = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(interactions, [Int], Col, F)
 => F = [ "The statistical model should not include the treatment-by-covariate ",
          "interactions", \mmlm(Col, [list(+, Int), "."])
        ].

hint(interactions, [_Int], _Col, H)
 => H = [ "The statistical model should not include any ",
          "treatment-by-covariate interactions."
        ].








%step 6 / calculating the model
intermediate(pvalue, ancova). 
expert(pvalue, stage(2), X, Y, [step(expert, ancova, [Therapy])]) :-
    X = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = ancova_p(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(ancova, [Therapy], Col, F)
 => F = [ "The main peffect for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, [Therapy], Col, H)
 => H = [ "Report the main peffect for ", \mmlm(Col, [Therapy, "."])
        ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for the ci task
% Copy of fratio code above


intermediate(cibase, item).

% Step 1: Extract the correct information for a ANCOVA from the
% task description
intermediate(cibase, baseline1).
expert(cibase, stage(1), X, Y, [step(expert, baseline, [])]) :-
    X = item(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline1(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(baseline, [], _Col, F)
 => F = [ "Correctly identified the problem as a group comparison with ",
          "covariate adjustment."
        ].

hint(baseline, [], _Col, H)
 => H = [ "This is a group comparison with covariate adjustment." ].

% Step 2: Use the correct covariate(s)
intermediate(cibase, baseline2).
expert(cibase, stage(2), X, Y, [step(expert, covariates, [Cov])]) :-
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

%% Omit relevant covariates (ex. forget to include baseline (T0))
buggy(cibase, stage(2), X, Y, [step(buggy, covariates, [Cov, [R | Removed]])]) :-
    X = baseline1(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
    my_subset(Subset, Cov, [R | Removed]),
    findall(omit(covariates , O), member(O, [R | Removed]), Omitted),
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
intermediate(cibase, baseline3).
expert(cibase, stage(2), X, Y, [step(expert, stratification, [Strata])]) :-
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

%% Misconception: Exclude Strata variable(s)
%buggy(cibase, stage(2), X, Y, [step(buggy, misstrata, [Strata, [R | Removed]])]) :-
%    X = baseline2(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
%    my_subset(Subset, Strata, [R | Removed]),
%    findall(omit(misstrata, O), member(O, [R | Removed]), Omitted),
%    append(Exclude0, Omitted, Exclude),
%    Y = baseline3(Prim, Cov, Subset, Other, Int, Exclude, Therapy).
%
%feedback(misstrata, [_Strata, Removed], Col, F)
% => F = [ "The relevant stratification variable(s) ", \mmlm(Col, list(+, Removed)), " were excluded ",
%          "in the statistical model."].
%
%hint(misstrata, [Strata, _Removed], Col, H)
% => H = [ "The stratification variable(s) ", \mmlm(Col, list(+, Strata)), " should be included ",
%          "in the statistical model."].

% Step 4: Ignore distractors
intermediate(cibase, baseline4).
expert(cibase, stage(2), X, Y, [step(expert, ignore, [Other])]) :-
    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = baseline4(Prim, Cov, Strata, [], Int, Exclude, Therapy).

feedback(ignore, [Other], Col, F)
 => F = [ "The post-randomization variable(s) ", \mmlm(Col, list(+, Other)), " were ",
          "correctly excluded from the statistical model."
        ].

hint(ignore, [Other], Col, H)
 => H = [ "Do not include the post-randomization ",
          "variables ", \mmlm(Col, list(+, Other)), " in the statistical model."
        ].

%% Step 4: Potential Misconception: add distractor variables to the model
%buggy(cibase, stage(2), X, Y, [step(buggy, distractors, [Other, [S | Subset]])]) :-
%    X = baseline3(Prim, Cov, Strata, Other, Int, Exclude0, Therapy),
%    my_subset([S | Subset], Other, Difference),
%    findall(add(distractors, D), member(D, [S | Subset]), Distractors),
%    append(Exclude0, Distractors, Exclude),
%    Y = baseline4(Prim, Cov, Strata, Difference, Int, Exclude, Therapy).
%
%feedback(distractors, [_Other, Dist], Col, F)
% => F = [ "The distractor variable(s) ", \mmlm(Col, list(+, Dist)), " were erroneously ",
%          "included in the model."
%        ].
%
%hint(distractors, [Other, _Dist], Col, H)
% => H = [ "Do not include the distractor variable(s) ", \mmlm(Col, list(+, Other)),
%          "in the statistical model."
%        ].

% Step 5: No treatment-by-covariate interactions
intermediate(cibase, baseline5).
expert(cibase, stage(2), X, Y, [step(expert, noint, [Int])]) :-
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

%buggy(cibase, stage(2), X, Y, [step(buggy, interactions, [Colon])]) :-
%    X = baseline4(Prim, Cov, Strata, Other, Int0, Exclude, Therapy),
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
%    findall(add(interactions, C), member(C, Colon), Invented),
%    append(Invented, Int0, Int),
%    Y = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy).
%
%feedback(interactions, [Int], Col, F)
% => F = [ "The statistical model should not include the treatment-by-covariate ",
%          "interactions", \mmlm(Col, [list(+, Int), "."])
%        ].
%
%hint(interactions, [_Int], _Col, H)
% => H = [ "The statistical model should not include any ",
%          "treatment-by-covariate interactions."
%        ].

% Step 6 / calculating the model
expert(cibase, stage(2), X, Y, [step(expert, ancova, [Therapy])]) :-
    X = baseline5(Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Y = ancova_ci(Prim, Cov, Strata, Other, Int, Exclude, Therapy).

feedback(ancova, [Therapy], Col, F)
 => F = [ "The main peffect for ", \mmlm(Col, Therapy), " has been reported."
        ].

hint(ancova, [Therapy], Col, H)
 => H = [ "Report the main peffect for ", \mmlm(Col, [Therapy, "."])
        ].
