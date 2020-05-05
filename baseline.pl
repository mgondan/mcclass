% Adjustment for baseline covariates
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

:- discontiguous intermediate/1, expert/5, buggy/4.

mathml:math_hook(Flags, m_T0, Flags, overline("T0")).
mathml:math_hook(Flags, m_EOT, Flags, overline("EOT")).
mathml:math_hook(Flags, s_T0, Flags, sub(s, "T0")).
mathml:math_hook(Flags, s_EOT, Flags, sub(s, "EOT")).

r:pl2r_hook(add(_, P), R) :-
    maplist(r:pl2r, P, R).

r:pl2r_hook(omit(_, _), 'NULL').

subset([], [], []).
subset([X | L], [X | S], D) :-
    subset(L, S, D).
subset(L, [H | S], [H | D]) :-
    subset(L, S, D).

:- multifile item/2.
item(baseline, Item) :-
    Item = baseline_fratio(data, "EOT", ["T0"], ["Sex"], ["Fidel", "FU"], [], [], "Therapy").

:- multifile intermediate/1.
intermediate(baseline_fratio/8).

:- multifile item//2.
item(baseline, Response) -->
    { r_init(baseline),
      N <- 'N',
      Lid_T0  <- sprintf("%.1f (%.1f)", m_T0["Lidcombe"], s_T0["Lidcombe"]),
      TAU_T0  <- sprintf("%.1f (%.1f)", m_T0["TAU"], s_T0["TAU"]),
      Lid_EOT <- sprintf("%.1f (%.1f)", m_EOT["Lidcombe"], s_EOT["Lidcombe"]),
      TAU_EOT <- sprintf("%.1f (%.1f)", m_EOT["TAU"], s_EOT["TAU"]),
      Lid_FU  <- sprintf("%.1f (%.1f)", m_FU["Lidcombe"], s_FU["Lidcombe"]),
      TAU_FU  <- sprintf("%.1f (%.1f)", m_FU["TAU"], s_FU["TAU"]),
      Tails   <- tails,
      http_log("Tails ~k~n", [Tails]),
      Alpha   <- alpha,
      maplist(mathml, ["Syllables %, Mean (SD)", "Lidcombe", "TAU"], H),
      maplist(mathml, ["Baseline", Lid_T0, TAU_T0], T0),
      maplist(mathml, ["EOT", Lid_EOT, TAU_EOT], EOT),
      maplist(mathml, ["FU", Lid_FU, TAU_FU], FU)
    },
    html(
      [ div(class(card),
        div(class('card-body'),
	  [ h1(class('card-title'), "Treatment of early stuttering"),
            p(class('card-text'),
              [ "Jones et al. (2005) investigated the efficacy of the ",
                "so-called Lidcombe therapy for the treatment of stuttering ",
		"in early childhood. The study is a randomized trial ",
		"on ", \mml('N' = N), " children, comparing Lidcombe ",
		"with “treatment as usual” (TAU). The significance level ",
		"is ", \mml(alpha = '100%'(Alpha)), " ", \mml(Tails), ". Please ",
		"analyze the data and draw the correct conclusions." 
              ]),
	    \table(H, [T0, EOT, FU]),
	    p(class('card-text'),
	        "The ficticious results can be downloaded below."),
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
		    "randomization (secondary endpoint)"])
              ]),
            \download(baseline)
          ])),
        div(class(card),
            div(class('card-body'),
              [ h4(class('card-title'), [a(id(question), []), "Question"]),
                p(class('card-text'),
                  [ "Does the Lidcombe therapy lead to a reduction in stuttered ",
                    "syllables compared to TAU? Please determine ",
                    "the ", span(class('text-nowrap'), [\mml('F'), "-ratio."])
		  ]),
                \form(Response)
              ]))
      ]).

% Step 1: Recognize as an ANCOVA problem
:- multifile expert/5.
expert(baseline_fratio, From >> To, _Flags, Feed, Hint) :-
    From = baseline_fratio(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = fratio(ancova_f(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy)),
    Feed = [ "Correctly identified the problem as a group comparison with ",
             "covariate adjustment." ],
    Hint = "This is a group comparison with covariate adjustment.".

intermediate(ancova_f/8).

% Step 2: Use the correct covariate(s)
expert(covariates, From >> To, Flags, Feed, Hint) :-
    From = ancova_f(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = ancova_ff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Feed = [ "The correct covariate(s) ", \mml(Flags, Cov), " were included ",
             "in the statistical model." ],
    Hint = [ "The covariate(s) ", \mml(Flags, Cov), " should be included in the ",
             "statistical model." ].

intermediate(ancova_ff/8).

% Omit one or more covariates
:- multifile buggy/4.
buggy(covariates, From >> To, Flags, Feed) :-
    From = ancova_f(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    subset(Subset, Cov, [R | Removed]),
    To   = ancova_ff(Data, Prim, Subset, Strata, Other, Int, 
             [omit(covariates, [R | Removed]) | Exclude], Therapy),
    Feed = [ "Please include the covariate(s) ", \mml(Flags, [R | Removed]), " in the ",
             "statistical model." ].

% Step 3: Use the correct stratification variable(s)
expert(strata, From >> To, Flags, Feed, Hint) :-
    From = ancova_ff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = ancova_fff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Feed = [ "The stratification variable(s) ", \mml(Flags, Strata), " were ",
             "included the statistical model." ],
    Hint = [ "The stratification variable(s) ", \mml(Flags, Strata), " should be ",
             "included in the statistical model." ].

intermediate(ancova_fff/8).

% Omit one or more stratification variables
buggy(strata, From >> To, Flags, Feed) :-
    From = ancova_ff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    subset(Subset, Strata, [R | Removed]),
    To   = ancova_fff(Data, Prim, Cov, Subset, Other, Int, 
           [omit(strata, [R | Removed]) | Exclude], Therapy),
    Feed = [ "Please include the stratification ",
             "variable(s) ", \mml(Flags, [R | Removed]), " in the statistical model." ].

% Step 4: Ignore distractors
expert(other, From >> To, Flags, Feed, Hint) :-
    From = ancova_fff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = ancova_ffff(Data, Prim, Cov, Strata, [], Int, Exclude, Therapy),
    Feed = [ "Correctly excluded ", \mml(Flags, Other), " from the analysis." ],
    Hint = [ "The variable(s) ", \mml(Flags, Other), " are not used."].

intermediate(ancova_ffff/8).

% Add one or more wrong predictors
buggy(other, From >> To, Flags, Feed) :-
    From = ancova_fff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    subset([S | Subset], Other),
    To   = ancova_ffff(Data, Prim, Cov, Strata, [add(other, [S | Subset])],
             Int, Exclude, Therapy),
    Feed = [ "The variable(s) ", \mml(Flags, [S | Subset]), " should not be used in ",
             "the analysis." ].

% Step 5: No treatment-by-covariate interactions
expert(interactions, From >> To, _Flags, Feed, Hint) :-
    From = ancova_ffff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = ancova_fffff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Feed = [ "Correctly excluded any treatment-by-covariate interactions from ",
             "the analysis." ],
    Hint = [ "The statistical model should not include any ",
             "treatment-by-covariate interactions."].

intermediate(ancova_fffff/8).

atomics_to_string_sep(Sep, List, String) :-
    atomics_to_string(List, Sep, String).

% Allow for treatment-by-covariate interactions
buggy(interactions, From >> To, Flags, Feed) :-
    From = ancova_ffff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    % T0, Sex
    append(Strata, Cov, Covariates),
    % [[T0], [Sex], [T0, Sex]]
    findall([H | T], subset([H | T], Covariates), Subsets),
    reverse(Subsets, Rev),
    % [[T0], [T0, Sex]]
    subset([S | Subset], Rev),
    % [[Therapy, T0], [Therapy, T0, Sex]]
    maplist(append([Therapy]), [S | Subset], Interactions),
    % [Therapy:T0, Therapy:T0:Sex]
    maplist(atomics_to_string_sep(:), Interactions, Colon),
    To   = ancova_fffff(Data, Prim, Cov, Strata, Other,
             [add(interactions, Colon) | Int], Exclude, Therapy),
    Feed = [ "The statistical model should not include treatment-by-covariate ",
             "interactions ",
             span(class('text-nowrap'), [\mml(Flags, Colon), "."])
           ]. % add hint with all interactions for traps

% Step 6: Apply linear regression
expert(main, From >> To, Flags, Feed, Hint) :-
    From = ancova_fffff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    To   = ancova_ffffff(Data, Prim, Cov, Strata, Other, Int, Exclude, Therapy),
    Feed = ["The main effect for ", \mml(Flags, Therapy), " has been reported."],
    Hint = [ "The main effect is ", \mml(Flags, Therapy), "."].


:- multifile r_init/1.
r_init(baseline) :-
    r_init,
    {|r||
        baseline_data = function(seed)
        {
            set.seed(seed)
            N = sample(100:150, size=1)
            pFem = 0.3
            Sex = factor(rbinom(N, size=1, prob=pFem), levels=c(0, 1), labels=c("M", "F"))
            AgeMo = round(runif(N, min=30, max=80))
            Ill = rnorm(N, mean=6 - as.numeric(Sex) + 0.05*AgeMo, sd=2)
            T0 = round(rnorm(N, mean=Ill, sd=2), 1)
            d = data.frame(ID=1:N, Sex, AgeMo, T0)

            # Inclusion criteria
            Ill = Ill[d$T0 > 2 & d$T0 < 10]
            d = d[d$T0 > 2 & d$T0 < 10, ]
            N = nrow(d)

            # Randomization
            d$Therapy = factor(rbinom(N, size=1, prob=0.5), levels=c(0, 1), labels=c("Lidcombe", "TAU")) 

	    # Treatment fidelity
	    d$Fidel = pmin(100, pmax(20, round(rnorm(N, mean=70, sd=15))))

            # EOT depends on therapy and fidelity
            d$EOT = pmax(0, pmin(10, round(digits=1, rnorm(N,
                mean=Ill-5.1-d$Fidel*0.03+1.2*as.numeric(d$Therapy), sd=2))))
            d$FU = pmax(0, pmin(10, round(digits=1, rnorm(N,
                mean=Ill-6.3-d$Fidel*0.02+1.5*as.numeric(d$Therapy), sd=2))))

            d$Therapy = as.character(d$Therapy)
            return(d)
        }

	baseline_fratio = function(d, Prim, Cov, Strata, Other, Int, Ex, Main)
	{
            Predictors = paste(c(Cov, Strata, Main), collapse="+")
            formula = sprintf("%s ~ %s", Prim, Predictors)
            m = lm(formula, data=d)
            anova(m)[Main, "F value"]
        }

        ancova_f <- ancova_ff <- ancova_fff <- ancova_ffff <- ancova_fffff <-
            function(d, Prim, Cov, Strata, Other, Int, Ex, Main)
        {
            ancova_ffffff(d, Prim, Cov, Strata, Other, Int, Ex, Main)
        }

        ancova_ffffff = function(d, Prim, Cov, Strata, Other, Int, Ex, Main)
	{
            Predictors = paste(c(Cov, Strata, Other, Int, Main), collapse="+")
            f = sprintf("%s ~ %s", as.character(Prim), Predictors)
            m = lm(f, data=d)
            F = anova(m)[Main, "F value"]
            attr(F, "df1") = anova(m)[Main, "Df"]
            attr(F, "df2") = anova(m)["Residuals", "Df"]
            F
        }

	anova_f <- function(model, main)
	{
	    anova(model)[main, "F value"]
	}
	
        data  <- baseline_data(seed=4711)
        'N'   <- nrow(data)
        m_T0  <- by(data$'T0', data$'Therapy', mean)
        s_T0  <- by(data$'T0', data$'Therapy', sd)
        m_EOT <- by(data$'EOT', data$'Therapy', mean)
        s_EOT <- by(data$'EOT', data$'Therapy', sd)
        m_FU  <- by(data$'FU', data$'Therapy', mean)
        s_FU  <- by(data$'FU', data$'Therapy', sd)
        alpha <- 0.05
        tails <- "two-tailed"
    |},
    csvfile(baseline, data).

