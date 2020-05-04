% t-test for paired samples

:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

:- discontiguous intermediate/1, expert/5, buggy/4.

mathml:math_hook(Flags, s_D, Flags, sub(s, 'D')).
mathml:math_hook(Flags, s_T0, Flags, sub(s, "T0")).
mathml:math_hook(Flags, s_EOT, Flags, sub(s, "EOT")).

%    
% Paired t-test, t-ratio
%
:- multifile(item/2).
item(tpaired, paired_tratio('D', ['T0', 'EOT'], s_D, [s_T0, s_EOT], 'N', mu)).

intermediate(paired_tratio/6).

:- multifile item//2.
item(tpaired, Response) -->
    { D <- 'D',
      Mu <- mu,
      S_D <- s_D,
      N <- 'N',
      T0 <- 'T0',
      S_T0 <- s_T0,
      EOT <- 'EOT',
      S_EOT <- s_EOT,
      Tails <- tails,
      Alpha <- alpha,
      maplist(mathml, ["HDRS", "T0", "EOT", 'D'], H),
      maplist([X, Y] >> mathml(round1(X), Y), ["Mean", T0, EOT, D], R1),
      maplist([X, Y] >> mathml(round1(X), Y), ["SD", S_T0, S_EOT, S_D], R2)
    }, 
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Phase II clinical study"),
            p(class('card-text'), 
            [ "Consider a clinical study on rumination-focused Cognitive ", 
              "Behavioral Therapy (rfCBT) with ", \mml('N' = round0(N)), 
              " patients. The primary outcome is the score on the ",
              "Hamilton Rating Scale for Depression (HDRS, range from ",
              "best = 0 to worst = 42). The significance level is set to ", 
              \mml(alpha = round('100%'(Alpha))), " ", \mml(Tails), "."
	    ]),
            \table(H, [R1, R2]),
            \download(tpaired)
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Does rfCBT lead to a relevant reduction (i.e., more than ",
	        \mml(mu = Mu), " units) in mean HDRS scores between ",
                "baseline (T0) and End of Treatment (EOT)? Please determine ",
		"the ", span(class('text-nowrap'), [\mml(t), "-ratio."])
              ]),
            \form(Response)
	  ]))
      ]).

% Choose t-ratio for one-sample t-test
expert(paired_tratio, From >> To, Flags, Feed, Hint) :-
    From = paired_tratio(D, _, S, _, N, Mu),
    To   = paired_t(D, Mu, S, N),
    Feed = ["Correctly identified the problem as a ",
            span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
            "samples."],
    Hint = ["This is a ",
            span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
            "samples."].

intermediate(paired_t/4).

% Choose t-ratio for one-sample t-test
expert(paired_t, From >> To, Flags, Feed, Hint) :-
    From = paired_t(D, Mu, S, N),
    To   = tratio(dfrac(D - Mu, S / sqrt(N))),
    Feed = ["Correctly identified the formula for the ",
            span(class('text-nowrap'), [\mml(Flags, t), "-ratio for paired samples."])],
    Hint = ["Determine the ",
            span(class('text-nowrap'), [\mml(Flags, t), "-ratio"]), " ",
            \mml(Flags, dfrac(D - Mu, S / sqrt(N))), "."].

% Choose t-ratio for two-groups t-test
buggy(groups_t, From >> To, Flags, Feed) :-
    From = paired_tratio(D, [T0, EOT], S, [S_T0, S_EOT], N, Mu),
    Inst = groups_t(T0, S_T0, N, EOT, S_EOT, N),
    Of   = paired_t(D, Mu, S, N),
    To   = instead_of(groups_t, Inst, Of),
    Feed = ["The result matches a ",
            span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for independent ",
            "groups. Please use the ",
            span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
            "samples instead."].

intermediate(groups_t/6).

% Expression for two-groups t-test
expert(groups_t, From >> To, Flags, Feed, Hint) :-
    From = groups_t(M_A, S_A, N_A, M_B, S_B, N_B),
    Pool = denoting(subsup(s, "pool", 2), var_pool(S_A^2, N_A, S_B^2, N_B), "the pooled variance"),
    To   = tratio(dfrac(M_A - M_B, sqrt(Pool * (1/N_A + 1/N_B)))),
    Feed = ["Correctly applied the expression for the ",
            span(class('text-nowrap'), [\mml(Flags, t), "-ratio for independent samples."])],
    Hint = ["Determine the ",
            span(class('text-nowrap'), [\mml(Flags, t), "-ratio"]), " ",
            \mml(Flags, dfrac(M_A - M_B, sqrt(Pool * (1/N_A + 1/N_B))))].

% Forgot school math
buggy(school, From >> To, Flags, Feed) :-
    From = 1/A + 1/B,
    Inst = frac(1, A + B),
    To   = instead_of(school, Inst, From),
    Feed = ["Please remember school math: ", 
            \mml(Flags, color(school, color("black", From) \= color("black", Inst)))].

% Forget parentheses
buggy(frac_paren, From >> To, Flags, Feed) :-
    From = dfrac(A - B, C / D),
    To   = left_landed(frac_paren, A - right_landed(frac_paren, 
               dfrac(left_elsewhere(frac_paren, A - B), 
	             right_elsewhere(frac_paren, C / D)) / D)),
    Feed = ["Please check the parentheses around the numerator and the ",
            "denominator of the fraction ",
            \mml(Flags, dfrac(color(frac_paren, paren(black(A - B))), 
	                color(frac_paren, paren(black(C / D))))), "."].

buggy(paren_num, From >> To, Flags, Feed) :-
    From = dfrac(A - B, C / D),
    To   = left_landed(paren_num, A - dfrac(left_elsewhere(paren_num, A - B), C / D)),
    Feed = ["Please check the parentheses around the numerator of the fraction ",
            \mml(Flags, dfrac(color(paren_num, paren(black(A - B))), C / D)), "."].

buggy(paren_denom, From >> To, Flags, Feed) :-
    From = dfrac(A - B, C / D),
    To   = right_landed(paren_denom, dfrac(A - B, right_elsewhere(paren_denom, C / D)) / D),
    Feed = ["Please check the parentheses around the denominator of the ",
            "fraction ", 
	    \mml(Flags, dfrac(A - B, color(paren_denom, paren(black(C / D))))), "."].

% Forget square root
buggy(root, From >> To, Flags, Feed) :-
    From = sqrt(N),
    Inst = N,
    To   = instead_of(root, Inst, From),
    Feed = ["Please do not forget the square root around ", 
            \mml([highlight(all) | Flags], color(root, N)), "."].

r_init(tpaired) :-
    r_init,
    {|r||
    paired_t <- function(d, mu, s, n)
    {
        dfrac(d - mu, s / sqrt(n))
    }

    var_pool <- function(var_A, n_A, var_B, n_B)
    {
        frac((n_A - 1) * var_A + (n_B - 1) * var_B, n_A + n_B - 2)
    }

    tpaired_data <- function(seed)
    {
        set.seed(seed)
	N    = sample(25:40, size=1)
	Id   = 1:N
	X    = rnorm(N, mean=25, sd=5)
	T0   = round(X + rnorm(N, mean=0, sd=4))
	EOT  = round(X + rnorm(N, mean=-6, sd=5))
	D    = T0 - EOT
	data.frame(Id, T0, EOT, D)    
    }

    data  = tpaired_data(seed=4711)
    N     = nrow(data)
    T0    = round(mean(data$T0), 1)
    s_T0  = round(sd(data$T0), 1)
    EOT   = round(mean(data$EOT), 1)
    s_EOT = round(sd(data$EOT), 1)
    D     = round(mean(data$T0 - data$EOT), 1)
    s_D   = round(sd(data$T0 - data$EOT), 1)
    mu    = 5
    alpha = 0.05
    tails = 'two-tailed'
    |},
    csvfile(tpaired, data).

data(tpaired, File) :-
    tempfile(tpaired, File).

%
% Invoke example
%
example :-
    r_init(tpaired, []),
    item(Item),
    solution(Item, Solution, Path),
    writeln(solution: Solution),
    r(Solution, Result),
    writeln(result: Result),
    writeln(path: Path),
    mathml(Solution = Result, Mathml),
    writeln(mathml: Mathml),
    palette(Solution, Flags),
    hints(Flags, Path, Hints),
    writeln(hints: Hints),
    traps(Path, Traps),
    writeln(traps: Traps),
    praise(Path, Praise),
    writeln(praise: Praise).

example :-
    item(Item),
    solution(Item, Solution, Path),
    wrong(Item, Solution, Wrong, Wooden),
    writeln(wrong: Wrong),
    mathex(fix, Wrong, Fix),
    writeln(fix: Fix),
    mathex(show, Wrong, Show),
    writeln(show: Show),
    r(Wrong, Result),
    writeln(result: Result),
    writeln(path: Wooden),
    palette(Wrong, Flags),
    mathml([highlight(all) | Flags], Wrong \= Result, Mathml),
    writeln(mathml: Mathml),
    feedback(Flags, Wooden, Feedback),
    writeln(feedback: Feedback),
    praise(Wooden, Praise),
    writeln(praise: Praise),
    mistakes(Flags, Wooden, Mistakes),
    writeln(mistakes: Mistakes),
    traps(Path, Traps),
    relevant(Mistakes, Traps, Relevant, Irrelevant),
    writeln(relevant: Relevant),
    writeln(irrelevant: Irrelevant).

example :-
    item(Item),
    html(\item(Item, ''), HTML, []),
    print_html(HTML).

