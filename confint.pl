% t-test for paired samples
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

mathml:math_hook(Flags, m_D, Flags, overline('D')).
mathml:math_hook(Flags, m_T0, Flags, overline("T0")).
mathml:math_hook(Flags, m_EOT, Flags, overline("EOT")).

mathml:math_hook(Flags, s_D, Flags, sub(s, 'D')).
mathml:math_hook(Flags, s_T0, Flags, sub(s, "T0")).
mathml:math_hook(Flags, s_EOT, Flags, sub(s, "EOT")).

%    
% Paired t-test, confidence interval
%
:- multifile item/1.
item(confint: paired_confint(m_D, [m_T0, m_EOT], s_D, [s_T0, s_EOT], 'N', mu, alpha)).

:- multifile intermediate/1.
intermediate(confint: paired_confint/7).

:- multifile item//2.
item(confint, Response) -->
    { D <- m_D,
      Mu <- mu,
      S_D <- s_D,
      N <- 'N',
      T0 <- m_T0,
      S_T0 <- s_T0,
      EOT <- m_EOT,
      S_EOT <- s_EOT,
      Tails <- tails,
      Alpha <- alpha,
      OneMinusAlpha <- 1 - alpha,
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
            \table(H, [R1, R2])
	        % \download(confint)
	      ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Does rfCBT lead to a relevant reduction (i.e., more than ",
	            \mml(mu = Mu), " units) in mean HDRS scores between ",
                "baseline (T0) and End of Treatment (EOT)?" 
              ]),
            \question(question, response, 
                [ "Please determine the ", \mml(round('100%'(OneMinusAlpha))), 
                  " confidence interval for the reduction in HDRS." 
                ], Response)
	      ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(confint: paired_confint, From >> To, Flags, Feed, Hint) :-
    From = paired_confint(D, M_wrong, S, S_wrong, N, Mu, Alpha),
    To   = paired_confint_2(D, M_wrong, S, S_wrong, N, Mu, Alpha),
    Feed = [ "Correctly identified the problem as a ",
             \nowrap([\mml(Flags, t), "-test"]), " for paired samples." 
           ],
    Hint = [ "This is a ",
             \nowrap([\mml(Flags, t), "-test"]), " for paired samples." 
           ].

intermediate(confint: paired_confint_2/7).

% Used the correct center of the interval
expert(confint: ci_center, From >> To, Flags, Feed, Hint) :-
    From = paired_confint_2(D, _M_wrong, S, _S_wrong, N, Mu, Alpha),
    To   = paired_confint_3(D, S, N, Mu, Alpha),
    Feed = [ "Correctly spanned the confidence interval around ",
             \nowrap([\mml(Flags, D), "."])
           ],
    Hint = [ \mml(Flags, D), " is the center of the confidence interval." ].

intermediate(confint: paired_confint_3/5).

% Used the correct center of the interval
buggy(confint: ci_center, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_2(D, M_wrong, S, S_wrong, N, Mu, Alpha),
    nth1(Index, M_wrong, M_W),
    nth1(Index, S_wrong, S_W),
    To   = paired_confint_3(instead_of(ci_center, M_W, D), instead_of(ci_center, S_W, S), N, Mu, Alpha),
    Feed = [ "The confidence interval is based on ", \mml(Flags, D), " and ", 
             \nowrap([\mml(Flags, S), ","]), " not ", \mml(Flags, M_W), " and ",
	     \nowrap([\mml(Flags, S_W), "."])
           ],
    Trap = [ \mml(Flags, D), " is the center of the confidence interval." ].

% Ignore mu
expert(confint: ci_mu, From >> To, Flags, Feed, Hint) :-
    From = paired_confint_3(D, S, N, Mu, Alpha),
    To   = confint(paired_ci(D, S, N, Alpha), digits=1),
    Feed = [ "Correctly ignored the null hypothesis in the confidence interval." ],
    Hint = [ "The null hypothesis ", \mml(Flags, Mu), " is not used for the ",
             "confidence interval."
           ].

intermediate(confint: paired_ci/4).

% Bug: subtract mu
buggy(confint: ci_mu, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_3(D, S, N, Mu, Alpha),
    To   = confint(paired_ci(right_landed(ci_mu, D - Mu), S, N, Alpha), digits=1),
    Feed = [ "Do not subtract the null hypothesis ", \mml(Flags, Mu), " in the ",
             "confidence interval." 
           ],
    Trap = [ "The null hypothesis ", \mml(Flags, Mu), " is not used for the ",
             "confidence interval."
           ].

expert(confint: paired_ci, From >> To, Flags, Feed, Hint) :-
    From = paired_ci(D, S, N, Alpha),
    To   = pm(D, qt(1 - Alpha/2, N-1) * dfrac(S, sqrt(N))),
    Feed = "Correctly applied the expression for the confidence interval.",
    Hint = [ "The confidence interval is within ", 
             \nowrap([\mml(Flags, pm(D, qt(1 - Alpha/2, N-1) * frac(S, sqrt(N)))), "."])
           ].

buggy(confint: lulu, From >> To, Flags, Feed, Trap) :-
    From = pm(D, qt(1 - Alpha/2, N-1) * SE),
    To   = pm(D, omit_left(lulu, qt(1 - Alpha/2, N-1) * SE)),
    Feed = Trap,
    Trap = [ "Please do not forget to multiply the standard error with the ",
             \nowrap([\mml(Flags, 1 - Alpha), "-quantile"]), " of the ", 
             \nowrap([\mml(Flags, 'T'), "-distribution:"]), " ", 
             \nowrap([\mml(Flags, pm(D, color(lulu, qt(1 - Alpha/2, N-1)) * SE)), "."])
           ].

buggy(confint: se, From >> To, Flags, Feed, Trap) :-
    From = pm(D, qt(1 - Alpha/2, N-1) * dfrac(S, sqrt(N))),
    To   = pm(D, qt(1 - Alpha/2, N-1) * instead_of(se, S, dfrac(S, sqrt(N)))),
    Feed = [ "The standard deviation ", \mml(Flags, S), " was used instead of ",
             "the standard error ", \nowrap([\mml(Flags, frac(S, sqrt(N))), "."])
           ],
    Trap = [ "Please do not forget to divide the standard deviation by the square root of ",
             \nowrap([\mml(Flags, N), "."])
           ].
 
:- multifile r_init/1.
r_init(confint) :-
    r_init,
    {|r||
        paired_t <- function(d, mu, s, n)
        {
            dfrac(d - mu, s / sqrt(n))
        }

        paired_ci <- function(d, s, n, alpha)
        {
            pm(d, qt(1-alpha/2, df=n-1) * s / sqrt(n))
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
        m_T0  = round(mean(data$T0), 1)
        s_T0  = round(sd(data$T0), 1)
        m_EOT = round(mean(data$EOT), 1)
        s_EOT = round(sd(data$EOT), 1)
        m_D   = round(mean(data$T0 - data$EOT), 1)
        s_D   = round(sd(data$T0 - data$EOT), 1)
        mu    = 5
        alpha = 0.05
        tails = 'two-tailed'

        # Exam 2018-06ag
        N     = 24
        m_T0  = 24.0
        s_T0  = 5.1
        m_EOT = 18.3
        s_EOT = 4.7
        m_D   = 5.8
        s_D   = 3.8
        mu    = 4.0
    |},
    csvfile(confint, data).

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = confint,
    r_init(Topic),
    item(Topic: Item),
    solution(Topic, Item, Solution, Path),
    writeln(solution: Solution),
    r(Solution, Result),
    writeln(result: Result),
    writeln(path: Path),
    mathml(Solution = number(Result), Mathml),
    writeln(mathml: Mathml),
    hints(Topic, Item, Path, _, Hints),
    writeln(hints: Hints),
    traps(Topic, Item, Path, _, Traps),
    writeln(traps: Traps),
    praise(Topic, Item, Path, _, Praise),
    writeln(praise: Praise).

example :-
    Topic = confint,
    r_init(Topic),
    item(Topic: Item),
    wrong(Topic, Item, Wrong, Woodden),
    writeln(wrong: Wrong),
    mathex(fix, Wrong, Fix),
    writeln(fix: Fix),
    mathex(show, Wrong, Show),
    writeln(show: Show),
    r(Wrong, Result),
    writeln(result: Result),
    writeln(path: Woodden),
    palette(Wrong, Flags),
    mathml([highlight(all) | Flags], Wrong \= Result, Mathml),
    writeln(mathml: Mathml),
    feedback(Topic, Item, Woodden, Flags, _, Feedback),
    writeln(feedback: Feedback),
    praise(Topic, Item, Woodden, Code_Praise, Praise),
    writeln(praise: Praise),
    mistakes(Topic, Item, Woodden, Flags, Code_Mistakes, Mistakes),
    writeln(mistakes: Mistakes),
    solution(Topic, Item, _, Path),
    hints(Topic, Item, Path, Code_Hints, _),
    relevant(Code_Praise, Code_Hints, Rel_Praise, Irrel_Praise),
    writeln(relevant_praise: Rel_Praise),
    writeln(irrelevant_praise: Irrel_Praise),
    traps(Topic, Item, Path, Code_Traps, _),
    relevant(Code_Mistakes, Code_Traps, Rel_Mistakes, Irrel_Mistakes),
    writeln(relevant_mistakes: Rel_Mistakes),
    writeln(irrelevant_mistakes: Irrel_Mistakes).

example :-
    Topic = confint,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = confint,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

