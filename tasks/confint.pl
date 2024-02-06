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

mathml:math_hook(Flags, s_D, Flags, subscript(s, 'D')).
mathml:math_hook(Flags, s_T0, Flags, subscript(s, "T0")).
mathml:math_hook(Flags, s_EOT, Flags, subscript(s, "EOT")).

%    
% Paired t-test, confidence interval
%
:- multifile item/1.
item(confint: 
    paired_confint(m_D, [m_T0, m_EOT], s_D, [s_T0, s_EOT], 'N', mu, alpha)).

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
                "Behavioral Therapy (rfCBT) ",
                "with ", \mml('N' = round0(N)), " patients. The primary outcome ",
                "is the score on the Hamilton Rating Scale for ",
                "Depression (HDRS, range from best = 0 to worst = 42). The ",
                "significance level is set ",
                "to ", \mml(alpha = round('100%'(Alpha))), 
                " ", \nowrap([\mml(Tails), "."])
              ]),
            \table(H, [R1, R2])
	        % \download(confint)
	      ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Does rfCBT lead to a relevant ",
                "reduction (i.e., more than ", \mml(mu = Mu), " units) in ",
                "mean HDRS scores between baseline (T0) ",
                "and End of Treatment (EOT)?" 
              ]),
            \question(question, response, 
              [ "Please determine ",
                "the ", \mml(round('100%'(OneMinusAlpha))), " confidence ",
                "interval for the reduction in HDRS." 
              ], Response)
          ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(confint: paired_confint, From >> To, Flags, Feed, Hint) :-
    From = paired_confint(D, M_wrong, S, S_wrong, N, Mu, Alpha),
    To   = paired_confint_2(D, M_wrong, S, S_wrong, N, Mu, Alpha, T),
    T    = protect(denoting('T', dfrac(D - Mu, S / sqrt(N)), span(["the observed ", math(mi(t)), "-statistic"]))),
    Feed = [ "Correctly identified the problem as ",
             "a ", \nowrap([\mml(Flags, t), "-test"]), " for paired samples." 
           ],
    Hint = [ "This is a ", \nowrap([\mml(Flags, t), "-test"]), " for paired ",
             "samples." 
           ].

intermediate(confint: paired_confint_2/8).

% Used the correct center of the interval
expert(confint: ci_center, From >> To, Flags, Feed, Hint) :-
    From = paired_confint_2(D, _M_wrong, S, _S_wrong, N, Mu, Alpha, T),
    To   = paired_confint_3(D, S, N, Mu, Alpha, T),
    Feed = [ "Correctly spanned the confidence interval ",
             "around ", \nowrap([\mml(Flags, D), "."])
           ],
    Hint = [ \mml(Flags, D), " is the center of the confidence interval." ].

intermediate(confint: paired_confint_3/6).

% Interval around mean T0 or mean EOT (same for SD)
:- multifile buggy/5.
buggy(confint: ci_center, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_2(D, M_wrong, S, S_wrong, N, Mu, Alpha, T),
    nth1(Index, M_wrong, M_W),
    nth1(Index, S_wrong, S_W),
    To   = paired_confint_3(instead_of(ci_center, M_W, D), 
	       instead_of(ci_center, S_W, S), N, Mu, Alpha, T),
    Feed = [ "The confidence interval is based ",
             "on ", \mml(Flags, D), " ",
             "and ", \nowrap([\mml(Flags, S), ","]), " but not ",
             "on ", \mml(Flags, M_W), " and ", \nowrap([\mml(Flags, S_W), "."])
           ],
    Trap = [ \mml(Flags, D), " is the center of the confidence interval, and ",
             "the width derives from ", \nowrap([\mml(Flags, S), "."]) 
           ].

% Interval around mean T0 or mean EOT (SD unchanged)
buggy(confint: ci_center_m, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_2(D, M_wrong, S, _S_wrong, N, Mu, Alpha, T),
    member(M_W, [Mu | M_wrong]),
    To   = paired_confint_3(instead_of(ci_center_m, M_W, D),
           S, N, Mu, Alpha, T),
    Feed = [ "The confidence interval for the change score is spanned ",
             "around ", \nowrap([\mml(Flags, D), ","]), " instead ",
             "of ", \nowrap([\mml(Flags, M_W), "."])
           ],
    Trap = [ \mml(Flags, D), " is the center of the confidence interval." ].

% Interval around mean D, but wrong SD
buggy(confint: ci_center_s, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_2(D, _M_wrong, S, S_wrong, N, Mu, Alpha, T),
    member(S_W, S_wrong),
    To   = paired_confint_3(D, instead_of(ci_center_s, S_W, S),
           N, Mu, Alpha, T),
    Feed = [ "The width of the confidence interval for the change score ",
             "derives from ", \nowrap([\mml(Flags, S), ","]), " instead ",
             "of ", \nowrap([\mml(Flags, S_W), "."])
           ],
    Trap = [ "The width of the confidence interval derives ",
             "from ", \nowrap([\mml(Flags, S), "."])
           ].

% Expert: Choose correct alpha
expert(confint: alpha, From >> To, Flags, Feed, Hint) :-
    From = paired_confint_3(D, S, N, Mu, Alpha, T),
    To   = paired_confint_4(D, S, N, Mu, Alpha, T),
    Level <- 1 - Alpha,
    Feed = [ "The width of the confidence interval is given ",
             "by ", \nowrap([\mml(Flags, 1 - Alpha = '100%'(Level)), "."])
           ],
    Hint = Feed.

intermediate(confint: paired_confint_4/6).

buggy(confint: alpha, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_3(D, S, N, Mu, Alpha, T),
    To   = paired_confint_4(D, S, N, Mu, left_landed(alpha, 0.1*Alpha), T),
    Level <- 1 - 0.1*Alpha,
    Feed = [ "The result matches ",
             "a ", \mml(Flags, '100%'(Level)), " confidence interval. Please ",
             "choose the correct quantile from ",
             "the ", \nowrap([\mml(Flags, t), "-distribution."])
           ],
    Trap = "Choose the wrong significance level.".

% Ignore mu
expert(confint: ci_mu, From >> To, Flags, Feed, Hint) :-
    From = paired_confint_4(D, S, N, Mu, Alpha, T),
    To   = confint(paired_ci(D, S, N, Mu, Alpha, T), 1),
    Feed = "Correctly ignored the null hypothesis in the confidence interval.",
    Hint = [ "The null hypothesis ", \mml(Flags, Mu), " is not used for the ",
             "confidence interval for ", \nowrap([\mml(Flags, D), "."])
           ].

intermediate(confint: paired_ci/6).

% Bug: subtract mu
buggy(confint: ci_mu, From >> To, Flags, Feed, Trap) :-
    From = paired_confint_4(D, S, N, Mu, Alpha, T),
    To   = confint(
               paired_ci(right_landed(ci_mu, D - Mu), S, N, Mu, Alpha, T), 1
           ),
    Feed = [ "Do not subtract the null hypothesis ", \mml(Flags, Mu), " in ",
             "the confidence interval for ", \nowrap([\mml(Flags, D), "."]) 
           ],
    Trap = [ "The null hypothesis ", \mml(Flags, Mu), " is not used for the ",
             "confidence interval for ", \nowrap([\mml(Flags, D), "."])
           ].

expert(confint: paired_ci, From >> To, Flags, Feed, Hint) :-
    From = paired_ci(D, S, N, _Mu, Alpha, _T),
    To   = pm(D, qt(1 - Alpha/2, N-1) * dfrac(S, sqrt(N))),
    Feed = "Correctly applied the expression for the confidence interval.",
    Hint = [ "The confidence interval is within ", 
             \nowrap([
	         \mml(Flags, pm(D, qt(1 - Alpha/2, N-1) * frac(S, sqrt(N)))), 
             "."])
           ].

buggy(confint: lu, From >> To, Flags, Feed, Trap) :-
    From = paired_ci(D, S, N, _Mu, Alpha, _T),
    L    = qt(1 - Alpha/2, N-1),
    R    = dfrac(S, sqrt(N)),
    To   = pm(D, omit_right(lu, L * R)),
    Feed = Trap,
    Trap = [ "Please do not forget to multiply the ",
             "the ", \nowrap([\mml(Flags, t), "-quantile"]), " with the ",
             "the standard error of the mean ",
             "change ", \nowrap([\mml(Flags, S), "."])
           ].

buggy(confint: lulu, From >> To, Flags, Feed, Trap) :-
    From = paired_ci(D, S, N, _Mu, Alpha, _T),
    L    = qt(1 - Alpha/2, N-1),
    R    = dfrac(S, sqrt(N)),
    To   = pm(D, omit_left(lulu, L * R)),
    Feed = Trap,
    Trap = [ "Please do not forget to multiply the standard error with ",
             "the ", \nowrap([\mml(Flags, 1 - Alpha), "-quantile"]), " of ",
             "the ", \nowrap([\mml(Flags, 'T'), "-distribution:"]), " ", 
             \nowrap([
	         \mml(Flags, pm(D, color(lulu, qt(1 - Alpha/2, N-1)) 
		         * frac(S, sqrt(N)))), "."])
           ].

buggy(confint: onetail, From >> To, Flags, Feed, Trap) :-
    From = paired_ci(D, S, N, _Mu, Alpha, _T),
    To   = pm(D, qt(1 - omit_right(onetail, Alpha / 2), N-1) * dfrac(S, sqrt(N))),
    _2alpha <- 2*alpha,
    Feed = [ "The result matches a one-sided confidence interval. Please ",
             "check if you used the correct quantile of ",
             "the ", \nowrap([\mml(Flags, t), "-distribution"]), " for ",
             "the ", i("two-sided"), " confidence interval."
           ],
    Trap = [ "Do not use the ",
             "one-sided ", \nowrap([\mml(Flags, t), "-quantile"]), " in the ",
             "expression for the confidence interval."
           ].

buggy(confint: tstat, From >> To, Flags, Feed, Trap) :-
    From = paired_ci(D, S, N, _Mu, Alpha, T),
    To   = pm(D, instead_of(tstat, T, qt(1 - Alpha/2, N-1)) * dfrac(S, sqrt(N))),
    Feed = [ "The result suggests that the ",
             "observed ", \nowrap([\mml(Flags, t), "-statistic"]), " was used ",
             "for the width of the confidence interval. Please check if you ",
             "used the correct quantile of ",
             "the ", \nowrap([\mml(Flags, t), "-distribution."])
           ],
    Trap = [ "Do not use the ",
             "observed ", \nowrap([\mml(Flags, t), "-statistic"]), " in the ",
             "expression for the confidence interval."
           ].

buggy(confint: se, From >> To, Flags, Feed, Trap) :-
    From = pm(D, qt(1 - Alpha, N-1) * dfrac(S, sqrt(N))),
    To   = pm(D, qt(1 - Alpha, N-1) * instead_of(se, S, dfrac(S, sqrt(N)))),
    Feed = [ "The standard deviation ", \mml(Flags, S), " was used instead of ",
             "the standard ",
             "error ", \nowrap([\mml(Flags, frac(S, sqrt(N))), "."])
           ],
    Trap = [ "Please do not forget to divide the standard deviation by the ",
             "square root of ", \nowrap([\mml(Flags, N), "."])
           ].
 
buggy(confint: root, From >> To, Flags, Feed, Trap) :-
    From = sqrt(N),
    Inst = N,
    To   = instead_of(root, Inst, From),
    Feed = [ "Please do not forget the square root around ",
             \nowrap([\mml([highlight(all) | Flags], color(root, N)), "."])
           ],
    Trap = Feed.

:- multifile r_init/1.
r_init(confint) :-
    r_init,
    {|r||
        paired_t <- function(d, mu, s, n)
        {
            dfrac(d - mu, s / sqrt(n))
        }

        paired_ci <- function(d, s, n, mu, alpha, t)
        {
            pm(d, qt(1-alpha/2, df=n-1) * s / sqrt(n))
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

        # Exam 2018-06 B
        N     = 22
        m_T0  = 18.7
        s_T0  = 7.9
        m_EOT = 13.0
        s_EOT = 7.9
        m_D   = 5.7
        s_D   = 4.2
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

