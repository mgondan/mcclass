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
% Paired t-test, t-ratio
%
:- multifile item/1.
item(tpaired: paired_tratio(m_D, [m_T0, m_EOT], s_D, [s_T0, s_EOT], 'N', mu)).

:- multifile intermediate/1.
intermediate(tpaired: paired_tratio/6).

:- multifile item//2.
item(tpaired, Response) -->
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
	    % \download(tpaired)
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

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(tpaired: paired_tratio, From >> To, Flags, Feed, Hint) :-
    From = paired_tratio(D, M_wrong, S, S_wrong, N, Mu),
    To   = paired_tratio_2(D, M_wrong, S, S_wrong, N, Mu),
    Feed = [ "Correctly identified the problem as a ",
             span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
             "samples." ],
    Hint = [ "This is a ",
             span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
             "samples." ].

intermediate(tpaired: paired_tratio_2/6).

% Choose correct numbers for numerator
expert(tpaired: minuend, From >> To, Flags, Feed, Hint) :-
    From = paired_tratio_2(D, M_other, S, S_wrong, N, Mu),
    To   = paired_tratio_3(D, M_other, S, S_wrong, N, Mu),
    Feed = [ "Correctly identified the numerator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ],
    Hint = [ "The numerator is ",
             span(class('text-nowrap'), [\mml(Flags, D - Mu), "."]) ].

intermediate(tpaired: paired_tratio_3/6).

% Other solutions
:- multifile buggy/5.
buggy(tpaired: minuend, From >> To, Flags, Feed, Trap) :-
    From = paired_tratio_2(D, M_other, S, S_wrong, N, Mu),
    member(Min, [Mu | M_other]),
    To   = paired_tratio_3(instead_of(minuend, Min, D), M_other, S, S_wrong, N, Mu),
    Feed = [ "Please check the numerator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ],
    Trap = [ "The numerator is ",
             span(class('text-nowrap'), [\mml(Flags, color(minuend, D) - cdots), "."]) ].

% Choose correct numbers for numerator: subtrahend
expert(tpaired: subtrahend, From >> To, Flags, Feed, Hint) :-
    From = paired_tratio_3(D, _, S, S_wrong, N, Mu),
    To   = paired_tratio_4(D, S, S_wrong, N, Mu),
    Feed = [ "Correctly identified the numerator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ],
    Hint = [ "The numerator is ",
             span(class('text-nowrap'), [\mml(Flags, D - Mu), "."]) ].

intermediate(tpaired: paired_tratio_4/5).

% Other solutions
buggy(tpaired: subtrahend, From >> To, Flags, Feed, Trap) :-
    From = paired_tratio_3(D, M_other, S, S_wrong, N, Mu),
    member(Sub, [D | M_other]),
    To   = paired_tratio_4(instead_of(subtrahend, Sub, Mu), S, S_wrong, N, Mu),
    Feed = [ "Please check the numerator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ],
    Trap = [ "The numerator is ",
             span(class('text-nowrap'), [\mml(Flags, cdots - color(subtrahend, Mu)), "."]) ].

% Choose correct numbers for denominator
expert(tpaired: denominator, From >> To, Flags, Feed, Hint) :-
    From = paired_tratio_4(D, S, _, N, Mu),
    To   = paired_t(D, Mu, S, N),
    Feed = [ "Correctly identified the denominator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ],
    Hint = [ \mml(Flags, S), " is used in the denominator of the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio."]) ].

intermediate(tpaired: paired_t/4).

% Use wrong standard deviation
buggy(tpaired: denominator, From >> To, Flags, Feed, Trap) :-
    From = paired_tratio_4(D, S_D, S_wrong, N, Mu),
    member(S, S_wrong),
    To   = paired_t(D, Mu, instead_of(sd, S, S_D), N),
    Feed = [ "Please use the standard deviation of the change scores ",
             \mml(Flags, S_D), " for the ", 
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio"]),
	         " instead of ", 
	         span(class('text-nowrap'), [\mml(Flags, color(sd, S)), "."])
	       ],
    Trap = [ "Use the standard deviation of the change scores in the ", 
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio "]),
	         "instead of ", 
	         span(class('text-nowrap'), 
	         [\mml(Flags, list((' ', "or", ' '), S_wrong)), "."]) 
	       ].

% Choose t-ratio for one-sample t-test
expert(tpaired: paired_t, From >> To, Flags, Feed, Hint) :-
    From = paired_t(D, Mu, S, N),
    To   = tratio(dfrac(D - Mu, S / sqrt(N))),
    Feed = [ "Correctly identified the formula for the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio for paired samples."])],
    Hint = [ "Determine the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio"]), " ",
             \mml(Flags, dfrac(D - Mu, S / sqrt(N))), "."].

% Choose t-ratio for two-groups t-test
buggy(tpaired: groups_t, From >> To, Flags, Feed, Trap) :-
    From = paired_tratio(D, [T0, EOT], S, [S_T0, S_EOT], N, Mu),
    Inst = groups_t(T0, S_T0, N, EOT, S_EOT, N),
    Of   = paired_t(D, Mu, S, N),
    To   = instead_of(groups_t, Inst, Of),
    Feed = [ "The result matches a ",
             span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for independent ",
             "groups. Please use the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-test"]), " for paired ",
             "samples instead."],
    Trap = [ "Do not use the ", span(class('text-nowrap'), [\mml(Flags, t), "-test "]), 
	         "for independent groups in this problem with paired measurements."].

intermediate(tpaired: groups_t/6).

% Expression for two-groups t-test
expert(tpaired: groups_t, From >> To, Flags, Feed, Hint) :-
    From = groups_t(M_A, S_A, N_A, M_B, S_B, N_B),
    Pool = denoting(subsup(s, "pool", 2), var_pool(S_A^2, N_A, S_B^2, N_B), "the pooled variance"),
    To   = tratio(dfrac(M_A - M_B, sqrt(Pool * (1/N_A + 1/N_B)))),
    Feed = [ "Correctly applied the expression for the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio for independent samples."])],
    Hint = [ "Determine the ",
             span(class('text-nowrap'), [\mml(Flags, t), "-ratio"]), " ",
             \mml(Flags, dfrac(M_A - M_B, sqrt(Pool * (1/N_A + 1/N_B))))
           ].

% Forgot school math
buggy(tpaired: school, From >> To, Flags, Feed, Trap) :-
    From = 1/A + 1/B,
    dif(A, B),
    Inst = frac(1, A + B),
    To   = instead_of(school, Inst, From),
    Feed = [ "Please remember school math: ", 
             \mml(Flags, color(school, color("black", From) \= color("black", Inst)))
           ],
    Trap = [ "Please remember school math: ",
             \mml(Flags, From = frac(A + B, A*B))
           ].

buggy(tpaired: school, From >> To, Flags, Feed, Trap) :-
    From = 1/A + 1/A,
    Inst = frac(1, 2*A),
    To   = instead_of(school, Inst, From),
    Feed = [ "Please remember school math: ",
             \mml(Flags, color(school, color("black", From) \= color("black", Inst)))
           ],
    Trap = [ "Please remember school math: ",
             \mml(Flags, From = frac(2, A))
           ].

% Forget parentheses
buggy(tpaired: frac_paren, From >> To, Flags, Feed, Trap) :-
    From = dfrac(A - B, C / D),
    To   = left_landed(frac_paren, A - right_landed(frac_paren, 
               dfrac(left_elsewhere(frac_paren, A - B), 
	               right_elsewhere(frac_paren, C / D)) / D)),
    Feed = [ "Please check the parentheses around the numerator and the ",
             "denominator of the fraction ",
             span(class('text-nowrap'), 
               [ \mml(Flags, dfrac(color(frac_paren, paren(black(A - B))), 
	             color(frac_paren, paren(black(C / D))))), "."
               ])
           ],
    Trap = Feed.

buggy(tpaired: paren_num, From >> To, Flags, Feed, Trap) :-
    From = dfrac(A - B, C / D),
    To   = left_landed(paren_num, A - dfrac(left_elsewhere(paren_num, A - B), C / D)),
    Feed = [ "Please check the parentheses around the numerator of the fraction ",
             span(class('text-nowrap'), 
               [ \mml(Flags, dfrac(color(paren_num, paren(black(A - B))), C / D)), "."
               ])
           ],
    Trap = Feed.

buggy(tpaired: paren_denom, From >> To, Flags, Feed, Trap) :-
    From = dfrac(A - B, C / D),
    To   = right_landed(paren_denom, dfrac(A - B, right_elsewhere(paren_denom, C / D)) / D),
    Feed = [ "Please check the parentheses around the denominator of the ",
             "fraction ", 
             span(class('text-nowrap'), 
               [ \mml(Flags, dfrac(A - B, color(paren_denom, paren(black(C / D))))), "."
               ])
           ],
    Trap = Feed.

% Forget square root
buggy(tpaired: root, From >> To, Flags, Feed, Trap) :-
    From = sqrt(N),
    Inst = N,
    To   = instead_of(root, Inst, From),
    Feed = [ "Please do not forget the square root around ", 
             span(class('text-nowrap'), 
               [ \mml([highlight(all) | Flags], color(root, N)), "."
               ])
           ],
    Trap = Feed.

% Forget to subtract the mu from the null hypothesis
buggy(tpaired: mu, From >> To, Flags, Feed, Trap) :-
    From = dfrac(D - mu, S / SQRTN),
    To   = dfrac(omit_right(mu, D - mu), S / SQRTN),
    Feed = [ "Please do not forget to subtract the average change ", 
             \mml(Flags, color(mu, mu)), " under the null hypothesis." 
           ],
    Trap = Feed.

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
    csvfile(tpaired, data).

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = tpaired,
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
    Topic = tpaired,
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
    Topic = tpaired,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = tpaired,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

