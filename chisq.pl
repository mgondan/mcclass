% chi-square test for rate comparisons
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

mathml:math_hook(Flags, p_VR, Flags, sub(p, "VR")).
mathml:math_hook(Flags, p_Box, Flags, sub(p, "Box")).

mathml:math_hook(Flags, s_VR, Flags, sub(s, "VR")).
mathml:math_hook(Flags, s_Box, Flags, sub(s, "Box")).

mathml:math_hook(Flags, n_VR, Flags, sub(n, "VR")).
mathml:math_hook(Flags, n_Box, Flags, sub(n, "Box")).

% R constants
r:pl_hook(p_VR, r(p_VR)).
r:pl_hook(s_VR, r(s_VR)).
r:pl_hook(n_VR, r(n_VR)).
r:pl_hook(p_Box, r(p_Box)).
r:pl_hook(s_Box, r(s_Box)).
r:pl_hook(n_Box, r(n_Box)).
r:pl_hook(p_pool1(S_A, N_A, S_B, N_B), r(p_pool(pl(S_A), pl(N_A), pl(S_B), pl(N_B)))).

%    
% chisquare-test for independent groups, ratio
%
:- multifile item/1.
item(chisq: chisq_ratio(s_VR, n_VR, s_Box, n_Box)).

:- multifile intermediate/1.
intermediate(chisq: chisq_ratio/4).

:- multifile item//2.
item(chisq, Response) -->
    { rod(p_VR, P_VR),
      rod(s_VR, S_VR),
      rod(n_VR, N_VR),
      rod(p_Box, P_Box),
      rod(s_Box, S_Box),
      rod(n_Box, N_Box)
    }, 
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
              [ "Here we look at the speed of the operation. The publication ",
                "states the following:"
              ]),
            p(class('card-text'),
              [ "“Laparoscopy-naïve medical students were randomized into ",
                "two groups. (...) The VR group completed the operation more ",
                "often within 80 min than the Box ",
                "group ", \nowrap(["(", \mml('100%'(P_VR))]), " ",
                "vs. ", \nowrap([\mml('100%'(P_Box)), ")."]), "The percentages ",
                "correspond to ", \mml(S_VR), " people (out ",
                "of ", \nowrap([\mml(N_VR), ")"]), " in the VR group ",
                "and ", \mml(S_Box), " people (out ",
                "of ", \nowrap([\mml(N_Box), ")"]), " in the Box group. "
              ])
	    % \download(tgroups)
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Does VR training lead to faster surgery times than traditional Box training?" 
              ]),
            \question(question, 
                response, 
                ["Please determine the ", \nowrap([\mml(chi^2), "-statistic."])], 
                Response)
	      ]))
      ]).

:- multifile expert/5.
expert(chisq: chisq_ratio, From >> To, Flags, Feed, Hint) :-
    From = chisq_ratio(S_A, N_A, S_B, N_B),
    To   = chisq_chi2(S_A, N_A, S_B, N_B),
    Feed = [ "Correctly identified the problem as ",
             "a ", \nowrap([\mml(Flags, chi^2), "-test"]), " for rate ",
             "comparisons."
	   ],
    Hint = [ "This is a ", \nowrap([\mml(Flags, chi^2), "-test "]), " for ", 
             "rate comparisons."
           ].

intermediate(chisq: chisq_chi2/4).

% Expression for two-group comparison
expert(chisq: chisq_chi2, From >> To, Flags, Feed, Hint) :-
    From = chisq_chi2(S_A, N_A, S_B, N_B),
    Pool = denoting(sub(p, "pool"), 
               p_pool(S_A, N_A, S_B, N_B), 
               "the pooled success rate"),
    To   = chi2ratio(dfrac((dec(S_A/N_A, 2) - dec(S_B/N_B, 2))^2, dec(Pool, 2) * dec(1 - Pool, 2) * dec(frac(1, N_A) + frac(1, N_B), 2)), 1),
    Feed = [ "Correctly applied the expression for the ",
             \nowrap([\mml(Flags, chi^2), "-ratio"]), " for rate comparisons."
           ],
    Hint = [ "Determine the ", \nowrap([\mml(Flags, chi^2), "-ratio:"]), " ",
             \mml(Flags, dfrac((sub(p, "A") - sub(p, "B"))^2, Pool * (1-Pool) * (frac(1, sub(n, "A")) + frac(1, sub(n, "B")))))
           ].

intermediate(chisq: p_pool/4).

expert(chisq: pool, From >> To, _, [], []) :-
    From = p_pool(S_A, N_A, S_B, N_B),
    To   = p_pool1(S_A, N_A, S_B, N_B).

:- multifile buggy/5.
buggy(chisq: confuse, From >> To, _Flags, Feed, Hint) :-
    From = p_pool(S_A, N_A, S_B, N_B),
    To   = p_pool1(1 + instead_of(confuse, S_B, S_A), N_A, S_A, N_B),
    Feed = [ "123a" ],
    Hint = [ "456a" ].

:- multifile r_init/1.
r_init(chisq) :-
    r_init,
    {|r||
        frac <- `/`

        p_pool <- function(s_A, n_A, s_B, n_B)
        {
            frac(s_A + s_B, n_A + n_B)
        }

        chisq_pvalue <- function(s_A, n_A, s_B, n_B)
        {
            pp = p_pool(s_A, n_A, s_B, n_B)

            p_A = s_A/n_A
            p_B = s_B/n_B
            frac((p_A - p_B)^2, pp * (1 - pp) * (1/n_A + 1/n_B))
        }

        alpha = 0.05
        tails = 'two-tailed'

        # Exam 2019
        s_Box = 7
        n_Box = 50
        p_Box = s_Box/n_Box
        s_VR  = 20
        n_VR  = 48
        p_VR  = s_VR/n_VR
    |},
    % csvfile(chisq, data),
    true.

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = chisq,
    r_init(Topic),
    item(Topic: Item),
    solution(Topic, Item, Solution, Path),
    writeln(solution: Solution),
    rod(Solution, Result),
    writeln(result: Result),
    writeln(path: Path),
    mathml(Solution = quantity(Result), Mathml),
    writeln(mathml: Mathml),
    hints(Topic, Item, Path, _, Hints),
    writeln(hints: Hints),
    traps(Topic, Item, Path, _, Traps),
    writeln(traps: Traps),
    praise(Topic, Item, Path, _, Praise),
    writeln(praise: Praise).

example :-
    Topic = chisq,
    r_init(Topic),
    item(Topic: Item),
    wrong(Topic, Item, Wrong, Woodden),
    writeln(wrong: Wrong),
    mathex(fix, Wrong, Fix),
    writeln(fix: Fix),
    mathex(show, Wrong, Show),
    writeln(show: Show),
    rod(Wrong, Result),
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
    Topic = chisq,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = chisq,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

