% t-test for paired samples
:- use_module(relevant).
:- use_module(intermediate).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).
:- consult(html).
:- consult(temp).
:- use_module(r).

mathml:math_hook(Flags, m_VR, Flags, overline("VR")).
mathml:math_hook(Flags, m_Box, Flags, overline("Box")).

mathml:math_hook(Flags, s_VR, Flags, sub(s, "VR")).
mathml:math_hook(Flags, s_Box, Flags, sub(s, "Box")).

mathml:math_hook(Flags, n_VR, Flags, sub(n, "VR")).
mathml:math_hook(Flags, n_Box, Flags, sub(n, "Box")).

% R constants
r:pl_hook(m_VR, r(m_VR)).
r:pl_hook(s_VR, r(s_VR)).
r:pl_hook(n_VR, r(n_VR)).
r:pl_hook(m_Box, r(m_Box)).
r:pl_hook(s_Box, r(s_Box)).
r:pl_hook(n_Box, r(n_Box)).
r:pl_hook(groups_pvalue(M_A, S_A, N_A, M_B, S_B, N_B), r(groups_pvalue(M_A, S_A, N_A, M_B, S_B, N_B))).

%    
% t-test for independent groups, t-ratio
%
:- multifile item/1.
item(tgroups: groups_tratio(m_VR, s_VR, n_VR, m_Box, s_Box, n_Box)).

:- multifile intermediate/1.
intermediate(tgroups: groups_tratio/6).

:- multifile item//2.
item(tgroups, Response) -->
    { rod(m_VR, M_VR),
      rod(s_VR, S_VR),
      rod(n_VR, N_VR),
      rod(m_Box, M_Box),
      rod(s_Box, S_Box),
      rod(n_Box, N_Box),
      rod(groups_pvalue(m_VR, s_VR, n_VR, m_Box, s_Box, n_Box), P)
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
              [ "Please check the following text from the publication ",
                "(40 ± 10 means “average 40, standard deviation 10”):"
              ]),
            p(class('card-text'),
              [ "“Laparoscopy-naïve medical students were randomized into ",
                "two groups. The Box ",
                "group ", \nowrap(["(", \mml(n = N_Box), ")"]), " used ",
                "E-learning for laparoscopic cholecystectomy and practiced ",
                "basic skills with Box trainers. The VR ",
                "group ", \nowrap(["(", \mml(n = N_VR), ")"]), "trained ",
                "basic skills and laparoscopic cholecystectomy on ",
                "LAP Mentor II (Simbionix, Cleveland, USA). Each group ",
                "trained 3 × 4 hours followed by a knowledge test. Blinded ",
                "raters assessed the operative performance using the ",
                "Objective Structured Assessment of Technical Skills ",
                "(OSATS). The VR group completed the operation significantly ",
                "faster and more often within 80 min than the Box ",
                "group (VR: 28% vs. Box: 22%, p = 0.596). The Box group ",
                "scored higher than the VR group in the knowledge ",
                "test (Box: 13.4 ± 1.2 vs. VR: 10.8 ± 1.8, p < 0.001). Both ",
                "groups showed equal operative performance in the OSATS score ",
                \nowrap(["(VR: ", \mml(pm(round1(M_VR), round1(S_VR)))]), 
                " vs. ",
                \nowrap(["Box: ", \mml(pm(round1(M_Box), round1(S_Box))), ","]),
                " ",
                \nowrap([\mml(pvalue(P)), ")."]), " Students generally ",
                "liked training and felt well prepared for assisting in ",
                "laparoscopic surgery. The efficiency of the training was ",
                "judged higher by the VR group than by the Box group."
              ])
	    % \download(tgroups)
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Is VR training superior to traditional Box training?" 
              ]),
            \question(question, 
                response, 
                ["Please determine the ", \nowrap([\mml(t), "-ratio."])], 
                Response)
	      ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(tgroups: groups_tratio, From >> To, Flags, Feed, Hint) :-
    From = groups_tratio(M_A, S_A, N_A, M_B, S_B, N_B),
    To   = groups_t(M_A, S_A, N_A, M_B, S_B, N_B),
    Feed = [ "Correctly identified the problem as ",
             "a ", \nowrap([\mml(Flags, t), "-test"]), " for independent ",
	     "groups."
	   ],
    Hint = [ "This is a ", \nowrap([\mml(Flags, t), "-test "]), " for ", 
             "independent groups."
           ].

intermediate(tgroups: groups_t/6).

% Expression for two-groups t-test
expert(tgroups: groups_t, From >> To, Flags, Feed, Hint) :-
    From = groups_t(M_A, S_A, N_A, M_B, S_B, N_B),
    Pool = denoting(subsup(s, "pool", 2), 
               var_pool(S_A^2, N_A, S_B^2, N_B), 
               "the pooled variance"),
    To   = tratio(dfrac(dec(M_A - M_B, 2), dec(sqrt(Pool * dec(frac(1, N_A) + frac(1, N_B), 3)), 2)), N_A + N_B - 2),
    Feed = [ "Correctly applied the expression for the ",
             \nowrap([\mml(Flags, t), "-ratio"]), " for independent samples."
           ],
    Hint = [ "Determine the ", \nowrap([\mml(Flags, t), "-ratio:"]), " ",
             \mml(Flags, dfrac(M_A - M_B, sqrt(Pool * (frac(1, N_A) + frac(1, N_B)))))
           ].

intermediate(tgroups: var_pool/4).

% Switch SDs in pooled variance
expert(tgroups: vp, From >> To, _, [], []) :-
    From = var_pool(V_A, N_A, V_B, N_B),
    To   = var_pool1(V_A, N_A, V_B, N_B).

buggy(tgroups: vp, From >> To, _Flags, Feed, Trap) :-
    From = var_pool(V_A, N_A, V_B, N_B),
    To   = var_pool1(instead_of(vp, V_B, V_A), N_A, instead_of(vp, V_A, V_B), N_B),
    Feed = [ "The SDs have been confused in the expression for the pooled variance." ],
    Trap = [ "Do not confuse the SDs in the pooled variance." ].

% Forgot school math
buggy(tgroups: school, From >> To, Flags, Feed, Trap) :-
    From = frac(1, A) + frac(1, B),
    dif(A, B),
    Inst = frac(1, A + B),
    To   = instead_of(school, Inst, From),
    Feed = [ "Please remember school math: ", 
             \mml(Flags, color(school, black(From) \= black(Inst)))
           ],
    Trap = [ "Please remember school math: ",
             \mml(Flags, From = frac(A + B, A*B))
           ].

buggy(tgroups: school, From >> To, Flags, Feed, Trap) :-
    From = frac(1, A) + frac(1, A),
    Inst = frac(1, 2*A),
    To   = instead_of(school, Inst, From),
    Feed = [ "Please remember school math: ",
             \mml(Flags, color(school, black(From) \= black(Inst)))
           ],
    Trap = [ "Please remember school math: ",
             \mml(Flags, From = frac(2, A))
           ].

% Forget parentheses
buggy(tgroups: paren_num, From >> To, Flags, Feed, Trap) :-
    From = dfrac(A - B, Den),
    To   = left_landed(paren_num, 
               A - dfrac(left_elsewhere(paren_num, A - B), Den)),
    Feed = [ "Please check the parentheses around the numerator of the ",
             "fraction ",
             \nowrap(
               [ \mml(Flags, 
                     dfrac(color(paren_num, paren(black(A - B))), Den)), 
                 "."
               ])
           ],
    Trap = Feed.

% Forget square root
buggy(tgroups: root, From >> To, Flags, Feed, Trap) :-
    From = sqrt(N),
    Inst = N,
    To   = instead_of(root, Inst, From),
    Feed = [ "Please do not forget the square root around ", 
             \nowrap([\mml([highlight(all) | Flags], color(root, N)), "."])
           ],
    Trap = Feed.

:- multifile r_init/1.
r_init(tgroups) :-
    r_init,
    {|r||
        frac <- `/`

        var_pool1 <- function(v_A, n_A, v_B, n_B)
        {
            frac((n_A - 1) * v_A + (n_B - 1) * v_B, n_A + n_B - 2)
        }

        groups_pvalue <- function(m_A, s_A, n_A, m_B, s_B, n_B)
        {
            vp = var_pool1(s_A^2, n_A, s_B^2, n_B)
            t = (m_A - m_B) / sqrt(vp * (1/n_A + 1/n_B))
            2 * pt(-abs(t), df=n_A + n_B - 2)
        }

        alpha = 0.05
        tails = 'two-tailed'

        # Exam 2019
        m_Box = 46.0
        s_Box = 14.4
        n_Box = 36
        m_VR  = 48.4
        s_VR  = 11.7
        n_VR  = 40
    |},
    % csvfile(tgroups, data),
    true.

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = tgroups,
    r_init(Topic),
    item(Topic: Item),
    solution(Topic, Item, Solution, Path),
    writeln(solution: Solution),
    rod(Solution, Result),
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
    Topic = tgroups,
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
    Topic = tgroups,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = tgroups,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

