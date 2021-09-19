:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(ztrans) :-
    data(ztrans).

data(ztrans) :-
    r_init,

    {|r||
        x <- 88
        mu <- 100
        sigma <- 15
    |}.

mathml:hook(Flags, x, Flags, 'X').

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
    R <- Expr,
    [Res] = R,
    number(Res).

render(ztrans, item(X, Mu, Sigma), Form) -->
    { option(resp(R), Form, '## %') },
    html(
      [ div(class(card), div(class('card-body'),
        [ h1(class('card-title'), "Normal distribution"),
          p(class('card-text'), 
            [ "Let ", \mmlm('X'), " follow a Normal distribution with ",
              "expectation ", \mmlm([round(0)], Mu), " and ",
              "standard deviation ", \mmlm([round(0)], [Sigma, "."]),
              "A table of the standard ",
              "Normal distribution is found below."
            ])
        ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "How many realizations are ",
                  "below ", \mmlm([round(0)], [X, "?"])
              ]))
           ]),

           form([class(form), method('POST'), action('#ztrans-tratio')],
              [ div(class("input-group mb-3"),
                  [ div(class("input-group-prepend"), 
                      span(class("input-group-text"), "Response")),
                    input([class("form-control"), type(text), name(resp), value(R)]),
                      div(class("input-group-append"),
                        button([class('btn btn-primary'), type(submit)], "Submit"))
              ])])
          ]))
      ]).

% Correctly identify as a paired t-test
:- multifile expert/5.
expert(ztrans: ztrans_pnorm, From >> To, _Flags, Feed, Hint) :-
    From = ztrans_pnorm(X, Mu, Sigma, Sigma2),
    To   = ztrans_pnorm_2(X, Mu, Sigma, Sigma2),
    Feed = "Correctly identified the problem as a Normal distribution.",
    Hint = "The task asks for the Normal distribution.".

intermediate(ztrans: ztrans_pnorm_2/4).

% z-standardization
expert(ztrans: ztrans_z, From >> To, Flags, Feed, Hint) :-
    From = ztrans_pnorm_2(X, Mu, Sigma, _Sigma2),
    Z    = dfrac(X - Mu, Sigma),
    To   = ztrans_pnorm(dec(Z, 1)),
    Feed = [ "Correctly standardized the score." ],
    Hint = [ "The expression for the standardized score is ", 
             \nowrap([\mml(Flags, dfrac(X - Mu, Sigma)), "."]) 
           ].

intermediate(ztrans: ztrans_pnorm/1).

% Use variance instead of sd
:- multifile buggy/5.
buggy(ztrans: ztrans_z, From >> To, Flags, Feed, Trap) :-
    From = ztrans_pnorm_2(X, Mu, Sigma, Sigma2),
    Z    = dfrac(X - Mu, instead_of(ztrans_z, Sigma2, Sigma)),
    To   = ztrans_pnorm(dec(Z, 1)),
    Feed = [ "Please use the SD for ",
             "the ", \nowrap([\mml(Flags, z), "-standardization"]), " instead ",
             "of the variance."
           ],
    Trap = Feed.

% Lower tail
expert(ztrans: ztrans_lower, From >> To, _Flags, Feed, Hint) :-
    From = ztrans_pnorm(Z),
    ZZ   = denoting(z, Z, "the standardized score"),
    To   = perc(pnorm(ZZ)),
    Feed = [ "Correctly determined the lower tail of the standard Normal ",
             "distribution."
           ],
    Hint = "Determine the lower tail of the standard Normal distribution.".

% Upper tail
buggy(ztrans: ztrans_lower, From >> To, _Flags, Feed, Trap) :-
    From = ztrans_pnorm(Z),
    ZZ   = denoting(z, Z, "the standardized score"),
    To   = perc(instead_of(ztrans_lower, unorm(ZZ), pnorm(ZZ))),
    Feed = [ "The upper tail of the standard Normal distribution was reported ",
             "instead of the lower tail."
           ],
    Trap = "Avoid to report the upper tail instead of the lower tail.".

% Forget Normal distribution
buggy(ztrans: ztrans_norm, From >> To, Flags, Feed, Trap) :-
    From = ztrans_pnorm(Z),
    To   = zratio(Z),
    Feed = [ "The ", \nowrap([\mml(Flags, z), "-value"]), " was reported ",
             "instead of the lower tail of the standard Normal distribution."
           ],
    Trap = "Avoid omitting the application of the Normal distribution.".

:- multifile r_init/1.
r_init(ztrans) :-
    r_init,
    {|r||
        mu     = round(100)
        sigma2 = round(225)
        sigma  = round(sqrt(sigma2))
        x      = round(80)
    |}.

%
% Invoke example
%
:- multifile example/0.
example :-
    Topic = ztrans,
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
    Topic = ztrans,
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
    Topic = ztrans,
    html(\item(Topic, ''), HTML, []),
    print_html(HTML).

example :-
    Topic = ztrans,
    buggies(Topic, Bugs),
    writeln(bugs: Bugs).

