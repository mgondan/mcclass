:- module(tasks, [task/3, feedback//4, solutions/3, download/1 ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_log)).
:- use_module(library(dcg/high_order)).
:- use_module(library(broadcast)).
:- use_module(mathml).
:- use_module(search).
:- use_module(depends).
:- use_module(steps).
:- use_module(r_session).
:- use_module(interval).
:- use_module(session).
:- use_module(library(quantity)).
:- use_module(hints).
:- use_module(solutions).
:- use_module(mistakes).
:- use_module(traps).
:- use_module(util).

user:term_expansion(mono(A, B), rint:mono(A, B)).
user:term_expansion(r_hook(A), rint:r_hook(r_session:r_topic, A)).

% Solutions with numerical results
solutions(Topic, Task, Solutions) :-
    findall(s(Expr, Res-Codes, Flags, Colors, String),
      ( Topic:sol(Task, Expr, Flags, Colors, String),
        interval(Expr, Res, [topic(Topic)]),
        sort(Flags, Sorted),
        codes(Sorted, Codes)
      ), List),
    % avoid duplicates by permutations
    sort(2, @<, List, Unique),
    findall(sol(Expr, Res, Flags, Colors, String),
      member(s(Expr, Res-_, Flags, Colors, String), Unique), Solutions).

% Incorrect results
mistakes(Topic, Task, Solutions) :-
    findall(s(Expr, Res-Codes, Flags, Colors, String),
      ( Topic:wrong(Task, Expr, Flags, Colors, String),
        interval(Expr, Res, [topic(Topic)]),
        sort(Flags, Sorted),
        dependencies(Sorted),
        exclusive(Sorted),
        codes(Sorted, Codes)
      ), List),
    % avoid duplicates by permutations
    sort(2, @<, List, Unique),
    findall(wrong(Expr, Res, Flags, Colors, String),
      member(s(Expr, Res-_, Flags, Colors, String), Unique), Solutions).

init_variants(Topic) :-
    Topic:task(Task),
    init_variant(Topic, Task).

:- dynamic taskdata/4.

init_variant(Topic, Task) :-
    findall(V, taskdata(Topic, Task, V, _), Variants),
    length(Variants, N),
    format(atom(Variant), "var~w", N),
    b_setval(topic, Topic),
    b_setval(variant, Variant),
    r_topic_source,
    solutions(Topic, Task, S),
    mistakes(Topic, Task, M),
    assert(taskdata(Topic, Task, Variant, [solutions(S), mistakes(M)])).

init_topic(Topic) :-
    use_module(Topic),
    dynamic(Topic:math_hook/2),
    init_solutions(Topic),
    init_hints(Topic),
    init_mistakes(Topic),
    init_traps(Topic),
    forall(init_variants(Topic), true).

init_topics :-
    init_topic(tpaired),
    init_topic(tpairedupper),
    init_topic(tpairedlower),
    init_topic(baseline),
    init_topic(oddsratio),
    init_topic(oddsratio2),
    init_topic(easyodds),
    init_topic(tgroups),
    init_topic(ztrans),
    init_topic(dbinom),
    init_topic(testbinom),
    init_topic(chisq),
    init_topic(subgroups),
    init_topic(regression).

:- init_topics.
:- listen(http(post_server_start), (between(1, 3, _), init_topics)).

% Render R result
mathml:math_hook(r(Expr), Res) :-
    r_topic(Expr, Res).

:- dynamic taskdata/4.

% more to come
task(Topic, Task, Data) :-
    session_data(topic(Topic, Variant)),
    taskdata(Topic, Task, Variant, D),
    !, Data = D.

task(Topic, Task, Data) :-
    findall(V, taskdata(Topic, Task, V, _), Variants),
    length(Variants, N),
    random_between(1, N, Index),
    nth1(Index, Variants, Variant),
    session_assert(topic(Topic, Variant)),
    !,
    taskdata(Topic, Task, Variant, Data).

% Correct response
feedback(Topic, Task, Data, _Form)
--> { % option(resp(R), Form),
      session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(solutions(Solutions), Data),
      member(sol(_Expr, Res, Flags, Colors, _S), Solutions),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Colors]),
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
        ), Items)
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-success'), "Congratulations"),
        div(class('card-body'),
          [ p(class('card-text'), "Correct response!"),
            ul(Items)
          ])
      ])).

% Buggy response
feedback(Topic, Task, Data, _Form)
--> { % option(resp(R), Form),
      session_data(resp(Topic, Task, R)),
      quantity(N0, Opt, R),
      interval(input(N0), Num, [topic(Topic), task(Task) | Opt]),
      memberchk(mistakes(Mistakes), Data),
      member(wrong(Expr, Res, Flags, Colors, _S), Mistakes),
      interval(Num =@= Res, true, [topic(Topic), task(Task) | Colors]),
      % This can be prepared on module initialization
      findall(H, Topic:hints(Task, _, H, _), Hints0),
      append(Hints0, Hints1),
      sort(Hints1, Hints),
      % Relevant feedback
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          memberchk(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task) | Colors], F)
        ), Correct0),
      ( Correct0 = []
        -> Correct = p(class('card-text'), "")
         ; Correct = p(class('card-text'), 
                       [ "Correct steps", ul(class('card-text'), ul(Correct0)) ])
      ),
      findall(li(F),
        ( member(step(buggy, Name, Args), Flags),
          Topic:trap(Task, _, Name, _),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Wrong0),
      ( Wrong0 = []
        -> Wrong = p(class('card-text'), "")
         ; Wrong = p(class('card-text'),
                       [ "Wrong steps", ul(class('card-text'), ul(Wrong0)) ])
      ),
      % Irrelevant feedback
      findall(li(F),
        ( member(step(expert, Name, Args), Flags),
          \+ memberchk(Name, Hints),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Praise0),
      ( Praise0 = []
        -> Praise = p(class('card-text'), "")
         ; Praise = p(class('card-text'),
                       [ "Other praise", ul(class('card-text'), ul(Praise0)) ])
      ),
      findall(li(F),
        ( member(step(buggy, Name, Args), Flags),
          \+ Topic:trap(Task, _, Name, _),
          Topic:feedback(Name, Args, [topic(Topic), task(Task), denote(false) | Colors], F)
        ), Blame0),
      ( Blame0 = []
        -> Blame = p(class('card-text'), "")
         ; Blame = p(class('card-text'),
                       [ "Other mistakes", ul(class('card-text'), ul(Blame0)) ])
      )
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-warning'), "Careful"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the correct expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(fix) | Colors], Expr)),
            p(class('card-text'), "Your response matches the following expression:"),
            p(class('card-text'), \mmlm([topic(Topic), task(Task), error(highlight) | Colors], Expr)),
            Correct, Wrong, Praise, Blame
          ])
      ])).

feedback(Topic, Task, _Data, _Form) -->
  { % option(resp(R), Form),
    session_data(resp(Topic, Task, R)),
    quantity(N, _Opt, R)
  },
  html(div(class(card),
    [ div(class('card-header text-white bg-warning'), "Careful"),
      div(class('card-body'),
        p(class('card-text'), "The response ~p is not correct and cannot be attributed to any known mistake."-[N]))
    ])).

feedback(Topic, Task, _Data, _Form) -->
    { % option(resp(R), Form),
      session_data(resp(Topic, Task, R))
    },
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Response not recognized: ~p"-[R]))
      ])).

feedback(_Topic, _Task, _Data, _Form) -->
    html(div(class(card),
      [ div(class('card-header text-white bg-secondary'), "Feedback"),
        div(class('card-body'),
          p(class('card-text'), "Waiting for response..."))
      ])).

% Download task data
download(File) :-
    session_tmpfile(File),
    r_topic(download(File)).

%
% Run example outside of webserver
%
% $ swipl
% ?- [tasks].
% ?- trace.   (if needed)
% ?- tasks:tasks.
%
tasks :-
    tasks(baseline, pvalue).

tasks(Topic, Task) :-
    b_setval(http_session_id, default_session),
    b_setval(topic, Topic),
    b_setval(variant, var1),
    task(Topic, Task, Data),
    writeln("Task data"),
    writeln(Data),
    writeln("Task"),
    html(\(Topic:render([topic(Topic)])), Item, []),
    writeln("Task as HTML"),
    writeln(Item),
    memberchk(solutions(S), Data), 
    writeln("Solutions"),
    writeln(S),
    html(\show_solutions(Topic, Task, Data), Sol, []),
    writeln(Sol),
    memberchk(mistakes(W), Data), 
    length(W, L), 
    format("Wrong alternatives: ~w~n", [L]),
    html(\show_mistakes(Topic, Task, Data), Wrong, []),
    writeln(Wrong),
    findall(Name, Topic:trap(Task, _, Name, _), T),
    format("Traps: ~w~n", [T]),
    html(\show_traps(Topic, Task), Traps, []),
    writeln(Traps).
