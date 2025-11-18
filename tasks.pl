:- module(tasks, [task/3, solutions/3, download/1 ]).

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
:- use_module(feedback).


% Solutions with numerical results
solutions(Topic, Task, Solutions) :-
    findall(s(Expr, Res-Codes, Flags, Colors, String),
      ( Topic:sol(Task, Expr, Flags, Colors, String),
        interval(Expr, Res, [topic(Topic)]),
        list_to_set(Flags, Sorted),
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
        list_to_set(Flags, Sorted),
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
    b_setval(variant, Variant),
    r_topic_source_with_check,
    solutions(Topic, Task, S),
    mistakes(Topic, Task, M),
    assert(taskdata(Topic, Task, Variant, [solutions(S), mistakes(M)])).

init_topic(Topic) :-
    use_module(Topic),
    assert_clauses(Topic),
    dynamic(Topic:math_hook/2),
    b_setval(topic, Topic),
    init_solutions(Topic),
    init_hints(Topic),
    init_mistakes(Topic),
    init_traps(Topic),
    forall(init_variants(Topic), true).

init_topics :-
    init_topic(tpaired),
    init_topic(tpairedupper),
    init_topic(tpairedlower),
    init_topic(tgroups),
    init_topic(oddsratio),
    init_topic(ztrans),
    init_topic(dbinom),
    init_topic(sequence),
    init_topic(testbinom),
    init_topic(chisq),
    init_topic(baseline),
    init_topic(subgroups),
    init_topic(regression).

done_topics :-
    http_log("Done with initialization.~n").

:-  init_topics.
:-  listen(http(post_server_start), 
    thread_create((init_topics, init_topics, init_topics), _, [at_exit(done_topics)])).

% Render R result
mathml:math_hook(r(Expr), Res) :-
    r_topic(Expr, Res).

% more to come
task(Topic, Task, Data) :-
    session_data(topic(Topic, Variant)),
    !,
    taskdata(Topic, Task, Variant, Data).

task(Topic, Task, Data) :-
    findall(V, taskdata(Topic, Task, V, _), Variants),
    length(Variants, N),
    random_between(1, N, Index),
    nth1(Index, Variants, Variant),
    session_assert(topic(Topic, Variant)),
    !,
    taskdata(Topic, Task, Variant, Data).

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
    tasks(regression, bcoef).

tasks(Topic, Task) :-
    b_setval(http_session_id, default_session),
    b_setval(topic, Topic),
    b_setval(variant, var0),
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
