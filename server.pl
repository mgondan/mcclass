% swipl server.pl --port=8001 --workers=10 --pidfile=http.pid

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(tasks).
:- use_module(r_session).
:- use_module(search).
:- use_module(feedback).
:- use_module(mathml).
:- use_module(session).
:- use_module(navbar).
:- use_module(login).
:- use_module(users).
:- use_module(table).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_wrapper)).

:- set_prolog_flag(backtrace_goal_depth, 10).
:- debug.

:- dynamic http:location/3.
http:location(mcclass, root(mcclass), []).

:- http_handler(root('favicon.ico'), http_reply_file('favicon.ico', []), []).
:- http_handler(mcclass('favicon.ico'), http_reply_file('favicon.ico', []), []).
:- http_handler(root('bootstrap.min.css'), http_reply_file('css/bootstrap.min.css', []), []).
:- http_handler(mcclass('bootstrap.min.css'), http_reply_file('css/bootstrap.min.css', []), []).
:- http_handler(root('bootstrap.min.css.map'), http_reply_file('css/bootstrap.min.css.map', []), []).
:- http_handler(mcclass('bootstrap.min.css.map'), http_reply_file('css/bootstrap.min.css.map', []), []).
:- http_handler(root('bootstrap.bundle.min.js'), http_reply_file('js/bootstrap.bundle.min.js', []), []).
:- http_handler(mcclass('bootstrap.bundle.min.js'), http_reply_file('js/bootstrap.bundle.min.js', []), []).
:- http_handler(mcclass(.), http_redirect(see_other, mcclass(tpaired)), []).
:- http_handler(root(.), http_redirect(see_other, mcclass(.)), []).

:- http_handler(mcclass(tpaired), handler(tpaired), []).
:- http_handler(mcclass(tpaired1t), handler(tpaired1t), []).
:- http_handler(mcclass(tpaired1tlow), handler(tpaired1tlow), []).
:- http_handler(mcclass(baseline), handler(baseline), []).
:- http_handler(mcclass(oddsratio), handler(oddsratio), []).
:- http_handler(mcclass(oddsratio2), handler(oddsratio2), []).
:- http_handler(mcclass(easyodds), handler(easyodds), []).
:- http_handler(mcclass(subgroups), handler(subgroups), []).
:- http_handler(mcclass(ztrans), handler(ztrans), []).
:- http_handler(mcclass(ztrans2), handler(ztrans2), []).
:- http_handler(mcclass(tgroups), handler(tgroups), []).
:- http_handler(mcclass(tgroupsdf), handler(tgroupsdf), []).
:- http_handler(mcclass(dbinom), handler(dbinom), []).
:- http_handler(mcclass(qbinom), handler(qbinom), []).
:- http_handler(mcclass(chisq), handler(chisq), []).
:- http_handler(mcclass(power), handler(power), []).
:- http_handler(mcclass(cigroups), handler(cigroups), []).
:- http_handler(mcclass(mathmltest), handler(mathmltest), []).
:- http_handler(mcclass(linreg), handler(linreg), []).

:- http_handler(mcclass(debug), handler(debug), []).


handler(Topic, Request) :-
    member(method(post), Request),
    !,
    http_parameters(Request, [], [form_data(Form)]),
    handle(Topic, Form).

handler(Topic, _) :-
    handle(Topic, []).

% Download csv data
handle(Topic, Form),
    member(download=_, Form)
 => r_init_session,
    b_setval(topic, Topic),
    download(Local),
    format(atom(File), "attachment; filename=~k.csv", [Topic]),
    http_current_request(Request),
    http_reply_file(Local,
      [ unsafe(true),
        mime_type(text/csv), headers(['Content-Disposition'(File)])
      ], Request).

% Response given
handle(Topic, Form),
    option(task(Task), Form),
    option(resp(Resp), Form)
 => session_retractall(resp(Topic, Task, _)),
    session_assert(resp(Topic, Task, Resp)),
    r_init_session,
    r_session_source(Topic),
    b_setval(topic, Topic),
    findall(T, Topic:task(T), [T1 | Tasks]),
    reply_html_page(
      [ title('McClass'),
        link([rel(stylesheet), href('bootstrap.min.css')]),
        link([rel(icon), href('favicon.ico'), type('image/x-icon')]),
        meta([name(viewport), content('width=device-width, initial-scale=1')]),
        style(
          [ ".table thead tr th { padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table thead tr:first-child { border-top: 2px solid black; border-bottom: 1px solid black; }",
            ".table tbody tr td { border: none; padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table tbody tr th { border: none; padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table tbody tr:last-child { border-bottom: 2px solid black; }"
          ])
      ],
      [ \navbar,
        \(Topic:render),
        \navtabs([T1 | Tasks], Task),
        div([class('tab-content'), id('nav-tabContent')],
          \foreach((member(T, [T1 | Tasks]), task(Topic, T, task(Topic, T, Data))),
            ( {T = Task}
            ->  html(div([class('tab-pane fade show active'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task(T)),
                    \feedback(Topic, T, Data, Form),
                    \pp_solutions(Topic, T, Data),
                    \pp_hints(Topic, T, Data),
                    \pp_wrongs(Topic, T, Data),
                    \pp_traps(Topic, T, Data)])))
            ;   html(div([class('tab-pane fade'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task(T)),
                    \feedback(Topic, T, Data, Form),
                    \pp_solutions(Topic, T, Data),
                    \pp_hints(Topic, T, Data),
                    \pp_wrongs(Topic, T, Data),
                    \pp_traps(Topic, T, Data)])))))),
        script(src('bootstrap.bundle.min.js'), '')
      ]).

% No response
handle(Topic, Form)
 => findall(T, Topic:task(T), [T1 | Tasks]),
    option(task(Task), Form, T1),
    r_init_session,
    r_session_source(Topic),
    b_setval(topic, Topic),
    reply_html_page(
      [ title('McClass'),
        link([rel(stylesheet), href('bootstrap.min.css')]),
        link([rel(icon), href('favicon.ico'), type('image/x-icon')]),
        meta([name(viewport), content('width=device-width, initial-scale=1')]),
        style(
          [ ".table thead tr th { padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table thead tr:first-child { border-top: 2px solid black; border-bottom: 1px solid black; }",
            ".table tbody tr td { border: none; padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table tbody tr th { border: none; padding-top: 0.1rem; padding-bottom: 0.1rem; }",
            ".table tbody tr:last-child { border-bottom: 2px solid black; }"
          ])
      ],
      [ \navbar,
        \(Topic:render),
        \navtabs([T1 | Tasks], Task),
        div([class('tab-content'), id('nav-tabContent')],
          \foreach((member(T, [T1 | Tasks]), task(Topic, T, task(Topic, T, Data))),
            ( {T = Task}
            ->  html(div([class('tab-pane fade show active'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)], 
                  div([
                    \(Topic:task(T)),
                    \feedback(Topic, T, Data, Form),
                    \pp_solutions(Topic, T, Data),
                    \pp_hints(Topic, T, Data),
                    \pp_wrongs(Topic, T, Data),
                    \pp_traps(Topic, T, Data)])))
            ;   html(div([class('tab-pane fade'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task(T)),
                    \feedback(Topic, T, Data, Form),
                    \pp_solutions(Topic, T, Data),
                    \pp_hints(Topic, T, Data),
                    \pp_wrongs(Topic, T, Data),
                    \pp_traps(Topic, T, Data)])))))),
        script(src('bootstrap.bundle.min.js'), '')
      ]).

