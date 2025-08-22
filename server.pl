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
:- use_module(hints).
:- use_module(solutions).
:- use_module(mistakes).
:- use_module(traps).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_unix_daemon)).

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
:- http_handler(mcclass(tpairedupper), handler(tpairedupper), []).
:- http_handler(mcclass(tpairedlower), handler(tpairedlower), []).
:- http_handler(mcclass(baseline), handler(baseline), []).
:- http_handler(mcclass(oddsratio), handler(oddsratio), []).
:- http_handler(mcclass(oddsratio2), handler(oddsratio2), []).
:- http_handler(mcclass(easyodds), handler(easyodds), []).
:- http_handler(mcclass(subgroups), handler(subgroups), []).
:- http_handler(mcclass(ztrans), handler(ztrans), []).
:- http_handler(mcclass(tgroups), handler(tgroups), []).
:- http_handler(mcclass(tgroupsdf), handler(tgroupsdf), []).
:- http_handler(mcclass(dbinom), handler(dbinom), []).
:- http_handler(mcclass(testbinom), handler(testbinom), []).
:- http_handler(mcclass(chisq), handler(chisq), []).
:- http_handler(mcclass(regression), handler(regression), []).

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
 => b_setval(topic, Topic),
    session_data(topic(Topic, Variant)),
    b_setval(variant, Variant),
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
    task(Topic, Task, _Data),
    http_session_data(topic(Topic, Variant)),
    b_setval(topic, Topic),
    b_setval(variant, Variant),
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
            ".table tbody tr:last-child { border-bottom: 2px solid black; }",
            ".accordion-button.hint { --bs-accordion-active-bg: var(--bs-warning-bg-subtle);
               --bs-accordion-active-color: var(--bs-warning-text-emphasis); }"
          ])
      ],
      [ \navbar,
        \(Topic:render([topic(Topic)])),
        \navtabs(Topic, [T1 | Tasks], Task),
        div([class('tab-content'), id('nav-tabContent')],
          \foreach((member(T, [T1 | Tasks]), task(Topic, T, Data)),
            ( {T = Task}
            ->  html(div([class('tab-pane fade show active'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task([topic(Topic)], T)),
		    \show_hints(Topic, T),
                    \show_feedback(Topic, T, Data),
                    \show_solutions(Topic, T, Data),
                    \show_mistakes(Topic, T, Data),
                    \show_traps(Topic, T)])))
            ;   html(div([class('tab-pane fade'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task([topic(Topic)], T)),
		    \show_hints(Topic, T),
                    \show_feedback(Topic, T, Data),
                    \show_solutions(Topic, T, Data),
                    \show_mistakes(Topic, T, Data),
                    \show_traps(Topic, T)])))))),
        script(src('bootstrap.bundle.min.js'), '')
      ]).

% No response
handle(Topic, Form)
 => findall(T, Topic:task(T), [T1 | Tasks]),
    option(task(Task), Form, T1),
    task(Topic, Task, _Data),
    http_session_data(topic(Topic, Variant)),
    b_setval(topic, Topic),
    b_setval(variant, Variant),
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
            ".table tbody tr:last-child { border-bottom: 2px solid black; }",
            ".accordion-button.hint { --bs-accordion-active-bg: var(--bs-warning-bg-subtle);
               --bs-accordion-active-color: var(--bs-warning-text-emphasis); }",
            ".accordion-button.mistake { --bs-accordion-active-bg: var(--bs-danger-bg-subtle);
               --bs-accordion-active-color: var(--bs-danger-text-emphasis); }"
          ])
      ],
      [ \navbar,
        \(Topic:render([topic(Topic)])),
	\navtabs(Topic, [T1 | Tasks], Task),
        div([class('tab-content'), id('nav-tabContent')],
	  \foreach((member(T, [T1 | Tasks]), task(Topic, T, Data)),
            ( {T = Task}
             -> html(div([class('tab-pane fade show active'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)], 
                  div([
                    \(Topic:task([topic(Topic)], T)),
		    \show_hints(Topic, T),
                    \show_feedback(Topic, T, Data),
                    \show_solutions(Topic, T, Data),
                    \show_mistakes(Topic, T, Data),
                    \show_traps(Topic, T)])))
            ;   html(div([class('tab-pane fade'), id('nav-~w'-[T]), role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[T]), tabindex(0)],
                  div([
                    \(Topic:task([topic(Topic)], T)),
		    \show_hints(Topic, T),
                    \show_feedback(Topic, T, Data),
                    \show_solutions(Topic, T, Data),
                    \show_mistakes(Topic, T, Data),
                    \show_traps(Topic, T)])))))),
        script(src('bootstrap.bundle.min.js'), '')
      ]).
