% swipl server.pl --port=8001 --workers=10 --pidfile=http.pid

:- use_module(library(dcg/basics)).
:- use_module(tasks).
:- use_module(r).
:- use_module(search).
:- use_module(feedback).
:- use_module(mathml).
:- use_module(session).

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
:- initialization http_daemon.

:- dynamic http:location/3.
http:location(mcclass, root(mcclass), []).

:- http_handler(root('favicon.ico'), http_reply_file('favicon.ico', []), []).
:- http_handler(mcclass(.), http_redirect(see_other, mcclass(tpaired)), []).
:- http_handler(root(.), http_redirect(see_other, mcclass(.)), []).

:- http_handler(mcclass(tpaired), handler(tpaired), []).
:- http_handler(mcclass(oddsratio), handler(oddsratio), []).
:- http_handler(mcclass(oddsratio2), handler(oddsratio2), []).
:- http_handler(mcclass(easyodds), handler(easyodds), []).
:- http_handler(mcclass(ztrans), handler(ztrans), []).
:- http_handler(mcclass(ztrans2), handler(ztrans2), []).
:- http_handler(mcclass(tgroups), handler(tgroups), []).
:- http_handler(mcclass(tgroups2), handler(tgroups2), []).
:- http_handler(mcclass(tgroupsdf), handler(tgroupsdf), []).
:- http_handler(mcclass(dbinom), handler(dbinom), []).
:- http_handler(mcclass(qbinom), handler(qbinom), []).
:- http_handler(mcclass(chisq), handler(chisq), []).
:- http_handler(mcclass(power), handler(power), []).

handler(Task, Request) :-
    member(method(post), Request),
    !,
    http_parameters(Request, [], [form_data(Form)]),
    handle(Task, Form).

handler(Task, _) :-
    handle(Task, []).

% Download csv data
handle(Task, Form) :-
    member(download=_, Form),
    task(Task, _TaskData),
    download(Task, Local),
    http_log("Local=~w~n", [Local]),
    format(atom(File), "attachment; filename=~k.csv", [Task]),
    http_current_request(Request),
    http_reply_file(Local,
      [ unsafe(true),
        mime_type(text/csv), headers(['Content-Disposition'(File)])
      ], Request).

% Task sheet
handle(Task, Form) :-
    r_initialize,
    task(Task, TaskData),
    start(Task, Item),
    reply_html_page(
      [ title('McClass'),
        link(
	      [ rel(stylesheet),
	        href('https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css'),
            integrity('sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3'),
            crossorigin(anonymous)
          ]),
	    link(
	      [ rel(icon), 
            href('/mcclass/favicon.ico'),
	        type('image/x-icon')
          ]),
        meta(
	      [ name(viewport), 
            content('width=device-width, initial-scale=1')])
          ],
      [ \render(Task, Item, Form),
        \feedback(Task, Form),
        \solution(TaskData),
        \hints(TaskData),
        \wrongs(TaskData),
        \traps(TaskData)
      ]).

handle(Task, _) :-
    reply_html_page(
      [ title('McClass'),
        link(
      [ rel(stylesheet),
        href('https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css'),
        integrity('sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3'),
        crossorigin(anonymous)]),
    link(
      [ rel(icon),
            href('/mcclass/favicon.ico'),
        type('image/x-icon')]),
        meta(
      [ name(viewport),
            content('width=device-width, initial-scale=1')])
      ],
      p("not found: ~w"-Task)
    ).

