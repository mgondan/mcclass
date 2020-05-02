% swipl server.pl --port=8001 --pidfile=http.pid

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- consult(tpaired).
:- use_module(library(quantity)).

:- initialization http_daemon.

% Cache expensive results at beginning of session
:- http_set_session_options([timeout(30), gc(active)]).
:- listen(http_session(begin(SessionId, _Peer)), begin_session(SessionId)).

begin_session(SessionId) :-
    http_log("Session started: ~k~n", [SessionId]),
    thread_create(work(SessionId), _).

realwork(Id, SessionId) :-
    http_log("Starting the real work for ~k~n", [Id]),
    sleep(5),
    item(Item),
    solution(Item, Solution, Path),
    r_init(Id, SessionId),
    r(Solution, Result),
    http_session_assert(solution(Item, Solution, Path, Result), SessionId),
    praise([], Path, Praise, _),
    http_session_assert(praise(Item, Praise), SessionId),
    hints(Path, Hints, Code_Hints),
    http_session_assert(hints(Item, Hints, Code_Hints), SessionId),
    traps(Path, Traps, Code_Traps),
    http_session_assert(traps(Item, Traps, Code_Traps), SessionId),
    wrongs_paths_results(Item, Wrongs_Paths_Results),
    maplist({SessionId}/[X] >> http_session_assert(X, SessionId), Wrongs_Paths_Results),
    http_log("Done with the real work.~n", []).

work(SessionId) :-
    thread_self(ThreadId),
    http_session_assert(ready(ThreadId), SessionId),
    http_log("Thread ~k for ~k started in Session ~k, ready~n", [ThreadId, Id, SessionId]),
    catch(thread_get_message(id(Id)), abort, abort(SessionId)),
    http_session_retract(ready(ThreadId), SessionId),
    http_session_assert(busy(ThreadId), SessionId),
    http_log("Thread ~k for ~k in Session ~k, busy~n", [ThreadId, Id, SessionId]),
    catch(realwork(Id, SessionId), abort, abort(SessionId)),
    http_log("Thread ~k for ~k in Session ~k, done~n", [ThreadId, Id, SessionId]),
    http_session_retract(busy(ThreadId), SessionId),
    http_session_assert(done, SessionId).

abort(SessionId) :-
    thread_self(ThreadId),
    http_log("Thread ~k killed in Session ~k~n", [ThreadId, SessionId]),
    fail.

% Session cleanup
:- listen(http_session(end(SessionId, _Peer)), end_session(SessionId)).

end_session(SessionId) :-
    http_log("Session stopped: ~k~n", [SessionId]),
    http_session_data(ready(ThreadId), SessionId),
    thread_signal(ThreadId, throw(abort)).

end_session(SessionId) :-
    http_log("Session stopped: ~k~n", [SessionId]),
    http_session_data(busy(ThreadId), SessionId),
    thread_signal(ThreadId, throw(abort)).

ready(ThreadId) :-
    http_session_data(ready(ThreadId)).

busy :-
    http_session_data(busy(_)).

done :-
    http_session_data(done).

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(mcclass, root(mcclass), []).

:- http_handler(root('favicon.ico'), http_reply_file('favicon.ico', []), []).
:- http_handler(mcclass(tpaired), handler(tpaired), []).
:- http_handler(mcclass(pvalue), handler(pvalue), []).
:- http_handler(mcclass(.), http_redirect(see_other, mcclass(tpaired)), []).
:- http_handler(root(.), http_redirect(see_other, mcclass(.)), []).

handler(Id, Request) :-
    member(method(post), Request),
    init_session(Id),
    http_parameters(Request,
      [ download(Download, [optional(true)]), 
        help(Help, [optional(true)]),
        response(Response, [optional(true)]) 
      ]),
    post(Id, [download(Download), help(Help), response(Response)]).

handler(Id, _Request) :-
    init_session(Id),
    page(Id).

init_session(_Id) :-
    done.

init_session(_Id) :-
    busy.

init_session(Id) :-
    ready(ThreadId),
    http_log("Contacting thread ~k.~n", [ThreadId]),
    thread_send_message(ThreadId, id(Id)).

% Download data
post(Id, Request) :-
    option(download(Download), Request),
    ground(Download),
    http_in_session(SessionId),
    data(Id, Temp, SessionId),
    http_reply_file(Temp, [ unsafe(true), mime_type(text/csv),
	headers(['Content-Disposition'('attachment ; filename="data.csv"')])], 
        Request).

% Ask for help
post(Id, Request) :-
    option(help(Help), Request),
    ground(Help),
    hint_increase,
    page(Id).

% Evaluate response
post(Id, Request) :-
    option(response(Response), Request),
    ground(Response),
    responded(Response),
    page(Id).

page(Id) :-
    http_in_session(SessionId),
    r_init(Id, SessionId),
    item(Item),
    response(Response),
    reply_html_page(
      [ title('McClass'),
        link(
	  [ rel(stylesheet),
	    href('https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css'),
	    integrity('sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh'),
	    crossorigin(anonymous)
	  ]),
	meta([name(viewport), content('width=device-width, initial-scale=1')]),
	\refresh(3)
      ],
      [ \item(Item, Response),
        \help(Id),
        \feedback(Id, Response),
	\wrongs(Id),
	\avoid(Id),
        \navigation(Id, [1-tpaired, 2-pvalue])
      ]).

refresh(_) -->
    { done },
    html([]).

refresh(Sec) -->
    html(meta(['HTTP-Equiv'(refresh), content(Sec)])).

hint_level(Hint) :-
    http_session_data(hint(H))
     -> Hint = H
      ; Hint = 0.

hint_increase :-
    hint_level(Hint),
    H is Hint + 1,
    http_session_asserta(hint(H)).

response(Response) :-
    http_session_data(response(Response)),
    !.

response('').

responded(Response) :-
    http_session_retractall(response(_)),
    http_session_assert(response(Response)).

feedback(_Id, '') -->
    html(div(class(card),
      [ div(class('card-header alert-information'), "Feedback"),
        div(class('card-body'), 
            p(class('card-text'), "Waiting for response..."))
      ])).

feedback(_Id, _Response) -->
    { busy ; ready(_) },
    html(div(class(card),
      [ div(class('card-header alert-warning'), "System is busy"),
        div(class('card-body'),
            p(class('card-text'), "Please wait..."))
      ])).

feedback(_Id, Response) -->
    { done,
      quantity(_, _, Response),
      item(Item),
      http_session_data(solution(Item, Solution, _, Result)),
      match(Result, Response, Format),
      http_session_data(praise(Item, Praise))
    },
    html(div(class(card),
      [ div(class('card-header alert-success'), "Correct result"),
        div(class('card-body'), 
	  [ \ul_nonempty(\mml(Solution = Result), Praise),
            \ul_nonempty("Additional hints", Format)
	  ])
      ])).

feedback(_Id, Response) -->
    { done,
      quantity(_, _, Response),
      item(Item),
      http_session_data(wrong(Item, Wrong, Woodden, Result)),
      match(Result, Response, Format),
      praise(Flags, Woodden, _, Code_Praise),
      http_session_data(hints(Item, _, Code_Hints)),
      relevant(Code_Praise, Code_Hints, RelPraise, IrrelPraise),
      palette(Wrong, Flags),
      mistakes(Flags, Woodden, _, Code_Mistakes),
      http_session_data(traps(Item, _, Code_Traps)),
      relevant(Code_Mistakes, Code_Traps, RelMistake, IrrelMistake),
      append([IrrelPraise, IrrelMistake, Format], Additional)
    },
    html(div(class(card),
      [ div(class('card-header alert-warning'), "Incorrect result"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the formula for the correct result:"),
	    p(class('card-text'), \mml([fix(all) | Flags], Wrong)),
	    p(class('card-text'), "Your result matches the following expression:"),
	    p(class('card-text'), \mml([show(all) | Flags], Wrong)),
	    \ul_nonempty(em("Correct steps"), RelPraise),
	    \ul_nonempty(em("Mistakes"), RelMistake),
	    \ul_nonempty(em("Additional hints"), Additional)
          ])
      ])).

feedback(_Id, _Response) -->
    html(div(class(card),
      [ div(class('card-header alert-danger'), "Incorrect result"),
        div(class('card-body'),
          p(class('card-text'), 
	    [ "Your response is incorrect. It does not match any known mistake, ",
	      "so I cannot provide useful feedback."
	    ]))
      ])).

help(_Id) -->
    { hint_level(0) }, 
    !,
    html(div(class(card),
      [ div(class('card-header alert-information'), "Hints"),
        div(class('card-body'),
            p(class('card-text'), "Press \"help me\" to request a hint to the solution."))
      ])).

help(_Id) -->
    { hint_level(Level),
      item(Item),
      http_session_data(hints(Item, Hints, _)),
      findall(H, (nth1(Index, Hints, H), Index =< Level), List)
    }, 
    html(div(class(card),
      [ div(class('card-header alert-info'), "Hints"),
        div(class('card-body'), 
	    \ul_nonempty("Steps to the solution", List))
      ])).

avoid(_Id) -->
    { item(Item),
      http_session_data(traps(Item, Traps, _))
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
	div(class('card-body'), \ul_nonempty("Avoid these traps:", Traps)) 
      ])).

wrongs(_Id) -->
    { item(Item),
      http_session_data(solution(Item, Solution, _, Result)),
      mathml(Solution = number(Result), Correct),
      findall(W = number(R), http_session_data(wrong(Item, W, _, R)), Wrong),
      maplist(mathml, Wrong, Wrongs)
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
        div(class('card-body'),
            \ul_nonempty("The following results are recognized by the system:", [Correct | Wrongs]))
      ])).

