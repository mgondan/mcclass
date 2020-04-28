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

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(images, root(images), []).
http:location(styles, root(styles), []).
http:location(scripts, root(scripts), []).
http:location(mcclass, root(mcclass), []).

:- http_handler(mcclass(tpaired), handler(tpaired), []).
:- http_handler(mcclass(pvalue), handler(pvalue), []).
:- http_handler(mcclass(.), http_redirect(see_other, mcclass(tpaired)), []).
:- http_handler(root(.), http_redirect(see_other, mcclass(.)), []).

:- http_handler(images(.), http_reply_from_files('./images', []), [prefix]).
:- http_handler(styles(.), http_reply_from_files('./styles', []), [prefix]).
:- http_handler(scripts(.), http_reply_from_files('./scripts', []), [prefix]).

% Dispatch requests
handler(Id, Request) :-
    member(method(post), Request),
    http_parameters(Request,
      [ download(Download, [optional(true)]), 
        help(Help, [optional(true)]),
        response(Response, [optional(true)]) 
      ]),
    post(Id, [download(Download), help(Help), response(Response)]).

handler(Id, _Request) :-
    hint_zero,
    post(Id, []).

% Download data
post(Id, Request) :-
    option(download(Download), Request),
    ground(Download),
    data(Id, Temp),
    http_reply_file(Temp, [ unsafe(true), mime_type(text/csv),
	headers(['Content-Disposition'('attachment ; filename="data.csv"')])], 
        Request).

% Ask for help
post(Id, Request) :-
    select_option(help(Help), Request, New),
    ground(Help),
    hint_increase,
    post(Id, New).

% Evaluate response
post(Id, Request) :-
    select_option(response(Response), Request, New),
    ground(Response),
    responded(Response),
    post(Id, New).

post(Id, _) :-
    r_init(Id),
    item(Item),
    solution(Item, Solution, _),
    palette(Solution, Flags),
    response(Response),
    reply_html_page(
      [ title('McClass'),
        link(
	  [ rel(stylesheet),
	    href('https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css'),
	    integrity('sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh'),
	    crossorigin(anonymous)
	  ]),
	meta(
	  [ name(viewport),
	    content('width=device-width, initial-scale=1')
	  ])
      ],
      [ \item(Flags, Item, Response),
        \help(Id),
        \feedback(Id, Response),
       	\avoid(Id),
      	\wrong(Id),
        \navigation(Id, [1-tpaired, 2-pvalue])
      ]).

hint_zero :-
    http_session_retractall(hint(_)).

hint_level(Hint) :-
    ( http_session_data(hint(H))
      -> Hint = H
       ; Hint = 0
    ).

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
    http_session_asserta(response(Response)).

feedback(_Id, '') -->
    html(div(class(card),
      [ div(class('card-header alert-information'), "Feedback"),
        div(class('card-body'), 
            p(class('card-text'), "Waiting for response..."))
      ])).

feedback(_Id, Response) -->
    { quantity(_, _, Response),
      item(Item),
      solution(Item, Solution, Path),
      r(Solution, Result),
      match(Result, Response, Format),
      palette(Solution, Flags),
      praise(Flags, Path, Praise),
      findall(li(L), member(_-L, Praise), PraiseList),
      append(Format, PraiseList, List)
    },
    html(div(class(card),
      [ div(class('card-header alert-success'), "Correct result"),
        div(class('card-body'), 
	    \ul_nonempty(\mml(Flags, Solution = Result), List))
      ])).

feedback(_Id, Response) -->
    { quantity(_, _, Response),
      item(Item),
      solution(Item, Solution, Path),
      route(Item, Wrong, Woodden),
      dif(Wrong, Solution),
      r(Wrong, Result),
      match(Result, Response, Format),
      palette(Wrong, Flags),
      praise(Flags, Woodden, Praise),
      hints(Flags, Path, Hints),
      relevant(Praise, Hints, RelevantPraise, IrrelevantPraise),
      findall(li(L), member(_-L, RelevantPraise), RelPraiseList),
      findall(li(L), member(_-L, IrrelevantPraise), IrrelPraiseList),
      mistakes(Flags, Woodden, Mistakes),
      traps(Path, Traps),
      relevant(Mistakes, Traps, Relevant, Irrelevant),
      findall(li(L), member(_-L, Relevant), RelMistakeList),
      findall(li(L), member(_-L, Irrelevant), IrrelMistakeList),
      findall(li(L), member(L, Format), FormatList),
      append([IrrelPraiseList, IrrelMistakeList, FormatList], Additional)
    },
    html(div(class(card),
      [ div(class('card-header alert-warning'), "Incorrect result"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the formula for the correct result:"),
	    p(class('card-text'), \mml([fix(all) | Flags], Wrong)),
	    p(class('card-text'), "Your result matches the following expression:"),
	    p(class('card-text'), \mml([show(all) | Flags], Wrong)),
	    \ul_nonempty(em("Correct steps"), RelPraiseList),
	    \ul_nonempty(em("Mistakes"), RelMistakeList),
	    \ul_nonempty(em("Additional hints"), Additional)
          ])
      ])).

help(_Id) -->
    { hint_level(0)
    }, !,
    html(div(class(card),
      [ div(class('card-header alert-information'), "Hints"),
        div(class('card-body'),
            p(class('card-text'), "Press \"help me\" to request a hint to the solution."))
      ])).

help(_Id) -->
    { hint_level(Level),
      item(Item),
      solution(Item, Solution, Path),
      palette(Solution, Flags),
      hints(Flags, Path, Hints),
      findall(li(H), (nth1(Index, Hints, _-H), Index =< Level), List)
    }, 
    html(div(class(card),
      [ div(class('card-header alert-warning'), "Hints"),
        div(class('card-body'), 
	    \ul_nonempty("Steps to the solution", List))
      ])).

avoid(_Id) -->
    { item(Item),
      solution(Item, _Solution, Path),
      traps(Path, Traps),
      findall(li(L), member(_-L, Traps), List)
    },
    html(div(class(card),
      [ div(class('card-header alert-warning'), "For teachers only"),
	div(class('card-body'), \ul_nonempty("Avoid these traps:", List)) 
      ])).

wrong(_Id) -->
    { item(Item),
      findall(li(M), (route(Item, Wrong, _), r(Wrong, Result), mathml(Wrong = Result, M)), List)
    },
    html(div(class(card),
      [ div(class('card-header alert-information'), "For teachers only"),
        div(class('card-body'),
            \ul_nonempty("These results are recognized by the system:", List))
      ])).

