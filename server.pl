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

:- dynamic cache/2.
id_assert(Id, Fact) :-
    assert(cache(Id, Fact)).

cache(Id) :-
    item(Item),
    solution(Item, Solution, Path),
    r_init(Id),
    r(Solution, Result),
    id_assert(Id, solution(Item, Solution, Path, Result)),
    praise([], Path, Praise, _),
    id_assert(Id, praise(Item, Praise)),
    hints(Path, Hints, Code_Hints),
    id_assert(Id, hints(Item, Hints, Code_Hints)),
    traps(Path, Traps, Code_Traps),
    id_assert(Id, traps(Item, Traps, Code_Traps)),
    wrongs_paths_results(Item, Wrongs_Paths_Results),
    maplist(id_assert(Id), Wrongs_Paths_Results).

:- cache(tpaired).

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
    http_parameters(Request,
      [ download(Download, [optional(true)]), 
        help(Help, [optional(true)]),
        response(Response, [optional(true)]) 
      ]),
    post(Id, [download(Download), help(Help), response(Response)]).

handler(Id, _Request) :-
    page(Id).

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
    r_init(tpaired),
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
	meta([name(viewport), content('width=device-width, initial-scale=1')])
      ],
      [ \item(Item, Response),
        \help(Id),
        \feedback(Id, Response),
	\wrongs(Id),
	\avoid(Id),
        \navigation(Id, [1-tpaired, 2-pvalue])
      ]).

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

feedback(Id, Response) -->
    { quantity(_, _, Response),
      item(Item),
      cache(Id, solution(Item, Solution, _, Result)),
      match(Result, Response, Format),
      cache(Id, praise(Item, Praise))
    },
    html(div(class(card),
      [ div(class('card-header alert-success'), "Correct result"),
        div(class('card-body'), 
	  [ \ul_nonempty(\mml(Solution = Result), Praise),
            \ul_nonempty("Additional hints", Format)
	  ])
      ])).

feedback(Id, Response) -->
    { quantity(_, _, Response),
      item(Item),
      cache(Id, wrong(Item, Wrong, Woodden, Result)),
      match(Result, Response, Format),
      praise(Flags, Woodden, _, Code_Praise),
      cache(Id, hints(Item, _, Code_Hints)),
      relevant(Code_Praise, Code_Hints, RelPraise, IrrelPraise),
      palette(Wrong, Flags),
      mistakes(Flags, Woodden, _, Code_Mistakes),
      cache(Id, traps(Item, _, Code_Traps)),
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

help(Id) -->
    { hint_level(Level),
      item(Item),
      cache(Id, hints(Item, Hints, _)),
      findall(H, (nth1(Index, Hints, H), Index =< Level), List)
    }, 
    html(div(class(card),
      [ div(class('card-header alert-info'), "Hints"),
        div(class('card-body'), 
	    \ul_nonempty("Steps to the solution", List))
      ])).

avoid(Id) -->
    { item(Item),
      cache(Id, traps(Item, Traps, _))
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
	div(class('card-body'), \ul_nonempty("Avoid these traps:", Traps)) 
      ])).

wrongs(Id) -->
    { item(Item),
      cache(Id, solution(Item, Solution, _, Result)),
      mathml(Solution = number(Result), Correct),
      findall(W = number(R), cache(Id, wrong(Item, W, _, R)), Wrong),
      maplist(mathml, Wrong, Wrongs)
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
        div(class('card-body'),
            \ul_nonempty("The following results are recognized by the system:", [Correct | Wrongs]))
      ])).

