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
:- use_module(library(quantity)).
:- use_module(library(r/r_call)).
:- use_module(login).

:- initialization http_daemon.

:- dynamic cache/2.
topic_assert(Topic, Fact) :-
    assert(cache(Topic, Fact)).

cache(Topic) :-
    item(Topic: Item),
    r_init(Topic),
    solution(Topic, Item, Solution, Path),
    rod(Solution, Result),
    topic_assert(Topic, solution(Item, Solution, Path, Result)),
    praise(Topic, Item, Path, Code_Praise, Praise),
    topic_assert(Topic, praise(Item, Code_Praise, Praise)),
    hints(Topic, Item, Path, Code_Hints, Hints),
    topic_assert(Topic, hints(Item, Code_Hints, Hints)),
    traps(Topic, Item, Path, Code_Traps, Traps),
    topic_assert(Topic, traps(Item, Code_Traps, Traps)),
    wrongs_paths_results(Topic, Item, WPR),
    maplist(topic_assert(Topic), WPR).

:- consult(tpaired), cache(tpaired).
:- consult(confint), cache(confint).
:- consult(tgroups), cache(tgroups).
:- consult(chisq), cache(chisq).
:- consult(baseline), cache(baseline).
:- consult(dbinom), cache(dbinom).
:- consult(uqbinom), cache(uqbinom).
:- consult(pwbinom), cache(pwbinom).
:- consult(ztrans), cache(ztrans).

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(mcclass, root(mcclass), []).

:- http_handler(mcclass('favicon.ico'), http_reply_file('favicon.ico', []), []).
:- http_handler(mcclass('signup_form.js'), http_reply_file('scripts/signup_form.js', []), []).
:- http_handler(mcclass(login), handler(login), []).
:- http_handler(mcclass(logout), handler(logout), []).
:- http_handler(mcclass(signin), handler(signin), []).
:- http_handler(mcclass(signup), handler(signup), []).
:- http_handler(mcclass(tpaired), handler(tpaired), []).
:- http_handler(mcclass(confint), handler(confint), []).
:- http_handler(mcclass(tgroups), handler(tgroups), []).
:- http_handler(mcclass(chisq), handler(chisq), []).
:- http_handler(mcclass(baseline), handler(baseline), []).
:- http_handler(mcclass(dbinom), handler(dbinom), []).
:- http_handler(mcclass(uqbinom), handler(uqbinom), []).
:- http_handler(mcclass(pwbinom), handler(pwbinom), []).
:- http_handler(mcclass(ztrans), handler(ztrans), []).
:- http_handler(mcclass(.), http_redirect(see_other, mcclass(tpaired)), []).
:- http_handler(root(.), http_redirect(see_other, mcclass(.)), []).

% Login (web form)
handler(login, _Request) :-
    !,
    login.

% Logout
handler(logout, Request) :-
    !,
    logout,
    http_redirect(see_other, mcclass(.), Request).

% Signin
handler(signin, Request) :-
    !,
    member(method(post), Request),
    member(cookie(Cookies), Request),
    member(user_id=UserId, Cookies),
    http_parameters(Request, [email(Email, [])]),
    signin(Email, UserId),
    http_redirect(see_other, mcclass(.), Request).

% Signup
handler(signup, Request) :-
    !,
    member(method(post), Request),
    member(cookie(Cookies), Request),
    member(user_id=UserId, Cookies),
    http_parameters(Request, [email(Email, [])]),
    signup(Email, UserId),
    http_redirect(see_other, mcclass(.), Request).

% Prepare handler
handler(Id, Request) :-
    r_init(Id),
    member(method(post), Request),
    !,
    ( member(cookie(Cookies), Request), member(user_id=U, Cookies) 
      -> Users = [user_id=U]
      ;  Users = []
    ), 
    http_parameters(Request, [], [form_data(Form)]),
    append(Users, Form, Data), 
    handle(Id, Data).

handler(Id, _Request) :-
    handle(Id, []).

% Download csv data
:- dynamic temp/3.
handle(Id, Data) :-
    member(download=_, Data),
    temp(Id, Temp, csv),
    format(atom(File), 'attachment; filename=~k.csv', [Id]),
    http_current(Request),
    http_reply_file(Temp, 
      [ unsafe(true), 
        mime_type(text/csv), headers(['Content-Disposition'(File)])
      ], Request).

% Download xlsx data
handle(Id, Data) :-
    member(download=_, Data),
    temp(Id, Temp, xlsx),
    format(atom(File), 'attachment; filename=~k.xlsx', [Id]),
    http_current_request(Request),
    http_reply_file(Temp, 
      [ unsafe(true), 
        mime_type('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'), 
        headers(['Content-Disposition'(File)]) 
      ], Request).

% Ask for help
handle(Id, Data) :-
    member(help=_, Data),
    hint_increase(Id),
    page(Id).

% Evaluate response
handle(Id, Data) :-
    member(response=Response, Data),
    responded(Id, Response),
    page(Id).

% Toggle buggy rules
handle(Id, Data) :-
    member(buggy=_, Data),
    buggies(Id, Codes_Feedback),
    findall(C, (member(C-_, Codes_Feedback), member(C=on, Data)), List),
    bugs(Id, List),
    page(Id).

% Default
handle(Id, _) :-
    page(Id).

page(Id) :-
    response(Id, Response),
    reply_html_page(
      [ title('McClass'),
        link([rel(stylesheet),
                href('https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css'),
                integrity('sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z'),
                crossorigin(anonymous)]),
	    link([rel(icon), href('/mcclass/favicon.ico'), type('image/x-icon')]),
	    meta([name(viewport), content('width=device-width, initial-scale=1')])
      ],
      [ \hello,
        \item(Id, Response),
        \help(Id),
        \feedback(Id, Response),
        \wrongs(Id),
        \avoid(Id),
        \navigation(Id, 
          [ 1-tpaired, 2-confint, 3-tgroups, 4-chisq, 5-ztrans, 6-dbinom, 7-uqbinom, 8-pwbinom, 9-baseline
          ])
      ]).

hint_level(Id, Hint) :-
    http_session_data(hint(Id, H))
     -> Hint = H
      ; Hint = 0.

hint_increase(Id) :-
    hint_level(Id, Hint),
    H is Hint + 1,
    http_session_asserta(hint(Id, H)).

response(Id, Response) :-
    http_session_data(response(Id, Response)),
    !.

response(_, '').

responded(Id, Response) :-
    http_session_retractall(response(Id, _)),
    http_session_assert(response(Id, Response)).

bugs(Id, Bugs) :-
    ground(Bugs),
    !,
    http_session_retractall(buggies(Id, _)),
    http_session_assert(buggies(Id, Bugs)).

bugs(Id, Bugs) :-
    http_session_data(buggies(Id, B)),
    !, Bugs = B.

bugs(Id, Bugs) :-
    buggies(Id, Codes_Feed),
    pairs_keys(Codes_Feed, Bugs),
    http_session_assert(buggies(Id, Bugs)).

feedback(_Id, '') -->
    html(div(class(card),
      [ div(class('card-header alert-information'), "Feedback"),
        div(class('card-body'), 
            p(class('card-text'), "Waiting for response..."))
      ])).

feedback(Id, Response) -->
    { quantity(_, _, Response),
      item(Id: Item),
      cache(Id, solution(Item, Solution, _, Result)),
      match(Result, Response, Format),
      cache(Id, praise(Item, _, Praise))
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
      item(Id: Item),
      cache(Id, wrong(Item, Wrong, Woodden, Result)),
      match(Result, Response, Format),
      praise(Id, Item, Woodden, Code_Praise, _),
      %      cache(Id, praise(Item, Code_Praise, _)),
      cache(Id, hints(Item, Code_Hints, _)),
      relevant(Code_Praise, Code_Hints, RelPraise, IrrelPraise),
      palette(Wrong, Flags),
      mistakes(Id, Item, Woodden, [fix(all) | Flags], Code_Mistakes, _),

      bugs(Id, Bugs),
      pairs_keys(Code_Mistakes, Codes_Bugs),
      subset(Codes_Bugs, Bugs),

      cache(Id, traps(Item, Code_Traps, _)),
      relevant(Code_Mistakes, Code_Traps, RelMistake, IrrelMistake),
      append([IrrelPraise, IrrelMistake, Format], Additional),
      mathml([fix(all) | Flags], Wrong, Fix),
      mathml([show(all) | Flags], Wrong, Show)
    },
    html(div(class(card),
      [ div(class('card-header alert-warning'), "Incorrect result"),
        div(class('card-body'),
          [ p(class('card-text'), "This is the formula for the correct result:"),
	        p(class('card-text'), Fix),
	        p(class('card-text'), "Your result matches the following expression:"),
	        p(class('card-text'), Show),
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

help(Id) -->
    { hint_level(Id, 0) },
    html(div(class(card),
      [ div(class('card-header'), "Hints"),
        div(class('card-body'),
            form([class(form), method('POST'), action('#question')],
                button([class('btn btn-link'), name(help), value(hint)], "Give me a hint")))
      ])).

help(Id) -->
    { hint_level(Id, Level),
      item(Id: Item),
      cache(Id, hints(Item, _, Hints)),
      append(Hints, ["No more hints available."], NoMore),
      findall(H, (nth1(I, NoMore, H), I =< Level), List)
    }, 
    html(div(class(card),
      [ div(class('card-header'), "Hints"),
        div(class('card-body'), 
            form([class(form), method('POST'), action('#question')],
	          [ \ul_nonempty("Steps to the solution", List),
                button([class('btn btn-link'), name(help), value(hint)], "Give me another hint")
              ]))
      ])).

avoid(Id) -->
    { item(Id: Item),
      cache(Id, traps(Item, _, Traps))
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
        div(class('card-body'), \ul_nonempty("Avoid these traps:", Traps)) 
      ])).

wrongs(Id) -->
    { item(Id: Item),
      buggies(Id, Codes_Feedback),
      bugs(Id, Active),
      cache(Id, solution(Item, Solution, _, Result)),
      mathml(Solution = quantity(Result), Correct),
      findall(W = quantity(R), 
        ( cache(Id, wrong(Item, W, Path, R)), 
          mistakes(Id, Item, Path, [], Codes_Mistakes, _),
          pairs_keys(Codes_Mistakes, Codes),
          subset(Codes, Active)
        ), Wrong),
      maplist(mathml, Wrong, Wrongs)
    },
    html(div(class(card),
      [ div(class('card-header alert-info'), "For teachers only"),
        div(class('card-body'),
          [ \ul_nonempty("Correct result:", [Correct]),
            \cb_nonempty("Buggy rules", buggy, Codes_Feedback, Active),
            br(''),
	        \ul_nonempty("The following incorrect results are recognized by the system:", Wrongs)
          ])
      ])).

