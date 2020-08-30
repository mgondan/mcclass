% User authentication
:- module(login, [anonymous/0, known/1, login/1, logout/0, login_screen/0, login_request/1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).

anonymous :-
    \+ http_session_data(user(_)).

known(User) :-
    http_session_data(user(User)).

login(User) :-
    http_session_retractall(user(_)),
    http_session_assert(user(User)).

logout :-
    http_session_retractall(user(_)).

login_screen :-
    reply_html_page(
          [ title('McClass'),
            link(
              [ rel(stylesheet),
                href('https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css'),
                integrity('sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z'),
                crossorigin(anonymous)
            ]),
            link(
              [ rel(icon), 
                href('/mcclass/favicon.ico'), 
                type('image/x-icon')
              ]),
            meta(
              [ name(viewport), 
                content('width=device-width, initial-scale=1')
              ])
          ],
        div(class('text-center'), div([class(container), style('max-width: 20em')],
            form([class('form-signin'), method('POST')],
              [ h1("Please sign in"),
                label([class('sr-only'), for(email)], "Email address"),
                input(
                  [ class('form-control'),
                    type(email), 
                    name(email),
                    placeholder("Email address"),
                    required(""),
                    autofocus("")
                  ]),
                label([class('sr-only'), for(password)], "Password"),
                input(
                  [ class('form-control'),
                    type(password),
                    name(password),
                    placeholder("Password"),
                    required("")
                  ]),
                button([class("btn btn-lg btn-primary btn-block"), type(submit)], "Sign in")
              ])))).

login_request(Data) :-
    member(email=Email, Data),
    http_log("Email: ~w\n", Email),
    login(Email).

