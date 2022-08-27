% User authentication
:- module(login, [current_user/1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(users).

:- http_handler(mcclass('login.js'), http_reply_file('scripts/login.js', []), []).
:- http_handler(mcclass(login), login, []).
:- http_handler(mcclass(register), register, []).
:- http_handler(mcclass(logout), logout, []).

logout(Request) :-
    do_logout,
    http_redirect(see_other, '/mcclass/', Request).

login(Request) :-
    formdata(Request, FormData),
    login(Request, FormData).

% Existing user, correct password: login, wrong password: show error
login(Request, FormData) :-
    option(email(Email), FormData),
    exists_user(Email),
    option(password(Password), FormData),
    !,
    ( do_login(Email, Password)
     -> http_redirect(see_other, '/mcclass/', Request)
     ;  form([login_failed, login(Email)])
    ).

% New user: please register
login(_Request, FormData) :-
    option(email(Email), FormData),
    \+ exists_user(Email),
    !,
    form([please_register, register(Email), login(Email)]).

register(Request) :-
    formdata(Request, FormData),
    register(Request, FormData).

% Existing user, correct password: login, wrong password: show error
register(Request, FormData) :-
    option(email(Email), FormData),
    exists_user(Email),
    option(password(Password), FormData),
    !,
    ( do_login(Email, Password)
     -> http_redirect(see_other, '/mcclass/', Request)
     ;  form([user_exists, register(Email), login(Email)])
    ).

% New user: register
register(Request, FormData) :-
    option(email(Email), FormData),
    \+ exists_user(Email),
    option(password(Password), FormData),
    option(verify(Password), FormData),
    !,
    do_register(Email, Password),
    http_redirect(see_other, '/mcclass/', Request).

% New user: register
register(_Request, FormData) :-
    option(email(Email), FormData),
    \+ exists_user(Email),
    option(password(Password), FormData),
    option(verify(Verify), FormData),
    dif(Password, Verify),
    !,
    form([password_verify, register(Email)]).

% Default: show registration form
register(_Request, FormData) :-
    option(email(Email), FormData, ''),
    form([register(Email)]).

form(Options) :-
    option(register(RegisterEmail), Options, ''),
    option(login(LoginEmail), Options, ''),
    ( member(user_exists, Options)
     -> RegisterEmailValid = ['is-invalid']
      ; RegisterEmailValid = []
    ),
    ( member(password_verify, Options)
     -> RegisterVerifyValid = ['is-invalid']
      ; RegisterVerifyValid = []
    ),
    ( member(please_register, Options)
     -> LoginEmailValid = ['is-invalid']
      ; LoginEmailValid = []
    ),
    ( member(login_failed, Options)
     -> LoginPasswordValid = ['is-invalid']
      ; LoginPasswordValid = []
    ),
    reply_html_page(
      [ title('McCLASS user'),
        link([rel(stylesheet), href("bootstrap.min.css")]),
        link([rel(icon), href('favicon.ico'), type('image/x-icon')]),
        meta([name(viewport), content('width=device-width, initial-scale=1')])
      ],
      [ h1(class('display-4'), "Welcome to McCLASS"),
        p(class(lead), "Misconception Aware Competence Learning and Assessment Smart System"),
        hr(class('my-4')),

        div([class(card), style('max-width: 20em')], 
          div(class('card-body'),
            [ h5(class('card-title'), "New here? Please register"),
              form([class('form-login'), id('register-form'), name(register), method(post), action(register), novalidate(''), onsubmit("return validateRegister();")],
                [ div(class('mb-3'), 
                    div(class('input-group has-validation'),
                      [ div([class(['form-floating' | RegisterEmailValid]), id('register-email-group')],
                          [ input([class(['form-control' | RegisterEmailValid]), type(email), id('register-email'), name(email), placeholder("Email"), value(RegisterEmail)], ''),
                            label(for('register-email'), "Email")
                          ]),
                        div(class('invalid-feedback'), "Email address already registered.")
                      ])),
                  div(class('mb-3'),
                    div(class('input-group has-validation'),
                      [ div([class('form-floating'), id('register-password-group')],
                          [ input([class('form-control'), type(password), id('register-password'), name(password), placeholder("Password")], ''),
                            label(for('register-password'), "Password")
                          ]),
                        div(class('invalid-feedback'), "Please choose a good password.")
                      ])),
                  div(class('mb-3'),
                    div(class('input-group has-validation'),
                      [ div([class(['form-floating' | RegisterVerifyValid]), id('verify-group')],
                          [ input([class(['form-control' | RegisterVerifyValid]), type(password), id('register-verify'), name(verify), placeholder("Verify password")], ''),
                            label(for('register-verify'), "Verify password")
                          ]),
                        div(class('invalid-feedback'), "Passwords do not match.")
                      ])),
                  button([class("btn btn-lg btn-primary btn-block"), name(register), value(true), type(submit)], "Register")
                ])
            ])),

          div([class(card), style('max-width: 20em')],
          div(class('card-body'),
            [ h5(class('card-title'), "Been here already? Please login"),
              form([class('form-login'), id('login-form'), name(login), method(post), action(login), novalidate(''), onsubmit("return validateLogin();")],
                [ div(class('mb-3'),
                    div(class('input-group has-validation'),
                      [ div([class(['form-floating' | LoginEmailValid]), id('login-email-group')],
                          [ input([class(['form-control' | LoginEmailValid]), type(email), id('login-email'), name(email), value(LoginEmail), placeholder("Email")], ''),
                            label(for('login-email'), "Email")
                          ]),
                        div(class('invalid-feedback'), "Unknown user id/email address.")
                      ])),
                  div(class('mb-3'),
                    div(class('input-group has-validation'),
                      [ div([class(['form-floating' | LoginPasswordValid]), id('login-password-group')],
                          [ input([class(['form-control' | LoginPasswordValid]), type(password), id('login-password'), name(password), placeholder("Password")], ''),
                            label(for('login-password'), "Password")
                          ]),
                        div(class('invalid-feedback'), "Login failed.")
                      ])),
                  button([class("btn btn-lg btn-primary btn-block"), name(login), value(true), type(submit)], "Login")
                ])
            ])),
        script(src('login.js'), ''),
        script(src("bootstrap.bundle.min.js"), '')
      ]).

% Helpers
formdata(Request, FormData),
    member(method(post), Request)
 => http_parameters(Request, [], [form_data(FormData)]).

formdata(_Request, FormData)
 => FormData = [].

% User database
do_login(Email, Password) :-
    http_log("do_login: ~w (~w)~n", [Email, Password]),
    exists_user(Email),
    do_logout,
    login_user(Email, Password),
    http_session_assert(email(Email)).
    
do_register(Email, Password) :-
    \+ exists_user(Email),
    do_logout,
    http_session_assert(email(Email)),
    add_user(Email, Password).

current_user(Email) :-
    http_session_data(email(Email)).

do_logout :-
    http_session_retractall(email(_)).

