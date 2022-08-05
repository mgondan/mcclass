% User authentication
:- module(login, [anonymous/0, known/2, login_screen/0, login_request/1, hello//0]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).

:- http_handler(mcclass('signup_form.js'), http_reply_file('scripts/signup_form.js', []), []).
:- http_handler(mcclass(login), handler(login), []).
:- http_handler(mcclass(logout), handler(logout), []).
:- http_handler(mcclass(signin), handler(signin), []).
:- http_handler(mcclass(signup), handler(signup), []).

% Login (web form)
handler(login, _Request) :-
    !,
    login_screen.

% Logout
handler(logout, Request) :-
    !,
    logout,
    http_redirect(see_other, mcclass(.), Request).

% Signin
handler(signin, Request) :-
    !,
    member(method(post), Request),
    member(cookie(_Cookies), Request),
    % member(user_id=UserId, Cookies),
    http_parameters(Request, [email(Email, [])]),
    login(Email, _UserId),
    http_redirect(see_other, mcclass(.), Request).

% Signup
handler(signup, Request) :-
    !,
    member(method(post), Request),
    member(cookie(_Cookies), Request),
    % member(user_id=UserId, Cookies),
    http_parameters(Request, [email(Email, [])]),
    login(Email, _UserId),
    http_redirect(see_other, mcclass(.), Request).


anonymous :-
    \+ http_session_data(user(_, _)).

known(User, Id) :-
    http_session_data(user(User, Id)),
    http_log("Found ~w~n", [user(User, Id)]).

hello -->
    { known(User, Id),
      http_log("Found user: ~w ~w\n", [User, Id]) 
    },
    html(p("Hello ~w (Id = ~w)"-[User, Id])).

hello -->
    html(p("Anonymous user")).

login(User, Id) :-
    http_session_retractall(user(_, _)),
    http_session_assert(user(User, Id)),
    http_log("User registered: ~w\n", [user(User, Id)]).

logout :-
    http_session_retractall(user(_, _)).

login_screen :-
    reply_html_page(
      [ title('McClass'),
        link(
          [ rel(stylesheet),
            href("https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css"),
            integrity("sha384-gH2yIJqKdNHPEq0n4Mqa/HGKIhSkIHeL5AyhkYV8i59U5AR6csBvApHHNl/vI1Bx"),
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
      [ div(class(jumbotron),
          [ h1(class('display-4'), "Welcome to McClass"),
            p(class(lead), "Mis-conception Aware Competence Learning and Assessment Smart System"),
            hr(class('my-4')),
            ul([class('nav nav-pills'), role(tablist)],
              [ li(class('nav-item'), 
                  a([class('nav-link active'), id('signup-tab'), 'data-toggle'(tab), href('#signup'), role(tab), 'aria-controls'(signup), 'aria-selected'(true)], "Register")),
                li(class('nav-item'),
                  a([class('nav-link'), id('login-tab'), 'data-toggle'(tab), href('#login'), role(tab), 'aria-controls'(login), 'aria-selected'(false)], "Login"))
              ]),
            div(class('tab-content'),
              [ div([class('tab-pane fade show active'), id(signup), role(tabpanel), 'aria-labelledby'('signup-tab')], 
                div([class(card), style('max-width: 20em')], 
                  div(class('card-body'),
                    [ h5(class('card-title'), "New here? Please register"),
                      form([class('form-signup'), name(signup), method('POST'), action('/mcclass/signup'), onsubmit('return validateSignupForm()')],
                        [ div(class('form-group'),
                            [ label(for(email), "Email address"),
                              input(
                                [ class('form-control'),
                                  type(email),
                                  name(email),
                                  placeholder("Email address"),
                                  required(""),
                                  autofocus("")
                                ]),
                              small([class('form-text text-muted'), id(signup_error_email)], "Please provide a valid email")
                            ]),
                          div(class('form-group'),
                            [ label(for(password), "Password"),
                              input(
                                [ class('form-control'),
                                  type(password),
                                  name(password),
                                  placeholder("Password"),
                                  required("")
                                ]),
                              small([class('form-text text-muted'), id(signup_error_password)], "Password should have at least 4 characters")
                            ]),
                          div(class('form-group'),
                            [ label(for(verify), "Verify password"),
                              input(
                                [ class('form-control'),
                                  type(password),
                                  name(verify),
                                  placeholder("Please repeat your password"),
                                  required("")
                                ]),
                              small([class('form-text text-muted'), id(signup_error_verify)], "Passwords should match")
                            ]),
                          input(
                            [ type(hidden),
                              name(salt),
                              value('Some big secret that nobody should be able to read, but unfortunately anybody can, but never mind.')
                            ]),
                          button([class("btn btn-lg btn-primary btn-block"), type(submit)], "Register")
                        ])
                    ]))),
                div([class('tab-pane fade'), id(login), role(tabpanel), 'aria-labelledby'('login-tab')],
                div([class(card), style('max-width: 20em')],
                  div(class('card-body'),
                    [ h5(class('card-title'), "Been here already? Please login"),
                      form([class('form-login'), name(login), method('POST'), onsubmit('return validateLoginForm()')],
                        [ div(class('form-group'),
                            [ label(for(email), "Email address"),
                              input(
                                [ class('form-control'),
                                  type(email),
                                  name(email),
                                  id(email),
                                  placeholder("Email address"),
                                  required(""),
                                  autofocus("")
                                ]),
                              small([class('form-text text-muted'), id(error_email)], "123")
                            ]),
                          div(class('form-group'),
                            [ label(for(password), "Password"),
                              input(
                                [ class('form-control'),
                                  type(password),
                                  name(password),
                                  id(password),
                                  placeholder("Password"),
                                  required("")
                                ]),
                              small([class('form-text text-muted'), id(error_password)], "456")
                            ]),
                          input(
                            [ type(hidden),
                              id(salt),
                              name(salt),
                              value('Some big secret that nobody should be able to read, but unfortunately anybody can, but never mind.')
                            ]),
                          button([class("btn btn-lg btn-primary btn-block"), type(submit)], "Login")
                        ])
                    ])))
              ])
          ]),
        script(src("/mcclass/signup_form.js"), ''),
        script([src("https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/js/bootstrap.bundle.min.js"), 
                integrity("sha384-A3rJD856KowSb7dwlZdYEkO39Gagi7vIsF0jrRAoQmDKKtQBHUuLZ9AsSv4jD4Xa"),
                crossorigin("anonymous")], '')
      ]).

login_request(Data) :-
    member(email=Email, Data),
    http_log("Email: ~w\n", Email),
    member(user_id=Id, Data),
    http_log("User-Id: ~w\n", Id),
    login(Email, Id).

