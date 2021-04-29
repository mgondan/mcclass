% User authentication
:- module(login, [anonymous/0, known/2, login/2, logout/0, login_screen/0, login_request/1, hello//0]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).

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
        script([src("https://code.jquery.com/jquery-3.5.1.slim.min.js"), integrity("sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj"), crossorigin(anonymous)], ''),
        script([src("https://cdn.jsdelivr.net/npm/popper.js@1.16.1/dist/umd/popper.min.js"), integrity("sha384-9/reFTGAW83EW2RDu2S0VKaIzap3H66lZH81PoYlFhbGU+6BZp6G7niu735Sk7lN"), crossorigin(anonymous)], ''),
        script([src("https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"), integrity("sha384-B4gt1jrGC7Jh4AgTPSdUtOBvfO8shuf57BaghqFfPlYxofvL8/KUEfYiJOMMV+rV"), crossorigin(anonymous)], '')
      ]).

login_request(Data) :-
    member(email=Email, Data),
    http_log("Email: ~w\n", Email),
    member(user_id=Id, Data),
    http_log("User-Id: ~w\n", Id),
    login(Email, Id).

