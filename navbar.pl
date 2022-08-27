:- module(navbar, [navbar//0]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(login).

:- http_handler(mcclass('owl.svg'), http_reply_file('images/owl.svg', []), []).
:- multifile page/2.

navbar -->
    { findall(li(a([class('dropdown-item'), href(Page)], Title)), page(Page, Title), Pages) },
    html(nav(class('navbar navbar-expand-lg sticky-top bg-light'), 
      div(class('container-fluid'),
        [ a([class('navbar-brand'), href('/mcclass')],
            [ img([src('owl.svg'), width(48), height(40), class('d-inline-block align-text-top')], ''),
              'McCLASS'
            ]),
          button(
            [ class('navbar-toggler'), type(button), 
              'data-bs-toggle'(collapse), 'data-bs-target'('#navbarNav'), 
              'aria-controls'(navbarNav), 'aria-expanded'(true), 'aria-label'("Toggle navigation")
            ], span(class('navbar-toggler-icon'), '')),
          div([class('collapse navbar-collapse'), id(navbarNav)],
            [ ul(class('navbar-nav me-auto'),
                li(class('nav-item dropdown'),
                  [ a([class('nav-link dropdown-toggle'), role(button), 'data-bs-toggle'(dropdown), 'aria-expanded'(false)], 
                      "Exercises"),
                    ul(class("dropdown-menu"), Pages)
                  ])),
              \showuser
            ])
        ]))).

showuser -->
    { current_user(Email) },
    html(form([class("d-flex"), method(post), action(logout)],
      [ span(class('navbar-text me-2'), Email),
        button([class('btn btn-outline-danger'), type(submit)], "Logout")
      ])).

showuser -->
    html(form([class("d-flex"), method(post), action(register)],
      button([class('btn btn-outline-success'), type(submit)], "Login/Register"))).
