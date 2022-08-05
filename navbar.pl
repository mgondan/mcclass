:- module(navbar, [navbar//0]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- http_handler(mcclass('owl.svg'), http_reply_file('images/owl.svg', []), []).

navbar -->
    html(nav(class('navbar sticky-top bg-light'), 
      div(class('container-fluid'),
        a([class('navbar-brand'), href('/mcclass')],
          [ img([src('owl.svg'), width(45), height(36), class('d-inline-block align-text-top')], ''),
            'McCLASS'
          ])))).

