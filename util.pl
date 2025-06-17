:- module(util, [nowrap//1]).

:- use_module(library(http/html_write)).
:- use_module(mathml).

% Shortcuts 
nowrap(List) 
 --> html(span(class('text-nowrap'), List)).


