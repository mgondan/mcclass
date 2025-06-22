:- module(util, [nowrap//1, my_subset/3]).

:- use_module(library(http/html_write)).
:- use_module(mathml).

% Shortcuts 
nowrap(List) 
 --> html(span(class('text-nowrap'), List)).

% my_subset(+List, -Subset, -Difference)
my_subset([], [], []).
my_subset([X | L], [X | S], D) :-
    my_subset(L, S, D).
my_subset(L, [H | S], [H | D]) :-
    my_subset(L, S, D).

