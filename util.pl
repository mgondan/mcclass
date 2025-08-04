:- module(util, [nowrap//1, my_subset/3, assert_clauses/1, add_option/3]).

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

% Assert clauses of interval_/3 at beginning of module
assert_clauses(Module) :-
    findall((rint:interval_(Expr, Res, Flags) :- Body), 
            clause(Module:interval_(Expr, Res, Flags), Body), Bag0),
    reverse(Bag0, Bag),
    maplist(asserta, Bag, _),
    abolish(Module:interval_/3). 

add_option(Option, Old, New),
    compound_name_arguments(Option, Name, _),
    OptionName =.. [Name, _],
    option(OptionName, Old)
 => New = Old.

add_option(Option, Old, New)
 => New = [Option | Old].