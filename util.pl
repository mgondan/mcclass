:- module(util, [expression_to_list/3]).

%
% Used to render multiple expressions separated by semicolon as a list to be displayed as feedback.
% If only one expression is found, it is not wrapped into a list.
%
%?- expression_to_list({1;2}, [color(zadd,"$blue")], M).
%   M = ul([li([\mmlm([color(zadd, "$blue")], 1)]), li([\mmlm([color(zadd, "$blue")], 2)])]).
%  
%?- expression_to_list({1}, [color(zadd,"$blue")], M).
%   M = \mmlm([color(zadd, "$blue")], 1) .  

expression_to_list(Expr, Flags, M) :-
    term_string(Expr, String),
    string_concat('{', String1, String),
    string_concat(String2, '}', String1),
    atomic_list_concat(Parts, ';', String2),
    as_list_items(Parts, Flags, M).
 
as_list_items([Expr| []], Flags, M) :-
    wrap_in_mmlm(Flags, Expr, M).

as_list_items(Expr, Flags, ul(Items)) :-
    maplist(wrap_in_mmlm(Flags), Expr, List),
    maplist(list_item, List, Items).

list_item(MmlmExpr, li([MmlmExpr])).

wrap_in_mmlm(Flags, Expr, \mmlm(Flags, Term)) :-
    term_string(Term, Expr).
