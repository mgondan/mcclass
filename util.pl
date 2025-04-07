:- module(util, [expression_to_list/3, fmt/2, fmt/3]).

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


% Shortcuts 
fmt(tratio, Col, span(class('text-nowrap'), [\mmlm(Col, t), "-ratio"])).
fmt(ttest, Col, span(class('text-nowrap'), [\mmlm(Col, t), "-test"])).
fmt(tdist, Col, span(class('text-nowrap'), [\mmlm(Col, t), "-distribution"])).
fmt(tstat, Col, span(class('text-nowrap'), [\mmlm(Col, t), "-statistic"])).
fmt(pvalue, Col, span(class('text-nowrap'), [\mmlm(Col, p), "-value"])).

fmt(tratio, span(class('text-nowrap'), [\mmlm(t), "-ratio"])).
fmt(ttest, span(class('text-nowrap'), [\mmlm(t), "-test"])).
fmt(tdist, span(class('text-nowrap'), [\mmlm(t), "-distribution"])).
fmt(tstat, span(class('text-nowrap'), [\mmlm(t), "-statistic"])).
fmt(pvalue, span(class('text-nowrap'), [\mmlm(p), "-value"])).