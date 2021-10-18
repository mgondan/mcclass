:- module(r, [r/1, r/2, r//1]).

:- use_module(library(rologp)).
:- use_module(session).

% Call R
r(Expr) :-
    init,
    r_call(Expr).

% Evaluate R expression
r(Expr, Res) :-
    init,
    r_eval(Expr, Res).

% Evaluate R expression and render it as html
r(Expr) -->
    { r_eval(Expr, R) },
    term_string(R, S),
    html(S).

init :-
    session_data(init(r)),
    !.

init :-
    r_call(source("r.R")),
    session_assert(init(r)).

