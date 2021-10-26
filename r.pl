:- module(r, 
  [ r/1, r/2, r//1, 
    r_session_new/0, 
    r_session_close/0, 
    r_session/1, 
    r_session/2,
    r_session//1,
    r_session_source/1
  ]).

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
    { r(Expr, R) },
    term_string(R, S),
    html(S).

% Evaluate R expressions in the current http_session
r_session_new :-
    session_id(Id),
    r_call('<-'(Id, 'new.env'())).

r_session_close :-
    session_id(Id),
    r_call(rm(Id)).

r_session(Expr) :-
    session_id(Id),
    r(with(Id, Expr)).

r_session(Expr, Res) :-
    session_id(Id),
    r(with(Id, Expr), Res).

r_session(Expr) -->
    { r_session(Expr, R) },
    term_string(R, S),
    html(S).

r_session_source(Module) :-
    session_data(Module),
    !.

r_session_source(Module) :-
    format(string(S), "~w.R", [Module]),
    session_id(Id),
    r(source(S, local=Id)),
    session_assert(Module).

init :-
    session_data(r),
    !.

init :-
    r_call(source("r.R")),
    r_session_new,
    session_assert(r).

test :-
   init,
   r('<-'(a, 1)),
   r_session('<-'(a, 2)),
   r(a, A),
   r_session(a, S),
   writeln(a=A),
   writeln(session(a)=S),
   r_session_source(tpaired),
   r_session(mu, M),
   writeln(session(mu)=M).

