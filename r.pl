:- module(r, 
  [ r/1, r/2, r//1, 
    r_session_begin/0, 
    r_session_end/0, 
    r_session/1, 
    r_session/2,
    r_session//1,
    r_session_source/1
  ]).

:- use_module(library(rologp)).
:- use_module(session).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_log)).

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
:- listen(http_session(begin(Id, _Peer)), r_session_begin(Id)).

r_session_begin :-
    session_id(Id),
    r_session_begin(Id).

% Avoid calling twice in case of redirection (see_other)
r_session_begin(_Id) :-
    session_data(session),
    !.

r_session_begin(Id) :-
    http_log("begin session ~w~n", [Id]),
    r_call('<-'(Id, 'new.env'())),
    session_assert(session).

:- listen(http_session(end(Id, _Peer)), r_session_end(Id)).

r_session_end :-
    session_id(Id),
    r_session_end(Id).

r_session_end(Id) :-
    http_log("end session ~w~n", [Id]),
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
    r_session_begin,
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
   writeln(session(mu)=M),
   r_session_end.

