:- module(r, 
  [ r/1, r/2, r//1, 
    r_session_begin/0, 
    r_session_end/0, 
    r_session_call/1, 
    r_session_eval/2,
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

% Use R environment for session specific R commands
:- listen(http_session(begin(Id, _Peer)), r_session_begin(Id)).

r_session_begin :-
    session_id(Id),
    r_session_begin(Id).

% Avoid calling twice in case of redirection (see_other)
%
% session_data looks for its own session id
r_session_begin(_Id) :-
    session_data(session),
    !.

r_session_begin(Id) :-
    r_call('<-'(Id, 'new.env'())),
    session_assert(session).

% Session clean up (here and in session.pl)
:- listen(http_session(end(Id, _Peer)), r_session_end(Id)).

r_session_end :-
    session_id(Id),
    r_session_end(Id).

r_session_end(Id) :-
    r_call(rm(Id)).

% Evaluate R expressions in the current http_session
r_session_call(Expr) :-
    session_id(Id),
    r(with(Id, Expr)).

r_session_eval(Expr, Res) :-
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

% Call r.R on startup
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
   r_session_call('<-'(a, 2)),
   r(a, A),
   r_session_eval(a, S),
   writeln(a=A),
   writeln(session(a)=S),
   r_session_source(tpaired),
   r_session_eval(mu, M),
   writeln(session(mu)=M),
   r_session_end.

