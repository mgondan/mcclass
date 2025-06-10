:- module(r_session, 
  [ r_init_session/0, r//1, r_session/1, r_session/2, r_session//1, r_session_source/1,
    r_topic/1, r_topic/2, r_topic//1
  ]).

:- reexport(library(r)).
:- use_module(session).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_log)).

% Initialize R, load some code into the base environment.
r_init_session :-
    session_id(S),
    r_initialize(S),
    r_session_begin.

% Evaluate R expression and render it as html
r(Expr) -->
    { r(Expr, R) },
    term_string(R, S),
    html(S).
    
% Use R environment for session specific R commands
:- listen(http_session(begin(_Session, _Peer)), r_session_begin).

% Avoid calling twice in case of redirection (see_other)
r_session_begin,
    session_data(r_session)
 => true.

r_session_begin
 => session_id(Session),
    r_initialize(Session),
    session_assert(r_session).

:- listen(http_session(end(_Session, _Peer)), r_session_end).

r_session_end
 => session_id(Session),
    r_close(Session).

% Evaluate R expression in the current http_session for the current topic
r_session(Expr)
 => r(Expr).

r_session(Expr, Res)
 => r(Expr, Res).

r_session(Expr) -->
    { r_session(Expr, Res) },
    term_string(Res, String),
    html(String).

r_topic(Expr),
    b_getval(topic, Topic)
 => r_session(with(Topic, Expr)).

r_topic(Expr, Res),
    b_getval(topic, Topic)
 => r_session(with(Topic, Expr), Res).

r_topic(Expr)
--> { r_topic(Expr, Res) },
    term_string(Res, String),
    html(String).

% Load a topic file into the current session
r_session_source(Topic),
    session_data(topic(Topic))
 => true.

r_session_source(Topic)
 => format(string(S), "~w.R", [Topic]),
    absolute_file_name(S, Abs),
    atom_string(Abs, String),
    r('<-'(Topic, 'new.env'())),
    r(with(Topic, source(String, local=true))),
    session_assert(topic(Topic)).
