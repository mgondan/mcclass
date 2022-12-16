:- module(r, 
  [ r_initialize/0,
    r/1, r/2, r//1, r_source/1, 
    r_session/1, r_session/2, r_session//1, r_session_source/1,
    r_task/1, r_task/2, r_task//1
  ]).

:- use_module(library(rologp)).
:- use_module(session).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_log)).

:- multifile r_hook/1.
:- dynamic r_initialized/0.

% Initialize R, load some code into the base environment.
r_initialize,
    r_initialized
 => true.

r_initialize
 => r_source(r),
    r_session_begin,
    assert(r_initialized).

% Call R
r(Expr) 
 => r_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => r_eval(Expr, Res).

% Evaluate R expression and render it as html
r(Expr) -->
    { r(Expr, R) },
    term_string(R, S),
    html(S).

r_source(File)
 => format(string(String), "~w.R", [File]),
    r(source(String)).

% Use R environment for session specific R commands
:- listen(http_session(begin(_Session, _Peer)), r_session_begin).

% Avoid calling twice in case of redirection (see_other)
r_session_begin,
    session_data(r_session)
 => true.

r_session_begin
 => session_id(Session),
    r_call('<-'(Session, 'new.env'())),
    session_assert(r_session).

:- listen(http_session(end(_Session, _Peer)), r_session_end).

r_session_end
 => session_id(Session),
    r_call(rm(Session)).

% Evaluate R expression in the current http_session for the current task
r_session(Expr)
 => session_id(Session),
    r(with(Session, Expr)).

r_session(Expr, Res)
 => session_id(Session),
    r(with(Session, Expr), Res).

r_session(Expr) -->
    { r_session(Expr, Res) },
    term_string(Res, String),
    html(String).

r_task(Expr),
    b_getval(task, Task)
 => r_session(with(Task, Expr)).

r_task(Expr, Res),
    b_getval(task, Task)
 => r_session(with(Task, Expr), Res).

r_task(Expr) -->
    { r_task(Expr, Res) },
    term_string(Res, String),
    html(String).

% Load a task file into the current session
r_session_source(Task),
    session_data(task(Task))
 => true.

r_session_source(Task)
 => format(string(S), "~w.R", [Task]),
    session_id(Session),
    r(with(Session, '<-'(Task, 'new.env'()))),
    r(with(Session, with(Task, source(S, local='TRUE')))),
    session_assert(task(Task)).

test :-
   r_init,
   r_session_begin,
   r('<-'(a, 1)),
   r_session('<-'(a, 2)),
   r(a, A),
   r_session(a, S),
   writeln(a=A),
   writeln(session(a)=S),
   r_session_source(tpaired),
   r_session($(tpaired, mu), Mu1),
   writeln(session($(tpaired, mu))=Mu1),
   r_session_source(tpaired),
   b_setval(task, tpaired),
   r_task(mu, Mu2),
   writeln(task(tpaired, mu)=Mu2).


