% Session management help functions for web and console applications
:- module(session, 
    [ session_id/1,
      session_assert/1,
      session_assert/2,
      session_data/1,
      session_data/2,
      session_retract/1,
      session_retract/2,
      session_tmpfile/1
    ]).

:- use_module(library(http/http_session)).

:- dynamic session_data/1.
:- dynamic session_data/2.

% Session management for web and console applications
session_id(Id) :-
    http_in_session(S),
    !,
    Id = S.

session_id(default_session).

% Store information for a session, either in the session or as a dynamic 
% predicate
session_assert(Data) :-
    session_assert(no_task, Data).

% Task-specific session information
session_assert(Task, Data) :-
    http_in_session(_),
    !,
    http_session_assert(session_data(Task, Data)).

session_assert(Task, Data) :-
    assert(session_data(Task, Data)).

% Retrieve information. If no http session is running, the dynamic predicate
% session_data/1,2 is used.
session_data(Data) :-
    session_data(no_task, Data).

session_data(Task, Data) :-
    http_in_session(_),
    !,
    http_session_data(session_data(Task, Data)).

% Remove information
session_retract(Data) :-
    session_retract(no_task, Data).

session_retract(Task, Data) :-
    http_in_session(_),
    !,
    http_session_retract(session_task(Task, Data)).

session_retract(Task, Data) :-
    retract(session_data(Task, Data)).

% Create a session-specific temporary file
session_tmpfile(String) :-
    tmp_file_stream(File, Stream, []),
    close(Stream),
    atom_string(File, String),
    session_assert(tmp(File)).
    
% Remove temporary files
:- listen(http_session(end(_Id, _Peer)), session_end).

session_end :-
    session_data(tmp(File)),
    delete_file(File),
    session_retract(tmp(File)),
    fail.

