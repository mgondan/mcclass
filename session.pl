% Session management for web and console applications
:- module(session, 
    [ session_id/1,
      session_assert/1,
      session_data/1,
      session_data/2,
      session_retract/1,
      session_retractall/1,
      session_tmpfile/1
    ]).

:- use_module(library(http/http_session)).
:- use_module(library(http/http_log)).

:- dynamic local_session_data/1.

session_id(Session),
    http_in_session(S)
 => Session = S.

session_id(Session)
 => Session = default_session.

% Store information for a session, either in the session or as a dynamic
% predicate session_data/1
session_assert(Data),
    http_in_session(_)
 => http_session_asserta(Data).

session_assert(Data)
 => assert(local_session_data(Data)).

% Retrieve information. If no http session is running, the dynamic predicate
% session_data/1 is used.
session_data(Data) :-
    http_in_session(_),
    !,
    http_session_data(Data).

session_data(Data) :-
    local_session_data(Data).

session_data(Data, _Default) :-
    session_data(Data).

session_data(Default, Default).

% Remove information, same as above
session_retract(Data),
    http_in_session(_)
 => http_session_retract(Data).

session_retract(Data)
 => retract(local_session_data(Data)).

session_retractall(Data),
    http_in_session(_)
 => http_session_retractall(Data).

session_retractall(Data)
 => retractall(session_data(Data)).

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
    fail. % force backtracking

session :-
    session_assert(data(data=1)),
    session_data(data(D)),
    writeln(data(D)),
    session_retract(data(_)),
    session_data(nodata(N), nodata(nodata)),
    writeln(nodata(N)),
    session_data(resp(tpaired, tratio, Resp), resp(tpaired, tratio, '#.##')),
    writeln(Resp),
    session_tmpfile(Temp),
    writeln(Temp).

