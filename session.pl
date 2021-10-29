:- module(session, [session_assert/1, session_data/1, session_id/1]).

:- use_module(library(http/http_session)).
:- dynamic session_data/1.

session_assert(Data) :-
    http_in_session(_),
    !,
    http_session_assert(Data).

session_assert(Data) :-
    assert(session_data(Data)).

session_retract(Data) :-
    http_in_session(_),
    !,
    http_session_retract(Data).

session_retract(Data) :-
    retract(session_data(Data)).

session_data(Data) :-
    http_in_session(_),
    !,
    http_session_data(Data).

:- listen(http_session(end(_Id, _Peer)), session_end).

session_end :-
    session_data(tmp(File)),
    delete_file(File),
    session_retract(tmp(File)),
    fail.

session_id(Id) :-
    http_in_session(S),
    !,
    Id = S.

session_id(default_session).

