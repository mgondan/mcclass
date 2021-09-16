:- module(session, [session_assert/1, session_data/1]).

:- use_module(library(http/http_session)).
:- dynamic session_data/1.

session_assert(Data) :-
    http_in_session(_),
    !,
    http_session_assert(Data).

session_assert(Data) :-
    assert(session_data(Data)).

session_data(Data) :-
    http_in_session(_),
    !,
    http_session_data(Data).

