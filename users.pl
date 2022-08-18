:- module(users, [add_user/3]).

:- use_module(library(odbc)).

db_init :-
    nb_current(add_user, _),
    !.

db_init :-
    odbc_connect(mcclass, _, [alias(mcclass)]),
    odbc_prepare(mcclass, 'INSERT INTO users (id, email, password) VALUES (?, ?, ?)', [default, default, default], AddUser),
    nb_setval(add_user, AddUser).

add_user(UserId, Email, Password) :-
    db_init,
    nb_getval(add_user, AddUser),
    odbc_execute(AddUser, [UserId, Email, Password]).

:- at_halt(db_close).
db_close :-
    nb_current(add_user, AddUser),
    odbc_free_statement(AddUser),
    odbc_disconnect(mcclass).

db_close.
