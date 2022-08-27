:- module(users, [exists_user/1, login_user/2, add_user/2]).

:- use_module(library(sha)).
:- use_module(library(odbc)).

db_init :-
    nb_current(add_user, _),
    !.

db_init :-
    odbc_connect(mcclass, _, [alias(mcclass)]),
    odbc_prepare(mcclass, 'INSERT INTO users (email, password) VALUES (?, ?)', [default, default], AddUser),
    nb_setval(add_user, AddUser).

exists_user(Email) :-
    db_init,
    odbc_query(mcclass, "SELECT email FROM users WHERE email = '~w'"-[Email], row(Email)).

salt("mcclass").

login_user(Email, Password) :-
    exists_user(Email),
    salt(Salt),
    atomic_list_concat([Salt, Email, Password], Message),
    sha_hash(Message, Hash, [algorithm(sha256), encoding(utf8)]),
    hash_atom(Hash, Atom),
    odbc_query(mcclass, "SELECT password FROM users WHERE email = '~w'"-[Email], row(Atom)).

add_user(Email, Password) :-
    salt(Salt),
    atomic_list_concat([Salt, Email, Password], Message),
    sha_hash(Message, Hash, [algorithm(sha256), encoding(utf8)]),
    hash_atom(Hash, Atom),
    db_init,
    nb_getval(add_user, AddUser),
    odbc_execute(AddUser, [Email, Atom]).

:- at_halt(db_close).
db_close :-
    nb_current(add_user, AddUser),
    odbc_free_statement(AddUser),
    odbc_disconnect(mcclass).

db_close.
