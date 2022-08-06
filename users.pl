:- module(users, [add_user/1]).

:- initialization(init_db, after_load).
init_db :-
    odbc_connect(mcclass, _, [alias(mcclass)]),
    odbc_prepare(mcclass, 'INSERT INTO users (id, name, email) VALUES (?, ?, ?)', [default, default, default], AddUser),
    nb_setval(add_user, AddUser).

add_user(Email) :-
    findall(user(Id, Name, Email), get(Id, Name, Email), List),
    length(List, Length),
    Id is Length + 1,
    add_user(Id, user, Email).

add_user(Id, Name, Email) :-
    nb_getval(add_user, AddUser),
    odbc_execute(AddUser, [Id, Name, Email]).

get(Id, Name, Email) :-
    odbc_query(mcclass, "SELECT id, name, email FROM users", row(Id, Name, Email)).

:- at_halt(done).
done :-
    nb_getval(add_user, AddUser),
    nb_delete(add_user),
    odbc_free_statement(AddUser),
    odbc_disconnect(mcclass).

