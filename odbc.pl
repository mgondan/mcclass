
:- initialization(run, after_load).
run :-
    odbc_connect(mcclass, _, [alias(mcclass)]),
    odbc_prepare(mcclass, 'INSERT INTO users (id, name, email) VALUES (?, ?, ?)', [default, default, default], AddUser),
    nb_setval(add_user, AddUser).

put(Id, Name, Email) :-
    nb_getval(add_user, AddUser),
    odbc_execute(AddUser, [Id, Name, Email]).

get :-
    odbc_query(mcclass, "SELECT name, email FROM users ORDER BY id DESC",
      Users, [findall(user(Name, Email), user(Name, Email))]),
    writeln(Users).

:- at_halt(done).
done :-
    nb_getval(add_user, AddUser),
    nb_delete(add_user),
    odbc_free_statement(AddUser),
    odbc_disconnect(mcclass).

