:- use_module(library(http/http_unix_daemon)).
:- initialization(run, main).

run :-
    setup_call_cleanup(
        odbc_connect(dbname, _, [alias(dbname)]),
        http_daemon,
        odbc_disconnect(dbname)
    ).

