:- module(salt, [salt/1]).

salt(Salt) :-
    nb_current(salt, S),
    !,
    Salt = S.

salt(Salt) :-
    exists_file(".salt"),
    open(".salt", read, In),
    read(In, S),
    close(In),
    !,
    string_bytes(Salt, S, utf8),
    nb_setval(salt, Salt).

salt(Salt) :-
    crypto_n_random_bytes(32, S),
    open(".salt", write, Out),
    write_term(Out, S, [fullstop(true), nl(true)]),
    close(Out),
    string_bytes(Salt, S, utf8).

