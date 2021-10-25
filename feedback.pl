% Collect feedback from Flags
:- module(feedback, [praise/4, blame/4, hints/4]).

% This may become more complex if we change the representation.
praise(Task, Flags, Col, Praise) :-
    findall(P, (member(step(expert, N, Args), Flags), feedback(Task, N, Args, Col, P)), Praise).

blame(Task, Flags, Col, Blame) :-
    findall(B, (member(step(buggy, N, Args), Flags), feedback(Task, N, Args, Col, B)), Blame).

hints(Task, Flags, Col, Hints) :-
    findall(FB, (member(step(expert, N, Args), Flags), hint(Task, N, Args, Col, FB)), Hints).

