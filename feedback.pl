% Collect feedback from Flags
:- module(feedback, [praise/4, blame/4]).

% This may become more complex if we change the representation.
praise(Task, Flags, Col, Praise) :-
    findall(P, (member(step(expert, N, Args), Flags), Task:feedback(N, Args, Col, P)), Praise).

blame(Task, Flags, Col, Blame) :-
    findall(B, (member(step(buggy, N, Args), Flags), Task:feedback(N, Args, Col, B)), Blame).

