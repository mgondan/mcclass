% Collect feedback from Flags
:- module(feedback, [praise/4, blame/4, feedback/4]).

% This may become more complex if we change the representation.
praise(Task, Flags, Col, Praise) :-
    findall(P, (member(expert(N, Args), Flags), feedback(Task, expert(N, Args), Col, P)), Praise).

blame(Task, Flags, Col, Blame) :-
    findall(B, (member(bug(N, Args), Flags), feedback(Task, bug(N, Args), Col, B)), Blame).

feedback(Task, Flags, Col, Feedback) :-
    praise(Task, Flags, Col, Praise),
    blame(Task, Flags, Col, Blame),
    append(Praise, Blame, Feedback).

