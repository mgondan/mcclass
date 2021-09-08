% Collect feedback from Flags
:- module(feedback, [praise/3, blame/3, feedback/3]).

% This may become more complex if we change the representation.
praise(Flags, Col, Praise) :-
    findall(P, (member(expert(N, Args), Flags), feedback(_, expert(N, Args), Col, P)), Praise).

blame(Flags, Col, Blame) :-
    findall(B, (member(bug(N, Args), Flags), feedback(_, bug(N, Args), Col, B)), Blame).

feedback(Flags, Col, Feedback) :-
    praise(Flags, Col, Praise),
    blame(Flags, Col, Blame),
    append(Praise, Blame, Feedback).

