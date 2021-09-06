% Collect feedback from Flags
:- module(feedback, [praise/2, blame/2, feedback/2]).

% This may become more complex if we change the representation.
praise(Flags, Praise) :-
    findall(P, (member(name(N), Flags), member(praise(N, P), Flags)), Praise).

blame(Flags, Blame) :-
    findall(B, (member(bug(N), Flags), member(blame(N, B), Flags)), Blame).

feedback(Flags, Feedback) :-
    praise(Flags, Praise),
    blame(Flags, Blame),
    append(Praise, Blame, Feedback).

