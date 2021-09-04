% Check dependencies in flags
:- module(depends, [dependencies/1]).

% Check if there is a name(X) or bug(X) for each depends(X)
dependencies(Flags) :-
    findall(D, member(depends(D), Flags), Dependencies),
    findall(D, (member(name(D), Flags) ; member(bug(D), Flags)), Bugs),
    subtract(Dependencies, Bugs, []).

