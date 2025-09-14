:-  module(r_session, 
      [ r//1, r_topic_source/0, r_topic/1, r_topic/2, r_topic//1, r_topic_source_with_check/0]).

:-  reexport(library(r)).
:-  use_module(session).

% Evaluate R expression and render it as html
r(Expr)
--> { r(Expr, R) },
    term_string(R, S),
    html(S).

r_topic(Expr),
    b_getval(topic, Topic),
    b_getval(variant, Variant)
 => r(with(Topic, with(Variant, Expr))).

r_topic(Expr, Res),
    b_getval(topic, Topic),
    b_getval(variant, Variant)
 => r(with(Topic, with(Variant, Expr)), Res).

r_topic(Expr)
--> { r_topic(Expr, Res) },
    term_string(Res, String),
    html(String).

% Load a topic into the current session
r_topic_source
 => r_topic,
    b_getval(topic, Topic),
    format(string(S), "~w.R", [Topic]),
    absolute_file_name(S, Abs),
    atom_string(Abs, String),
    r_topic(source(String, local=true)).

:- dynamic topic/1, variant/2.

r_topic :-
    b_getval(topic, Topic),
    (   topic(Topic)
    ;   r('<-'(Topic, 'new.env'())),
	assert(topic(Topic))
    ),  !, 
    b_getval(variant, Variant),
    (   variant(Topic, Variant)
    ;   r(with(Topic, '<-'(Variant, 'new.env'()))),
        assert(variant(Topic, Variant))
    ),  !.

% Load .R topic file only once per variant (not multiple times for each task) 
r_topic_source_with_check :-
    b_getval(topic, Topic),
    topic(Topic),
    b_getval(variant, Variant),
    variant(Topic, Variant),
    !.

r_topic_source_with_check :-
    r_topic_source.

