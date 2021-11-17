%:- module(chisq,
%       	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.


init(chisq) :-
    r_session_source(chisq).

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, p_VR, Flags, sub(p, "VR")).
mathml:hook(Flags, p_Box, Flags, sub(p, "Box")).
mathml:hook(Flags, s_VR, Flags, sub(s, "VR")).
mathml:hook(Flags, s_Box, Flags, sub(s, "Box")).
mathml:hook(Flags, n_VR, Flags, sub(n, "VR")).
mathml:hook(Flags, n_Box, Flags, sub(n, "Box")).
mathml:hook(Flags, chi2, Flags, chi^2).
mathml:hook(Flags, p_pool, Flags, sub(p, "pool")).

%
% R constants
%
interval:hook(pl, p_VR, r(p_VR)).
interval:hook(pl, s_VR, r(s_VR)).
interval:hook(pl, n_VR, r(n_VR)).
interval:hook(pl, p_Box, r(p_Box)).
interval:hook(pl, s_Box, r(s_Box)).
interval:hook(pl, n_Box, r(n_Box)).
interval:hook(pl, chi2, r(chi2)).
interval:hook(pl, p_pool, r(p_pool)).

render(chisq, item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box), Form) -->
    {option(resp(R), Form, '#.##') },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Training of surgical skills"),
            p(class('card-text'),
              [ "Surgeons need special motor skills, especially for ",
                "endoscopic surgery through the belly. Nickel et al. (2015) ",
                "report the results of a study with two learning methods for ",
                "motor skill training. One group underwent a virtual reality ",
                "training (VR group), the other group participated in a ",
                "mixture of online courses and classical training of motor ",
                "skill with the so-called Box-trainer (Box group). ",
                "The primary dependent variable is the result on the OSATS ",
                "test (interval scaled, normally distributed, high scores = ",
                "good performance). A few more dependent variables were ",
                "assessed, including a knowledge test (interval scaled), ",
                "operation time (dichotomized, above or below 80 min), and ",
                "efficiency ratings (ordinal scale, 1=bad ... 5=good)."
              ]),
            p(class('card-text'),
              [ "Here we look at the speed of the operation. The publication ",
                "states the following:"
              ]),
            p(class('card-text'),
              [ "“Laparoscopy-naïve medical students were randomized into ",
                "two groups. (...) The VR group completed the operation more ",
                "often within 80 min than the Box ",
                "group ", \mmlm(["(", (r(P_VR*100)), "%"]), " vs. ", 
		\mmlm([(r(P_Box*100)), "%)."]), " The percentages ",
                "correspond to ", \mmlm([round(0)], r(S_VR)), " people (out ",
                "of ", \mmlm([round(0)], [r(N_VR), ")"]), " in the VR group ",
                "and ", \mmlm([round(0)], r(S_Box)), " people (out ",
                "of ", \mmlm([round(0)], [r(N_Box), ")"]), " in the Box group. "
              ])
	  ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'), 
              [ "Does VR training lead to faster surgery times than ",
		"traditional Box training?"
	      ]),
	    p(class('card-text'),
	      [ "Please determine the ", \mmlm(hyph(chi^2, "statistic."))
              ]),
	    form([class(form), method('POST'), action('#chisq-reverse')],
	      [ div(class("input-group mb-3"),
                  [ div(class("input-group-prepend"), 
                      span(class("input-group-text"), "Response")),
                    input([class("form-control"), type(text), name(resp), value(R)]),
                      div(class("input-group-append"),
                        button([class('btn btn-primary'), type(submit)], "Submit"))
              ])])
          ]))
      ]).

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% chi squared test.
intermediate(_, item).
start(chisq, item(p_VR, s_VR, n_VR, p_Box, s_Box, n_Box)):-
    init(oddsratio).

% Correct soultion.
expert(chisq, stage(2), From, To, [step(expert, allinone, [])]) :-
    From = item(P_VR, S_VR, N_VR, P_Box, S_Box, N_Box),
    To = { '<-'(p_pool, dfrac(S_VR + S_Box, N_VR + N_Box)) ;
	   '<-'(chi2, dfrac((P_VR - P_Box) ^ 2, 
		p_pool * (1 - p_pool) * (1 / N_VR + 1 / N_Box)));
	    chi2
	 }.

feedback(chisq, allinone, [], _Col, FB) :-
    FB = [ "Everything done correctly." ].

hint(chisq, allinone, [], _Col, FB) :-
    FB = [ "Try to do everything correctly." ].
