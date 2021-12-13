%:- module(power,
%	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(power) :-
    r_session_source(power).

data(power, File) :-
    session_tmpfile(File),
    r_session(power_data(File)).

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, n_vr, Flags, sub(n, "VR")).
mathml:hook(Flags, n_box, Flags, sub(n, "BOX")).
mathml:hook(Flags, vr, Flags, overline("VR")).
mathml:hook(Flags, s_vr, Flags, sub(s, "VR")).
mathml:hook(Flags, box, Flags, overline("BOX")).
mathml:hook(Flags, s_box, Flags, sub(s, "BOX")).
mathml:hook(Flags, s2p, Flags, sub(s, "pool")^2).

% Obtain information from R
interval:hook(pl, n_vr, r(n_vr)).
interval:hook(pl, n_box, r(n_box)).
interval:hook(pl, vr, r(vr)).
interval:hook(pl, s_vr, r(s_vr)).
interval:hook(pl, box, r(box)).
interval:hook(pl, s_box, r(s_box)).
interval:hook(pl, s2p, r(s2p)).
interval:hook(pl, t, r(t)).

render(power, item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
    { option(resp(R), Form, '#.##') },
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
		  [ "Please check the following text from the publication ",
		    "(40 ± 10 means “average 40, standard deviation 10”):"
		  ]),
		p(class('card-text'),
		  [ "“Laparoscopy-naïve medical students were randomized into ",
		    "two groups. The Box group (", 
		    \mmlm([round(0)], N_BOX = r(n_box)), ") used E-learning for ", 
		    "laparoscopic cholecystectomy and practiced ",
		    "basic skills with Box trainers. The VR group (", 
		    \mmlm([round(0)], N_VR = r(n_vr)), ") trained ",
		    "basic skills and laparoscopic cholecystectomy on ",
		    "LAP Mentor II (Simbionix, Cleveland, USA). Each group ",
		    "trained 3 × 4 hours followed by a knowledge test. Blinded ",
		    "raters assessed the operative performance using the ",
		    "Objective Structured Assessment of Technical Skills ",
		    "(OSATS). The VR group completed the operation significantly ",
		    "faster and more often within 80 min than the Box ",
		    "group (VR: 28% vs. Box: 22%, p = 0.596). The Box group ",
		    "scored higher than the VR group in the knowledge ",
		    "test (Box: 13.4 ± 1.2 vs. VR: 10.8 ± 1.8, p < 0.001). Both ",
		    "groups showed equal operative performance in the OSATS score ",
		    "(VR: ", \mmlm([round(1)], r(vr)), " ± ", \mmlm([round(1)], r(s_vr)), 
		    " vs. BOX: ", \mmlm([round(1)], r(box)), " ± ", \mmlm([round(1)], r(s_box)), 
		    ", p = 0.437). The significance level is set to ",
		    \mmlm(alpha = [5, "%"]), " two-tailed. ",
		    "Students generally liked training and felt well prepared for ", 
		    "assisting in laparoscopic surgery. The efficiency of the training ",
		    "was judged higher by the VR group than by the Box group."
		  ]), 
		 form(method('POST'),
		    button([ class('btn btn-secondary'), name(download), value(power) ], "Download data"))
	      ])),
	    div(class(card), div(class('card-body'),
	    [ h4(class('card-title'), [a(id(question), []), "Question"]),
	      p(class('card-text'),
		[ "Is VR training superior to traditional Box training?"
		]),
	      form([class(form), method('POST'), action('#power-pnorm')],
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

% t-test for independent groups
intermediate(_, item).
start(power, item(vr, s_vr, n_vr, box, s_box, n_box)).

% Correctly identified the problem as a t-test for independent groups.
intermediate(power, indep).
expert(power, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX)) ;
	   '<-'(t, dfrac(VR - BOX, sqrt(s2p * (1/N_VR + 1/N_BOX)))) ;
	   t
	 }.

feedback(power, indep, [], Col, FB) :-
    FB = [ "You identified the problem as a ", \mmlm(Col, hyph(t, "test")),
	   " for independent samples and solved it correctly." ].

hint(power, indep, [], _Col, FB) :-
    FB = [ "Try to do everthing correctly." ].

% 1
