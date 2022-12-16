:- module(tgroupsdf, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(r).
:- use_module(mathml).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(n_vr, sub(n, "VR")).
mathml_hook(n_box, sub(n, "Box")).
mathml_hook(vr, overline("VR")).
mathml_hook(s_vr, sub(s, "VR")).
mathml_hook(box, overline("Box")).
mathml_hook(s_box, sub(s, "Box")).
mathml_hook(n, sub('N', "total")).

% Obtain information from R
r:r_hook(n_vr).
r:r_hook(n_box).
r:r_hook(vr).
r:r_hook(s_vr).
r:r_hook(box).
r:r_hook(s_box).
r:r_hook(n).
r:r_hook(df).

render(item(VR, S_VR, N_VR, Box, S_Box, N_Box), Form) -->
    { option(resp(R), Form, '##.##') },
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
		    \mmlm(N_Box = r(N_Box)), ") used E-learning for ", 
		    "laparoscopic cholecystectomy and practiced ",
		    "basic skills with Box trainers. The VR group (", 
		    \mmlm(N_VR = r(N_VR)), ") trained ",
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
		    "(VR: ", \mmlm([digits(1)], r(VR)), " ± ", \mmlm([digits(1)], r(S_VR)), 
		    " vs. BOX: ", \mmlm([digits(1)], r(Box)), " ± ", \mmlm([digits(1)], r(S_Box)), 
		    ", p = 0.437). The significance level is set to ",
		    \mmlm(alpha = [5, "%"]), " two-tailed. ",
		    "Students generally liked training and felt well prepared for ", 
		    "assisting in laparoscopic surgery. The efficiency of the training ",
		    "was judged higher by the VR group than by the Box group."
		  ])
	      ])),
	    div(class(card), div(class('card-body'),
	    [ h4(class('card-title'), [a(id(question), []), "Question"]),
	      p(class('card-text'),
		[ "How many degrees of freedom do you have to account for?"
		]),
	      form([class(form), method('POST'), action('#tgroupsdf-indep')],
		[ div(class("input-group mb-3"),
		    [ div(class("input-group-prepend"), 
			span(class("input-group-text"), "Response")),
		      input([class("form-control"), type(text), name(resp), value(R)]),
			div(class("input-group-append"),
			  button([class('btn btn-primary'), type(submit)], "Submit"))
	        ])])
	      ]))
	]).

% t-test for independent groups
intermediate(item).
start(item(vr, s_vr, n_vr, box, s_box, n_box)).

% Correctly identified the problem as a t-test for independent groups.
expert(stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX),
    To = { '<-'(df, N_VR + N_BOX - 2) ;
	   df
	 }.

feedback(indep, [], _Col, FB) =>
    FB = [ "You correctly calculated the degrees of freedom." ].

hint(indep, [], _Col, FB) =>
    FB = [ "Try to do everthing correctly." ].

% 1) subtracted 1 rather than 2
buggy(stage(2), From, To, [step(buggy, one, [])]) :-
    From = N_VR + N_BOX - 2,
    To = N_VR + N_BOX - color(one, 1).

feedback(one, [], Col, FB) =>
    FB = [ "Please remember to subtract ", \mmlm(Col, 1), 
	   \mmlm(Col, color(one, " per")), " sample." ].

hint(one, [], Col, FB) =>
    FB = [ "Subtract ", \mmlm(Col, color(one, 2)), " instead of ", 
	   \mmlm(Col, color(one, 1)) ].

% 2) used only the sample size of N_VR.
buggy(stage(2), From, To, [step(buggy, singlen, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(singlen, N_VR + N_BOX) - 2.
buggy(stage(2), From, To, [step(buggy, singlen2, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_left(singlen2, N_VR + N_BOX) - 2.

feedback(singlen, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", \mmlm(Col, color(singlen, N_VR + N_BOX)) ].

feedback(singlen2, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", \mmlm(Col, color(singlen2, N_VR + N_BOX)) ].

hint(singlen, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", \mmlm(Col, color(singlen, N_VR + N_BOX)) ].
hint(singlen2, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", \mmlm(Col, color(singlen2, N_VR + N_BOX)) ].

% 3) and again, but with -1.
buggy(stage(2), From, To, [step(buggy, singlen3, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_left(singlen3, N_VR + N_BOX) - color(singlen3, 1).
buggy(stage(2), From, To, [step(buggy, singlen4, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(singlen4, N_VR + N_BOX) - color(singlen4, 1).

feedback(singlen3, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", 
	   \mmlm(Col, color(singlen3, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen3, " per")), " sample."].
feedback(singlen4, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", 
	   \mmlm(Col, color(singlen4, N_VR + N_BOX)), " and to subtract ",
	   \mmlm(Col, 1), \mmlm(Col, color(singlen4, " per")), " sample."].

hint(singlen3, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", 
	   \mmlm(Col, color(singlen3, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen3, " per")), " sample." ].
hint(singlen4, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ",
	   \mmlm(Col, color(singlen4, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen4, " per")), " sample." ].

% 4) Gguessed that there is one degree of freedom per group.
buggy(stage(2), From, To, [step(buggy, guess, [From])]) :-
    From = N_VR + N_BOX - 2,
    To = instead(guess, 2.00, N_VR + N_BOX - 2).

feedback(guess, [_From], Col, FB) =>
    FB = [ "While the ", \mmlm(Col, "df"), " depend on the number of groups ",
	   " they are more than just the number of groups." ].

hint(guess, [From], Col, FB) =>
    FB = [ "The correct formula for ", \mmlm(Col, "df"), " is ", 
	   \mmlm(Col, color(guess, From)) ].

% 5) Forgot to subtract 2.
buggy(stage(2), From, To, [step(buggy, nosub, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(nosub, (N_VR + N_BOX) - 2).

feedback(nosub, [_N_VR, _N_BOX], Col, FB) =>
    FB = [ "Please remember to subtract ", \mmlm(Col, color(nosub, 2)), 
	   " from the sum of test subjects ." ].
hint(nosub, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to subtract ", \mmlm(Col, color(nosub, 2)), 
	   " from ", \mmlm(Col, N_VR + N_BOX) ].

