:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, n_vr, [task(tgroupsdf) | Flags], sub(n, "VR")).
mathml:hook(Flags, n_box, [task(tgroupsdf) | Flags], sub(n, "BOX")).
mathml:hook(Flags, vr, [task(tgroupsdf) | Flags], overline("VR")).
mathml:hook(Flags, s_vr, [task(tgroupsdf) | Flags], sub(s, "VR")).
mathml:hook(Flags, box, [task(tgroupsdf) | Flags], overline("BOX")).
mathml:hook(Flags, s_box, [task(tgroupsdf) | Flags], sub(s, "BOX")).
mathml:hook(Flags, nt, [task(tgroupsdf) | Flags], sub(n, "total")).

% Obtain information from R
interval:r_hook(n_vr).
interval:r_hook(n_box).
interval:r_hook(vr).
interval:r_hook(s_vr).
interval:r_hook(box).
interval:r_hook(s_box).
interval:r_hook(nt).
interval:r_hook(df).

render(tgroupsdf, item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
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
		    \mmlm([task(tgroupsdf), digits(0)], N_BOX = r(n_box)), ") used E-learning for ", 
		    "laparoscopic cholecystectomy and practiced ",
		    "basic skills with Box trainers. The VR group (", 
		    \mmlm([task(tgroupsdf), digits(0)], N_VR = r(n_vr)), ") trained ",
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
		    "(VR: ", \mmlm([task(tgroupsdf), digits(1)], r(vr)), " ± ", \mmlm([task(tgroupsdf), digits(1)], r(s_vr)), 
		    " vs. BOX: ", \mmlm([task(tgroupsdf), digits(1)], r(box)), " ± ", \mmlm([task(tgroupsdf), digits(1)], r(s_box)), 
		    ", p = 0.437). The significance level is set to ",
		    \mmlm([task(tgroupsdf)], alpha = [5, "%"]), " two-tailed. ",
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

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% t-test for independent groups
intermediate(_, item).
start(tgroupsdf, item(vr, s_vr, n_vr, box, s_box, n_box)).

% Correctly identified the problem as a t-test for independent groups.
expert(tgroupsdf, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX),
    To = { '<-'(df, N_VR + N_BOX - 2) ;
	   df
	 }.

feedback(tgroupsdf, indep, [], _Col, FB) =>
    FB = [ "You correctly calculated the degrees of freedom." ].

hint(tgroupsdf, indep, [], _Col, FB) =>
    FB = [ "Try to do everthing correctly." ].

% 1) subtracted 1 rather than 2
buggy(tgroupsdf, stage(2), From, To, [step(buggy, one, [])]) :-
    From = N_VR + N_BOX - 2,
    To = N_VR + N_BOX - color(one, 1).

feedback(tgroupsdf, one, [], Col, FB) =>
    FB = [ "Please remember to subtract ", \mmlm(Col, 1), 
	   \mmlm(Col, color(one, " per")), " sample." ].

hint(tgroupsdf, one, [], Col, FB) =>
    FB = [ "Subtract ", \mmlm(Col, color(one, 2)), " instead of ", 
	   \mmlm(Col, color(one, 1)) ].

% 2) used only the sample size of N_VR.
buggy(tgroupsdf, stage(2), From, To, [step(buggy, singlen, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(bug(singlen), N_VR + N_BOX) - 2.
buggy(tgroupsdf, stage(2), From, To, [step(buggy, singlen2, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_left(bug(singlen2), N_VR + N_BOX) - 2.

feedback(tgroupsdf, singlen, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", \mmlm(Col, color(singlen, N_VR + N_BOX)) ].

feedback(tgroupsdf, singlen2, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", \mmlm(Col, color(singlen2, N_VR + N_BOX)) ].

hint(tgroupsdf, singlen, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", \mmlm(Col, color(singlen, N_VR + N_BOX)) ].
hint(tgroupsdf, singlen2, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", \mmlm(Col, color(singlen2, N_VR + N_BOX)) ].

% 3) and again, but with -1.
buggy(tgroupsdf, stage(2), From, To, [step(buggy, singlen3, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_left(bug(singlen3), N_VR + N_BOX) - color(singlen3, 1).
buggy(tgroupsdf, stage(2), From, To, [step(buggy, singlen4, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(bug(singlen4), N_VR + N_BOX) - color(singlen4, 1).

feedback(tgroupsdf, singlen3, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", 
	   \mmlm(Col, color(singlen3, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen3, " per")), " sample."].
feedback(tgroupsdf, singlen4, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please remember to add up ", 
	   \mmlm(Col, color(singlen4, N_VR + N_BOX)), " and to subtract ",
	   \mmlm(Col, 1), \mmlm(Col, color(singlen4, " per")), " sample."].

hint(tgroupsdf, singlen3, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ", 
	   \mmlm(Col, color(singlen3, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen3, " per")), " sample." ].
hint(tgroupsdf, singlen4, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to add up ",
	   \mmlm(Col, color(singlen4, N_VR + N_BOX)), " and to subtract ", 
	   \mmlm(Col, 1), \mmlm(Col, color(singlen4, " per")), " sample." ].

% 4) Gguessed that there is one degree of freedom per group.
buggy(tgroupsdf, stage(2), From, To, [step(buggy, guess, [From])]) :-
    From = N_VR + N_BOX - 2,
    To = instead(bug(guess), 2.00, N_VR + N_BOX - 2).

feedback(tgroupsdf, guess, [_From], Col, FB) =>
    FB = [ "While the ", \mmlm(Col, "df"), " depend on the number of groups ",
	   " they are more than just the number of groups." ].

hint(tgroupsdf, guess, [From], Col, FB) =>
    FB = [ "The correct formula for ", \mmlm(Col, "df"), " is ", 
	   \mmlm(Col, color(guess, From)) ].

% 5) Forgot to subtract 2.
buggy(tgroupsdf, stage(2), From, To, [step(buggy, nosub, [N_VR, N_BOX])]) :-
    From = N_VR + N_BOX - 2,
    To = omit_right(bug(nosub), (N_VR + N_BOX) - 2).

feedback(tgroupsdf, nosub, [_N_VR, _N_BOX], Col, FB) =>
    FB = [ "Please remember to subtract ", \mmlm(Col, color(nosub, 2)), 
	   " from the sum of test subjects ." ].
hint(tgroupsdf, nosub, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget to subtract ", \mmlm(Col, color(nosub, 2)), 
	   " from ", \mmlm(Col, N_VR + N_BOX) ].
