:- module(tgroups, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(tgroups, ["Independent ", i(t), "-test (1)"]).

:- discontiguous intermediate/1, expert/4, buggy/4, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
mathml_hook(n_vr, sub(n, "VR")).
mathml_hook(n_box, sub(n, "BOX")).
mathml_hook(vr, overline("VR")).
mathml_hook(s_vr, sub(s, "VR")).
mathml_hook(box, overline("BOX")).
mathml_hook(s_box, sub(s, "BOX")).
mathml_hook(s2p, sub(s, "pool")^2).

% Obtain information from R
interval:r_hook(n_vr).
interval:r_hook(n_box).
interval:r_hook(vr).
interval:r_hook(s_vr).
interval:r_hook(box).
interval:r_hook(s_box).
interval:r_hook(s2p).
interval:r_hook(t).

render(item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
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
		    \mmlm(N_BOX = r(n_box)), ") used E-learning for ", 
		    "laparoscopic cholecystectomy and practiced ",
		    "basic skills with Box trainers. The VR group (", 
		    \mmlm(N_VR = r(n_vr)), ") trained ",
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
		    "(VR: ", \mmlm([digits(1)], r(vr)), " ± ", \mmlm([digits(1)], r(s_vr)), 
		    " vs. BOX: ", \mmlm([digits(1)], r(box)), " ± ", \mmlm([digits(1)], r(s_box)), 
		    ", p = 0.437). The significance level is set to ",
		    \mmlm(alpha = [5, "%"]), " two-tailed. ",
		    "Students generally liked training and felt well prepared for ", 
		    "assisting in laparoscopic surgery. The efficiency of the training ",
		    "was judged higher by the VR group than by the Box group."
		  ]), 
		 form(method('POST'),
		    button([ class('btn btn-secondary'), name(download), value(tgroups) ], "Download data"))
	      ])),
	    div(class(card), div(class('card-body'),
	    [ h4(class('card-title'), [a(id(question), []), "Question"]),
	      p(class('card-text'),
		[ "Is VR training superior to traditional Box training?"
		]),
	      form([class(form), method('POST'), action('#tgroups-pnorm')],
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
intermediate(var_pool_).
intermediate(tcalc).
expert(stage(1), From, To, [step(expert, indep, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX)) ;
	   '<-'(t, tcalc(VR, BOX, s2p, N_VR, N_BOX))
	 }.

feedback(indep, [], Col, FB) =>
    FB = [ "You identified the problem as a ", \mmlm(Col, hyph(t, "test")),
	   " for independent samples with all its main steps." ].

hint(indep, [], Col, FB) =>
    FB = [ "First determine the pooled variance, then the ", \mmlm(Col, hyph(t, "statistic.")) ].

% Correctly calculated the pooled variance.
expert(stage(1), From, To, [step(expert, varpool, [To])]) :-
    From = var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX),
    To = var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX).

feedback(varpool, [_To], Col, FB) =>
    FB = [ "You correctly determined the pooled variance ", \mmlm(Col, s2p) ].

hint(varpool, [To], Col, FB) =>
    FB = [ "The pooled variance is ", \mmlm(Col, To) ].

% Correctly calculated t.
expert(stage(2), From, To, [step(expert, tcalc, [To])]) :-
    From = tcalc(VR, BOX, s2p, N_VR, N_BOX),
    To = dfrac(VR - BOX, sqrt(s2p * (1/N_VR + 1/N_BOX))).

feedback(tcalc, [_To], Col, FB) =>
    FB = [ "You correctly determined the ",  \mmlm(Col, hyph(t, "statistic.")) ].

hint(tcalc, [To], Col, FB) =>
    FB = [ "Use the following formula to calculate the ", 
	   \mmlm(Col, hyph(t, "statistic:")), " ", \mmlm(Col, To) ].

% 1) Swap vr and box groups.
buggy(stage(1), From, To, [step(buggy, swap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(color(swap, box), s_vr, n_vr, color(swap, vr), s_box, n_box).

feedback(swap, [], Col, FB) =>
    FB = [ "You swapped the ", \mmlm(Col, color(swap, "VR")), " and ",
	   \mmlm(Col, color(swap, "BOX")), " variables." ].

hint(swap, [], _Col, FB) =>
    FB = [ "Do not switch the VR groups variables with ",
	   "those of the Box group." ].

% 2) Forgot to use square of standard deviation in pooled variance.
buggy(stage(1), From, To, [step(buggy, sqsd, [S_VR, S_BOX])]) :-
    From = var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX),
    To = var_pool(instead(sqsd, S_VR, S_VR ^ 2), N_VR, 
	instead(sqsd, S_BOX, S_BOX ^ 2), N_BOX).

feedback(sqsd, [S_VR, S_BOX], Col, FB) =>
    FB = [ "You need to square ", \mmlm(Col, color(sqsd, S_VR)), " and ", 
	   \mmlm(Col, color(sqsd, S_BOX)) ].

hint(sqsd, [_S_VR, _S_BOX], _Col, FB) =>
    FB = [ "Use the variance rather than the standard deviation ",
	   "when calculating the pooled variance." ].

% 3) Forgot school math.
buggy(stage(2), From, To, [step(buggy, school, [From, To])]) :-
    From = 1 / N_VR + 1 / N_BOX,
    To = color(school, frac(1, N_VR + N_BOX)).

feedback(school, [From, To], Col, FB) =>
    FB = [ "Keep in mind that ", 
	   \mmlm(Col, color(school, From) =\= To) ].

hint(school, [From, To], Col, FB) =>
    FB = [ "Do not forget that ", 
	   \mmlm(Col, color(school, From) =\= To) ].

% 4) Forgot paranthesis around numerator in t-statistic.
buggy(stage(2), From, To, [step(buggy, bug1, [VR, BOX, S2P, N_VR, N_BOX])]) :-
    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
    To = invent_left(bug1, VR - dfrac(BOX, sqrt(S2P * (1/N_VR + 1/N_BOX)))).
%VR - dfrac(BOX, sqrt(s2p * (1/N_VR + 1/N_BOX))).

feedback(bug1, [_VR, _BOX, S2P, N_VR, N_BOX], Col, FB) =>
    FB = [ "You forgot the parentheses around the numerator of ",
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", overline("VR") - overline("BOX")))), 
	   sqrt(S2P * (1/N_VR + 1/N_BOX) ) ) )
	 ].

hint(bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) =>
    FB = [ "Remember to use parenthesis around numerator. ",
	   "The correct formula for the ", \mmlm(Col, hyph(t, "ratio")), "is ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
	   sqrt(S2P * (1/N_VR + 1/N_BOX) ) ) )
	 ].

% 5) Forgot square root.
buggy(stage(2), From, To, [step(buggy, nosr1, [A])]) :-
    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
    A = S2P * (1/N_VR + 1/N_BOX),
    To = dfrac(VR - BOX, instead(nosr1, A, sqrt(A))).

feedback(nosr1, [A], Col, FB) =>
   FB = [ "You forgot to take the square root of ", \mmlm(Col, color(nosr1, A)) ].

hint(nosr1, [A], Col, FB) =>
    FB = [ "Keep in mind that you need to take the square root of ", 
	   \mmlm(Col, color(nosr1, A)) ].


% 6) Only took the square root of the first element of the denominator.
buggy(stage(2), From, To, [step(buggy, nosr2, [S2P, N_VR, N_BOX])]) :-
    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
    A = sqrt(S2P * (1/N_VR + 1/N_BOX)),
    To = dfrac(VR - BOX, instead(nosr2, sqrt(S2P) * (1/N_VR + 1/N_BOX), A)).

feedback(nosr2, [_, _, _], Col, FB) =>
   FB = [ "You took the square root of only ", \mmlm(Col, color(nosr2, s2p)),
	  " instead of the square root of the entire denominator." ].

hint(nosr2, [S2P, N_VR, N_BOX], Col, FB) =>
   FB = [ "Keep in mind that you need to take the square root of ", 
	   \mmlm(Col, color(nosr2, S2P * (1/N_VR + 1/N_BOX))), " in its entirety." ].

