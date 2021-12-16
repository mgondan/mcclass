:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile download/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

download(tgroups2, File) :-
    session_tmpfile(File),
    r_task(tgroups2, download(File)).

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

render(tgroups2, item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
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
		p(class('card-text'),
		  [ "Compare the OSATS-Scores of both Groups, assuming homogeneity of variance."
 		  ]), 
		 form(method('POST'),
		    button([ class('btn btn-secondary'), name(download), value(tgroups2) ], "Download data"))
	      ])),
	    div(class(card), div(class('card-body'),
	    [ h4(class('card-title'), [a(id(question), []), "Question"]),
	      p(class('card-text'),
		[ "Calculate the the pooled Variance ", \mmlm(s2p)
		]),
	      form([class(form), method('POST'), action('#tgroups2-indep')],
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
start(tgroups2, item(vr, s_vr, n_vr, box, s_box, n_box)).


%
% This Task should be completed before tgroups.pl as it only covers the first 
% half of the necessarry calculations to solve a full t-test.
%
% Correctly calculated the pooled variance.
expert(tgroups2, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(_VR, S_VR, N_VR, _BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, dfrac((N_VR - 1) * S_VR ^ 2 + (N_BOX - 1) * S_BOX ^ 2, 
			   N_VR + N_BOX - 2)) ;
	   s2p
	 }.

feedback(tgroups2, indep, [], _Col, FB) :-
    FB = [ "You correctly reportet the pooled variance." ].

hint(tgroups2, indep, [], _Col, FB) :-
    FB = [ "Try to do everthing correctly." ].

% 1) Used standard deviation instead of variance.
buggy(tgroups2, stage(2), From, To, [step(buggy, sd, [S_VR, S_BOX])]) :-
    From = dfrac(A * S_VR ^ 2 + B * S_BOX ^ 2, C),
    To = dfrac(A * instead(bug(sd), S_VR, S_VR ^ 2) + B * instead(bug(sd), S_BOX, S_BOX ^ 2), C).

feedback(tgroups2, sd, [S_VR, S_BOX], Col, FB) :-
    FB = [ "Please remember to use the squares of ", 
	   \mmlm(Col, color(sd, S_VR)), " and ", 
	   \mmlm(Col, color(sd, S_BOX)), " in the pooled variance." ].

hint(tgroups2, sd, [_S_VR, _S_BOX], _Col, FB) :-
    FB = [ "Try using the variance rather than the standard deviation ",
	   "when calculating the pooled variance." ].

% 2) Forgot parentheses around numerator and denominator.
buggy(tgroups2, stage(2), From, To, [step(buggy, bug1, [N_VR, N_BOX, S_VR, S_BOX])]) :-
    From = dfrac((N_VR - 1) * S_VR ^ 2 + (N_BOX - 1) * S_BOX ^ 2, 
		 N_VR + N_BOX - 2),
    To = invent_right(bug(bug1), invent_left(bug(bug1), color(bug1, N_VR - 1 * S_VR ^ 2 + N_BOX) - 
	 invent_left(bug(bug1), 1 * dfrac(S_BOX ^ 2, N_VR))) + (N_BOX - 2)).

feedback(tgroups2, bug1, [N_VR, N_BOX, S_VR, S_BOX], Col, FB) :-
    FB = [ "Please do not forget the parentheses around the numerator and ",
	   "the denominator of a fraction, ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", 
	   color(bug1, paren(color("#000000", N_VR - 1))) * S_VR ^ 2 + 
	   color(bug1, paren(color("#000000", N_BOX - 1))) * S_BOX ^ 2))),
	   color(bug1, paren(color("#000000", N_VR + N_BOX - 2)))))
	 ].

hint(tgroups2, bug1, [N_VR, N_BOX, S_VR, S_BOX], Col, FB) :-
    FB = [ "Do not forget the parentheses! The correct formula is ",
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", 
	   color(bug1, paren(color("#000000", N_VR - 1))) * S_VR ^ 2 + 
	   color(bug1, paren(color("#000000", N_BOX - 1))) * S_BOX ^ 2))),
	   color(bug1, paren(color("#000000", N_VR + N_BOX - 2)))))
	 ].

% 3) Swapped N_VR and N_Box.
buggy(tgroups2, stage(1), From, To, [step(buggy, nswap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(vr, s_vr, color(nswap, n_box), box, s_box, color(nswap, n_vr)).

feedback(tgroups2, nswap, [], Col, FB) :-
    FB = [ "Please double check the sample sizes ", \mmlm(Col, color(nswap, n_vr)), 
	   " and ", \mmlm(Col, color(nswap, n_box)), " of both groups." ].

hint(tgroups2, nswap, [], Col, FB) :-
    FB = [ "Do not swap the sample sizes in ", \mmlm(Col, color(nswap, s2p)) ].

