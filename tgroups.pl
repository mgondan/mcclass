:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, n_vr, [task(tgroups) | Flags], sub(n, "VR")).
mathml:hook(Flags, n_box, [task(tgroups) | Flags], sub(n, "BOX")).
mathml:hook(Flags, vr, [task(tgroups) | Flags], overline("VR")).
mathml:hook(Flags, s_vr, [task(tgroups) | Flags], sub(s, "VR")).
mathml:hook(Flags, box, [task(tgroups) | Flags], overline("BOX")).
mathml:hook(Flags, s_box, [task(tgroups) | Flags], sub(s, "BOX")).
mathml:hook(Flags, s2p, [task(tgroups) | Flags], sub(s, "pool")^2).

% Obtain information from R
interval:r_hook(n_vr).
interval:r_hook(n_box).
interval:r_hook(vr).
interval:r_hook(s_vr).
interval:r_hook(box).
interval:r_hook(s_box).
interval:r_hook(s2p).
interval:r_hook(t).

render(tgroups, item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX), Form) -->
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
		    \mmlm([task(tgroups), digits(0)], N_BOX = r(n_box)), ") used E-learning for ", 
		    "laparoscopic cholecystectomy and practiced ",
		    "basic skills with Box trainers. The VR group (", 
		    \mmlm([task(tgroups), digits(0)], N_VR = r(n_vr)), ") trained ",
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
		    "(VR: ", \mmlm([task(tgroups), digits(1)], r(vr)), " ± ", \mmlm([task(tgroups), digits(1)], r(s_vr)), 
		    " vs. BOX: ", \mmlm([task(tgroups), digits(1)], r(box)), " ± ", \mmlm([task(tgroups), digits(1)], r(s_box)), 
		    ", p = 0.437). The significance level is set to ",
		    \mmlm([task(tgroups)], alpha = [5, "%"]), " two-tailed. ",
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

% Prolog warns if the rules of a predicate are not adjacent. This
% does not make sense here, so the definitions for intermediate, expert
% and buggy are declared to be discontiguous.
:- multifile intermediate/2, expert/5, buggy/5.

% t-test for independent groups
intermediate(_, item).
start(tgroups, item(vr, s_vr, n_vr, box, s_box, n_box)).

% Correctly identified the problem as a t-test for independent groups.
intermediate(tgroups, var_pool_).
intermediate(tgroups, tcalc).
expert(tgroups, stage(1), From, To, [step(expert, indep, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX)) ;
	   '<-'(t, tcalc(VR, BOX, s2p, N_VR, N_BOX)) ;
	   t
	 }.

feedback(tgroups, indep, [], Col, FB) =>
    FB = [ "You identified the problem as a ", \mmlm(Col, hyph(t, "test")),
	   " for independent samples along with all main steps." ].

hint(tgroups, indep, [], Col, FB) =>
    FB = [ "First determine the pooled variance, then the ", \mmlm(Col, hyph(t, "statistic.")) ].

% Correctly calculated the pooled variance.
expert(tgroups, stage(1), From, To, [step(expert, varpool, [To])]) :-
    From = var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX),
    To = var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX).

feedback(tgroups, varpool, [_To], _Col, FB) =>
    FB = [ "You correctly determined the pooled variance." ].

hint(tgroups, varpool, [To], Col, FB) =>
    FB = [ "The pooled variance is ", \mmlm(Col, To) ].

% Correctly calculated t.
expert(tgroups, stage(2), From, To, [step(expert, tcalc, [To])]) :-
    From = tcalc(VR, BOX, s2p, N_VR, N_BOX),
    To = dfrac(VR - BOX, sqrt(s2p * (1/N_VR + 1/N_BOX))).

feedback(tgroups, tcalc, [_To], Col, FB) =>
    FB = [ "You correctly determined the ",  \mmlm(Col, hyph(t, "statistic.")) ].

hint(tgroups, tcalc, [To], Col, FB) =>
    FB = [ "Use the following formula to calculate the ", 
	   \mmlm(Col, hyph(t, "statistic:")), " ", \mmlm(Col, To) ].

% 1) Swap vr and box groups.
% Vincent To Do: Feedback has no corresponding colors in main rule, still needs to be unique.
buggy(tgroups, stage(1), From, To, [step(buggy, swap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(box, s_box, n_box, vr, s_vr, n_vr).

feedback(tgroups, swap, [], Col, FB) =>
    FB = [ "You swapped the ", \mmlm(Col, color(swap, "VR")), " and ",
	   \mmlm(Col, color(swap, "BOX")), " variables." ].

hint(tgroups, swap, [], _Col, FB) =>
    FB = [ "Do not switch the Variables belonging to the VR group with ",
	   "those of the Box groups." ].

% 2) Forgot to use square of standard deviation in pooled variance.
buggy(tgroups, stage(1), From, To, [step(buggy, sqsd, [S_VR, S_BOX])]) :-
    From = var_pool_(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX),
    To = var_pool(instead(bug(sqsd), S_VR, S_VR ^ 2), N_VR, 
	 instead(bug(sqsd), S_BOX, S_BOX ^ 2), N_BOX).

feedback(tgroups, sqsd, [S_VR, S_BOX], Col, FB) =>
    FB = [ "You need to square ", \mmlm(Col, color(sqsd, S_VR)), " and ", 
	   \mmlm(Col, color(sqsd, S_BOX)), " in the pooled variance." ].

hint(tgroups, sqsd, [_S_VR, _S_BOX], _Col, FB) =>
    FB = [ "Try using the variance rather than the standard deviation ",
	   "when calculating the pooled variance." ].

% 3) Forgot school math.
buggy(tgroups, stage(2), From, To, [step(buggy, school, [From, N_VR, N_BOX])]) :-
    dif(N_VR, N_BOX),
    From = 1 / N_VR + 1 / N_BOX,
    To = color(school, frac(1, N_VR + N_BOX)).

feedback(tgroups, school, [From, N_VR, N_BOX], Col, FB) =>
    FB = [ "Please do not forget school math ", 
	   \mmlm(Col, color(school, From) 
	   =\= color(school, frac(1, N_VR + N_BOX))) ].

hint(tgroups, school, [From, N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget school math ", 
	   \mmlm(Col, color(school, From) =\= color(school, frac(1, N_VR + N_BOX))) ].

% 4) Same for N1 = N2
buggy(tgroups, stage(2), From, To, [step(buggy, school2, [From, N])]) :-
    From = 1 / N + 1 / N,
    To = color(school2, frac(1, 2 * N)).

feedback(tgroups, school2, [From, N], Col, FB) =>
    FB = [ "Please do not forget school math ", 
	   \mmlm(Col, color(school2, From) =\= color(school2, frac(1, 2 * N))) ].

hint(tgroups, school2, [From, N], Col, FB) =>
    FB = [ "Do not forget school math ", 
	   \mmlm(Col, color(school2, From) =\= color(school2, frac(1, 2 * N))) ].

% 5) Forgot paranthesis.
buggy(tgroups, stage(2), From, To, [step(buggy, bug1, [VR, BOX, S2P, N_VR, N_BOX])]) :-
    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
    To = invent_left(bug(bug1), VR - 
	 invent_right(bug(bug1), dfrac(BOX, sqrt(S2P)) * (1/N_VR + 1/N_BOX))).

feedback(tgroups, bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) =>
    FB = [ "You forgot the parentheses around the numerator and ",
	   "the denominator of ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
	   color(bug1, paren(color("#000000", sqrt(S2P * (1/N_VR + 1/N_BOX) ) ))) ) )
	 ].

hint(tgroups, bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) =>
    FB = [ "Remember to use parenthesis around numerator and denominator. ",
	   "The correct formula for the ",
	   \mmlm(Col, hyph(t, "ratio")), "is ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
	   color(bug1, paren(color("#000000", sqrt(S2P * (1/N_VR + 1/N_BOX) ) ))) ) )
%	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
%	   sqrt(color(bug1, color("#000000", S2P)) * 
%	   color(bug1, paren(color("#000000", 1/N_VR + 1/N_BOX)))))) 
	 ].

% 6) Forgot square root.
buggy(tgroups, stage(2), From, To, [step(buggy, nosr1, [A])]) :-
    From = tcalc(VR, BOX, S2P, N_VR, N_BOX),
    A = S2P * (1/N_VR + 1/N_BOX),
    To = dfrac(VR - BOX, instead(bug(nosr1),A , sqrt(A))).

feedback(tgroups, nosr1, [A], Col, FB) =>
   FB = [ "You forgot to draw the square root of ", \mmlm(Col, color(nosr1, A))].

hint(tgroups, nosr1, [A], Col, FB) =>
    FB = [ "Keep in mind that you need to draw the square root of ", 
	   \mmlm(Col, color(nosr1, A))].

% 7) forgot square root and parenthesis.
% Rule is currently not selective enough
%buggy(tgroups, stage(2), From, To,  [step(buggy, nosr2, [S2P])]) :-
%    From = dfrac(BOX, sqrt(S2P)),
%    To = dfrac(BOX, color(nosr2, S2P)).

feedback(tgroups, nosr2, [_S2P], _Col, FB) =>
    FB = [ "You forgot to draw the square root of the main functions denominator."].

hint(tgroups, nosr2, [S2P], Col, FB) =>
    FB = [ "Keep in mind that you need to draw the square root of ", 
	   \mmlm(Col, color(nosr2, S2P * (1 / "n_vr" + 1 / "n_box"))) ].

% 8) Swapped N_VR and N_Box
buggy(tgroups, stage(1), From, To, [step(buggy, nswap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(vr, s_vr, color(nswap, n_box), box, s_box, color(nswap, n_vr)).

feedback(tgroups, nswap, [], Col, FB) =>
    FB = [ "You confused the sample sizes ", \mmlm(Col, color(nswap, n_vr)), 
	   " and ", \mmlm(Col, color(nswap, n_box)) ].

hint(tgroups, nswap, [], Col, FB) =>
    FB = [ "Do not swap the sample sizes in ", \mmlm(Col, color(nswap, s2p)) ].


% 6) forgot school math and parenthesis. 
% (doesn't work right now and is probably unnecessary)
%buggy(tgroups, stage(2), From, To, [step(buggy, scb1, [N_VR, N_BOX]), depends(bug1)]) :-
%    From = color(_, N_VR) + color(_, 1 / N_BOX),
%    To = color(scb1, N_VR) + omit_left(bug(scb1), 1 / N_BOX).

feedback(tgroups, scb1, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Please do not forget school math ", \mmlm([error(correct) | Col],
	   frac(1, color("#000000", N_VR)) + frac(1, color("#000000", N_BOX))
	    =\= 1 / color("#000000", N_VR) + color("#000000", N_BOX)) 
	 ].

hint(tgroups, scb1, [N_VR, N_BOX], Col, FB) =>
    FB = [ "Do not forget school math",  \mmlm([error(correct) | Col], 
	   frac(1, color("#000000", N_VR)) + frac(1, color("#000000", N_BOX))
	   =\= 1 / color("#000000", N_VR) + color("#000000", N_BOX)) 
	 ].
