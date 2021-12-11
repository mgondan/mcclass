%:- module(tgroups,
%	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(tgroups) :-
    r_session_source(tgroups).

data(tgroups, File) :-
    session_tmpfile(File),
    r_session(tgroups_data(File)).

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
expert(tgroups, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX)) ;
	   '<-'(t, dfrac(VR - BOX, sqrt(s2p * (1/N_VR + 1/N_BOX)))) ;
	   t
	 }.

feedback(tgroups, indep, [], Col, FB) :-
    FB = [ "You identified the problem as a ", \mmlm(Col, hyph(t, "test")),
	   " for independent samples and solved it correctly." ].

hint(tgroups, indep, [], _Col, FB) :-
    FB = [ "Try to do everthing correctly." ].

% 1) Swap vr and box groups.
buggy(tgroups, stage(1), From, To, [step(buggy, swap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(box, s_box, n_box, vr, s_vr, n_vr).

feedback(tgroups, swap, [], Col, FB) :-
    FB = [ "It appears you swapped the ", \mmlm(Col, color(swap, "VR")), " and ",
	   \mmlm(Col, color(swap, "BOX")), " variables." ].

hint(tgroups, swap, [], _Col, FB) :-
    FB = [ "Please keep in mind that you analyse the changes caused by the ",
	   "treatment at it's end and arrange the variables accordingly." ].

% 2) Forgot to use square of standard deviation in pooled variance.
buggy(tgroups, stage(2), From, To, [step(buggy, sqsd, [S_VR, S_BOX])]) :-
    From = var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX),
    To = var_pool(instead(bug(sqsd), S_VR, S_VR ^ 2), N_VR, 
	 instead(bug(sqsd), S_BOX, S_BOX ^ 2), N_BOX).

feedback(tgroups, sqsd, [S_VR, S_BOX], Col, FB) :-
    FB = [ "Please remember to use the squares of ", 
	   \mmlm(Col, color(sqsd, S_VR)), " and ", 
	   \mmlm(Col, color(sqsd, S_BOX)), " in the pooled variance." ].

hint(tgroups, sqsd, [_S_VR, _S_BOX], _Col, FB) :-
    FB = [ "Try using the variance rather than the standard deviation ",
	   "when calculating the pooled variance." ].

% 3) Forgot school math.
buggy(tgroups, stage(2), From, To, [step(buggy, school, [From, N_VR, N_BOX])]) :-
    dif(N_VR, N_BOX),
    From = 1 / N_VR + 1 / N_BOX,
    To = color(school, frac(1, N_VR + N_BOX)).

feedback(tgroups, school, [From, N_VR, N_BOX], Col, FB) :-
    FB = [ "Please do not forget school math, ", 
	   \mmlm(Col, color(school, From) 
	   =\= color(school, frac(1, N_VR + N_BOX))) ].

hint(tgroups, school, [From, N_VR, N_BOX], Col, FB) :-
    FB = [ "Do not forget school math, ", 
	   \mmlm(Col, color(school, From) =\= color(school, frac(1, N_VR + N_BOX))) ].

% 4) Same for N1 = N2
buggy(tgroups, stage(2), From, To, [step(buggy, school2, [From, N])]) :-
    From = 1 / N + 1 / N,
    To = color(school2, frac(1, 2 * N)).

feedback(tgroups, school2, [From, N], Col, FB) :-
    FB = [ "Please do not forget school math, ", 
	   \mmlm(Col, color(school2, From) =\= color(school2, frac(1, 2 * N))) ].

hint(tgroups, school2, [From, N], Col, FB) :-
    FB = [ "Do not forget school math, ", 
	   \mmlm(Col, color(school2, From) =\= color(school2, frac(1, 2 * N))) ].

% 5) Forgot paranthesis.
buggy(tgroups, stage(2), From, To, [step(buggy, bug1, [VR, BOX, S2P, N_VR, N_BOX])]) :-
    From = dfrac(VR - BOX, sqrt(S2P * (1/N_VR + 1/N_BOX))),
    To =  invent_left(bug(bug1), VR - dfrac(BOX, 
	  color(bug1, sqrt(S2P)))) * color(bug1, 1) / color(bug1, N_VR) + color(bug1, 1 / N_BOX).

feedback(tgroups, bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) :-
    FB = [ "Please do not forget the parentheses around the numerator and ",
	   "the denominator of a fraction, ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
	   sqrt(color(bug1, color("#000000", S2P)) * 
	   color(bug1, paren(color("#000000", 1/N_VR + 1/N_BOX)))))) 
	 ].

hint(tgroups, bug1, [VR, BOX, S2P, N_VR, N_BOX], Col, FB) :-
    FB = [ "Remember to use parenthesis. The correct formula for the ",
	   \mmlm(Col, hyph(t, "ratio")), "is ", 
	   \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", VR - BOX))), 
	   sqrt(color(bug1, color("#000000", S2P)) * 
	   color(bug1, paren(color("#000000", 1/N_VR + 1/N_BOX)))))) 
	 ].

% 7) Forgot square root.
buggy(tgroups, stage(2), From, To, [step(buggy, nosr1, [A])]) :-
    From = sqrt(S2P * (1/N_VR + 1/N_BOX)),
    A = S2P * (1/N_VR + 1/N_BOX),
    To = instead(bug(nosr1), A, sqrt(A)).

feedback(tgroups, nosr1, [A], Col, FB) :-
   FB = [ "Remeber to draw the square root of ", \mmlm(Col, color(nosr1, A))].

hint(tgroups, nosr1, [A], Col, FB) :-
    FB = [ "Keep in mind that you need to draw the square root of ", 
	   \mmlm(Col, color(nosr1, A))].

% 8) forgot square root and parenthesis.
buggy(tgroups, stage(2), From, To,  [step(buggy, nosr2, [S2P])]) :-
    From = dfrac(BOX, color(_, sqrt(S2P))),
    To = dfrac(BOX, color(nosr2, S2P)).

feedback(tgroups, nosr2, [_S2P], _Col, FB) :-
    FB = [ "Please remeber to draw the square root of the main functions denominator."].

hint(tgroups, nosr2, [S2P], Col, FB) :-
    FB = [ "Keep in mind that you need to draw the square root of ", 
	   \mmlm(Col, color(nosr2, S2P * (1 / "n_vr" + 1 / "n_box"))) ].

% 8) Swapped N_VR and N_Box
buggy(tgroups, stage(1), From, To, [step(buggy, nswap, [])]) :-
    From = item(vr, s_vr, n_vr, box, s_box, n_box),
    To = item(vr, s_vr, color(nswap, n_box), box, s_box, color(nswap, n_vr)).

feedback(tgroups, nswap, [], Col, FB) :-
    FB = [ "Please double check the sample sizes ", \mmlm(Col, color(nswap, n_vr)), 
	   " and ", \mmlm(Col, color(nswap, n_box)), " of both groups." ].

hint(tgroups, nswap, [], Col, FB) :-
    FB = [ "Do not swap the sample sizes in ", \mmlm(Col, color(nswap, s2p)) ].


% 6) forgot school math and parenthesis. 
% (doesn't work right now and is probably unnecessary)
%buggy(tgroups, stage(2), From, To, [step(buggy, scb1, [N_VR, N_BOX]), depends(bug1)]) :-
%    From = color(_, N_VR) + color(_, 1 / N_BOX),
%    To = color(scb1, N_VR) + omit_left(bug(scb1), 1 / N_BOX).

%feedback(tgroups, scb1, [N_VR, N_BOX], Col, FB) :-
%    FB = [ "Please do not forget school math ", \mmlm([error(correct) | Col],
%	   frac(1, color("#000000", N_VR)) + frac(1, color("#000000", N_BOX))
%	    =\= 1 / color("#000000", N_VR) + color("#000000", N_BOX)) 
%	 ].

%hint(tgroups, scb1, [N_VR, N_BOX], Col, FB) :-
%    FB = [ "Do not forget school math",  \mmlm([error(correct) | Col], 
%	   frac(1, color("#000000", N_VR)) + frac(1, color("#000000", N_BOX))
%	   =\= 1 / color("#000000", N_VR) + color("#000000", N_BOX)) 
%	 ].
