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

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
    r_session(Expr, Res),
    number(Res).

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
		    "groups showed equal operative performance in the OSATS score VR:  ",
		    \mmlm(r(vr)), " ± ", \mmlm(r(s_vr)), " vs. BOX:  ", 
		    \mmlm(r(box)), " ± ", \mmlm(r(s_box)),
		    ". The significance level is set to ",
		    \mmlm(alpha = [5, "%"]), " two-tailed.",
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
intermediate(tgroups, indep).
expert(tgroups, stage(1), X, Y, [step(expert, indep, [])]) :-
	X = item(T0, S_T0, N_T0, EOT, S_EOT, N_EOT),
	Y = indep(T0, S_T0, N_T0, EOT, S_EOT, N_EOT).

feedback(tgroups, indep, [], Col, FB) :-
	FB = [ "Correctly identified the ", \mmlm(Col, hyph(t, "test")), 
	       " for independent groups." ].

hint(tgroups, indep, [], Col, FB) :-
	FB = [ "This is a ",\mmlm(Col, hyph(t, "test")), 
	       " for independent samples." ].

% Correctly standardized the score.
expert(tgroups, stage(1), X, Y, [step(expert, pnorm, [T0, EOT, P, N_T0, N_EOT])]) :-
	X = indep(T0, S_T0, N_T0, EOT, S_EOT, N_EOT),
	P = abbrev(s2p, var_pool(S_T0^2, N_T0, S_EOT^2, N_EOT), "'the pooled variance'"),
	Y = dfrac(T0 - EOT, sqrt(P * (1/N_T0 + 1/N_EOT))).

feedback(tgroups, pnorm, [_T0, _EOT, _P, _N_T0, _N_EOT], Col, FB) :-
	FB = [ "Correctly identified the expression for the ", 
	       \mmlm(Col, hyph(t, "ratio")) ].

hint(tgroups, pnorm, [T0, EOT, P, N_T0, N_EOT], Col, FB) :-
	FB = ["The ", \mmlm(Col, hyph(t, "ratio")), " is ",
	       \mmlm(Col, dfrac(T0 - EOT, sqrt(P * (1/N_T0 + 1/N_EOT))))].

%Swap t0 and eot.
buggy(tgroups, stage(1), X, Y, [step(buggy, swap, [])]) :-
	X = item(T0, S_T0, N_T0, EOT, S_EOT, N_EOT),
	Y = indep(EOT, S_EOT, N_EOT, T0, S_T0, N_T0).

feedback(tgroups, swap, [], Col, FB) :-
	FB = [ "It appears you swapped the ", \mmlm(Col, color(swap, "T0")), " and ",
	\mmlm(Col, color(swap, "EOT")), " variables." ].

hint(tgroups, swap, [], _Col, FB) :-
	FB = [ "Please keep in mind that you analyse the changes caused by the treatment at it's end",
	       " and arrange the variables accordingly." ].

% Forgot to use square of standard deviation in pooled variance.
buggy(tgroups, stage(1), X, Y, [step(buggy, sqsd, [S_T0, S_EOT])]) :-
	X = var_pool(S_T0^2, N_T0, S_EOT^2, N_EOT),
	Y = var_pool(instead(bug(sqsd), S_T0, S_T0^2), N_T0, instead(bug(sqsd), S_EOT, S_EOT^2), N_EOT).

feedback(tgroups, sqsd, [S_T0, S_EOT], Col, FB) :-
	FB = [ "Please remember to use the squares of  ", \mmlm(Col, color(sqsd, S_T0)), " and ", 
	       \mmlm(Col, color(sqsd, S_EOT)), " in the pooled variance." ].

hint(tgroups, sqsd, [_S_T0, _S_EOT], _Col, FB) :-
	FB = [ "Try using the variance rather than the standard deviation when calculating the pooled variance." ].

% Forgot school math.
buggy(tgroups, stage(2), X, Y, [step(buggy, school, [N1, N2])]) :-
	dif(N1, N2),
	X = 1/N1 + 1/N2,
	Y = frac(1, color(school, N1 + N2)).

feedback(tgroups, school, [N1, N2], Col, FB) :-
	FB = [ "Please do not forget school ",
	       "math, ", \mmlm(Col, frac(1, color(school, N1)) + 
	       frac(1, color(school, N2)) =\= frac(1, color(school, N1+N2))) ].

hint(tgroups, school, [N1, N2], Col, FB) :-
	FB = [ "Please do not forget school ",
	       "math, ", \mmlm(Col, frac(1, color(school, N1)) +
	       frac(1, color(school, N2)) =\= frac(1, color(school, N1+N2))) ].

% Same for N1 = N2
buggy(tgroups, stage(2), X, Y, [step(buggy, school2, [N])]) :-
	X = 1/N + 1/N,
	Y = frac(1, color(school2, 2*N)).

feedback(tgroups, school2, [N], Col, FB) :-
	FB = [ "Please do not forget school math, ", 
	       \mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N)) 
	       =\= frac(1, color(school2, 2*N))) ].

hint(tgroups, school2, [N], Col, FB) :-
	FB = [ "Please do not forget school math, ", 
	       \mmlm(Col, frac(1, color(school2, N)) + frac(1, color(school2, N)) 
	       =\= frac(1, color(school2, 2*N))) ].

% Forgot paranthesis.
buggy(tgroups, stage(1), X, Y, [step(buggy, bug1, [T0, EOT, P, N_T0, N_EOT])]) :-
	X = indep(T0, S_T0, N_T0, EOT, S_EOT, N_EOT),
	P = abbrev(s2p, var_pool(S_T0^2, N_T0, S_EOT^2, N_EOT), "'the pooled variance'"),
	Y = color(bug1, T0) - dfrac(omit_left(bug(bug1), T0 - EOT), 
	    sqrt(P *  1 / color(bug1, N_T0) + color(bug1, 1/N_EOT))).

feedback(tgroups, bug1, [T0, EOT, P, N_T0, N_EOT], Col, FB) :-
	FB = [ "Please do not forget the parentheses around the numerator and ",
	       "the denominator of a fraction, ", 
	       \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", T0 - EOT))), 
	       sqrt(color(bug1, color("#000000", P)) * color(bug1, paren(color("#000000", 1/N_T0 + 1/N_EOT)))))) 
	     ].

hint(tgroups, bug1, [T0, EOT, P, N_T0, N_EOT], Col, FB) :-
	FB = [ "Remember to use parenthesis. The correct formula for the",
	       \mmlm(Col, hyph(t, "ratio")), "is ", 
	       \mmlm([error(correct) | Col], dfrac(color(bug1, paren(color("#000000", T0 - EOT))), 
	       sqrt(color(bug1, color("#000000", P)) * color(bug1, paren(color("#000000", 1/N_T0 + 1/N_EOT)))))) 
	     ].

% forgot school math and parenthesis.
buggy(tgroups, stage(2), X, Y, [step(buggy, scb1, [P, N_T0, N_EOT])]) :-
	X = P * 1/ color(_, N_T0) + color(_, 1/N_EOT),
	Y = P * color(scb1, 1) / color(scb1, N_T0) + color(scb1, N_EOT).

feedback(tgroups, scb1, [P, N_T0, N_EOT], Col, FB) :-
	FB = [ "Please do not forget school math ", \mmlm([error(correct) | Col],
	       P * 1 / color("#000000", N_T0) + color("#000000", N_EOT) 
	       =\= P * (frac(1, color("#000000", N_T0)) + frac(1, color("#000000", N_EOT)))) 
	     ].

hint(tgroups, scb1, [P, N_T0, N_EOT], Col, FB) :-
	FB = [ "Please do not forget school math",  \mmlm([error(correct) | Col], 
	       P * 1 / color("#000000", N_T0) + color("#000000", N_EOT)
	       =\= frac(1, color("#000000", N_T0)) + frac(1, color("#000000", N_EOT))) 
	     ].

% Forgot square root.
buggy(tgroups, stage(2), X, Y, [step(buggy, nosr1, [A])]) :-
	X = sqrt(P * (1/N_T0 + 1/N_EOT)),
	A = P * (1/N_T0 + 1/N_EOT),
	Y = instead(bug(nosr1), A, sqrt(A)).

feedback(tgroups, nosr1, [A], Col, FB) :-
	FB = [ "Remeber to draw the square root of ", \mmlm(Col, color(nosr1, A))].

hint(tgroups, nosr1, [A], Col, FB) :-
	FB = [ "Keep in mind that you need to draw the square root of ", \mmlm(Col, color(nosr1, A))].

% forgot square root and parenthesis.
buggy(tgroups, stage(2), X, Y,  [step(buggy, nosr2, [A])]) :-
	X = sqrt(P *  1 / color(_, N_T0) + color(_, 1/N_EOT)),
	A =  P *  1 / color(bug1, N_T0) + color(bug1, 1/N_EOT),
	Y = instead(bug(nosr2), A, sqrt(P * (1/N_T0 + 1/N_EOT))).

feedback(tgroups, nosr2, [A], Col, FB) :-
	FB = [ "Remeber to draw the square root of", \mmlm(Col, color(nosr2, A))].

hint(tgroups, nosr2, [A], Col, FB) :-
	FB = [ "Keep in mind that you need to draw the square root of ", \mmlm(Col, color(nosr2, A))].
