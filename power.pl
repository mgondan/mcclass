:- module(power, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(power, "Power").
task(power).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering
mathml:math_hook(n_vr, subscript(n, "VR")).
mathml:math_hook(n_box, subscript(n, "Box")).
mathml:math_hook(vr, overline("VR")).
mathml:math_hook(s_vr, subscript(s, "VR")).
mathml:math_hook(box, overline("Box")).
mathml:math_hook(s_box, subscript(s, "Box")).
mathml:math_hook(s2p, subscript(s, "pool")^2).

% Obtain information from R
r_hook(n_vr).
r_hook(n_box).
r_hook(vr).
r_hook(s_vr).
r_hook(box).
r_hook(s_box).
r_hook(s2p).
r_hook(t).

r_hook(var_pool/4).
mono((var_pool)/4, [+, /, +, /]).


% Task description
render
--> {start(item(_VR, _S_VR, N_VR, _BOX, _S_BOX, N_BOX)) },
	html(
	   div(class(card), div(class('card-body'),
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
		    button([ class('btn btn-secondary'), name(download), value(power) ], "Download data"))
	      ]))).

task(power, Form)
--> { start(item(_VR, _S_VR, _N_VR, _BOX, _S_BOX, _N_BOX)),
      (   option(task(power), Form)
      ->  option(resp(Resp), Form, '#.##'),
          session_retractall(resp(power, power, _)),
          session_assert(resp(power, power, Resp))
      ;   session_data(resp(power, power, Resp), resp(power, power, '#.##'))
      )
    },
    html(\htmlform([ "Is VR training superior to traditional Box training?"], power, Resp)).
	    
	

% t-test for independent groups
intermediate(power, item).
start(item(vr, s_vr, n_vr, box, s_box, n_box)).

% First Step: Correctly identified the problem as a t-test for independent samples.
intermediate(power, indep).
expert(power, stage(2), From, To, [step(expert, indep, [])]) :-
    From = item(VR, S_VR, N_VR, BOX, S_BOX, N_BOX),
    To = { '<-'(s2p, var_pool(S_VR ^ 2, N_VR, S_BOX ^ 2, N_BOX)) ;
	   '<-'(t, dfrac(VR - BOX, sqrt(s2p * (1/N_VR + 1/N_BOX)))) ;
	   t
	 }.

feedback(indep, [], Col, FB) =>
    FB = [ "Correctly identified the problem as a ", \mmlm(Col, hyph(t, "test")),
	   " for independent samples and solved it correctly." ].

hint(indep, [], Col, FB) =>
    FB = [ "This is a ", \mmlm(Col, hyph(t, "test")), " for independent ",
           "samples." ].

