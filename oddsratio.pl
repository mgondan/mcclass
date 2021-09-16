%:- module(oddsratio, 
%       	[ start/2, init/1, data/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, 
%	render//3]).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r).
:- use_module(mathml).

:- multifile init/1, data/2, start/2, intermediate/2, expert/5, buggy/5, feedback/5, hint/5, render//3.

init(oddsratio) :-
 	data(oddsratio).

data(oddsratio, File) :-
	session_data(download(oddsratio, File)).

data(oddsratio) :- 
	r_init,

	{|r||
        sr_a <- round(runif(1, min=10, max=95))
        Id <- 1:sr_a
        OR <- round(runif(sr_a, min=0.02, max=10))
        data <- data.frame(Id, OR)
        or <- mean(data$OR)
    |},

	r_data_frame_colnames(data, Names),
	r_data_frame_to_rows(data, row, Rows),
	Header =.. [row | Names],
	tmp_file_stream(File, Stream, []),
	csv_write_stream(Stream, [Header | Rows], [separator(0';), encoding(utf8)]),
	close(Stream),
	session_assert(download(oddsratio, File)).

%
% Prettier symbols for mathematical rendering
%
mathml:hook(Flags, sr_a, Flags, sub(sr, "A")).
mathml:hook(Flags, or, Flags, 'OR').

% Render R result
mathml:hook(Flags, r(Expr), Flags, Res) :-
	R <- Expr,
	[Res] = R,
	number(Res).

render(oddsratio, item(SR_A, OR), Form) -->
	{ option(resp(R), Form, '##') },
	html(
		[ div(class(card), div(class('card-body'),
		    [ h1(class('card-title'), "Clinical Study"),
		      p(class('card-text'),
		       [ "Compare the effectiveness of ",
		         "two therapies. Therapy A has a probability of success of ",
		          \mmlm(SR_A = [r(sr_a), "%"]), ". The Odds Ratio is ",
		          \mmlm(OR = r(or)), " in favor of Therapy A." 
		       ])
		    ])),
	          div(class(card), div(class('card-body'),
		     [ h4(class('card-title'), [a(id(question), []), "Question"]),
		       p(class('card-text'),
		        [ "What is the probability of sucess of treatment B?"
		        ]),
		       form([class(form), method('POST'), action('#oddsratio-reverse')],
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

% Odds ratio with two probabilities.
% sr_a = sucess rate of a; or = odds ratio; 
intermediate(_, item).
start(oddsratio, item(sr_a, or)) :-
	init(oddsratio).

intermediate(oddsratio, odd).
expert(oddsratio, stage(2), X, Y, [step(expert, odd, [])]) :-
	X = item(SR_A, OR),
	Y = odd(SR_A, OR).

feedback(oddsratio, odd, [], Col, FB) :-
	FB = [ "Correctly recognised the problem as a ", \mmlm(Col, hyph(odds, "ratio")), "."].

hint(oddsratio, odd, [], Col, Hint) :-
        Hint = [ "This is a ", \mmlm(Col, hyph(odds, "ratio")), "."].

% Turn percentage into decimal.
intermediate(oddsratio, odds1).
expert(oddsratio, stage(2), X, Y, [step(expert, odds1, [SR_A])]) :-
	X = odd(SR_A, OR),
	Y = odds1((SR_A / (100 - SR_A)), OR).

feedback(oddsratio, odds1, [SR_A], Col, FB) :-
	FB = [ "Correctly converted ", \mmlm(Col, SR_A), " to odds." ].

hint(oddsratio, odds1, [SR_A], Col, Hint) :-
	Hint = [ "The formula to convert ", \mmlm(Col, SR_A), " to odds is ", 
		 \mmlm(Col, SR_A / (100 - SR_A)) ].

% Calculate decimal for sucess rate of b.
intermediate(oddsratio, odds2).
expert(oddsratio, stage(2), X, Y, [step(expert, odds2, [O_A, OR])]) :-
	X = odds1(O_A, OR),
	Y = odds2(O_A / OR).

feedback(oddsratio, odds2, [_O_A, _OR], Col, FB) :-
	FB = [ "Correctly calculated the odds for sucess with treatment ", 
	       \mmlm(Col, b)].

hint(oddsratio, odds2, [O_A, OR], Col, Hint) :-
	Hint = [ "The odds for success with treatment ",
		 \mmlm(Col, b), " are ", \mmlm(Col, (O_A / OR)) ].

% Turn decimal into Sucess rate of b.
expert(oddsratio, stage(2), X, Y, [step(expert, reverse, [O_B])]) :-
	X = odds2(O_B),
	Y = dfrac(O_B, (1 + O_B)) * 100.

feedback(oddsratio, reverse, [_O_B], Col, FB) :-
	FB = [ "Correctly calculated the probability for success with treatment ", 
	       \mmlm(Col, b), " and converted it to percent." ].

hint(oddsratio, reverse, [O_B], Col, Hint) :-
	Hint = [ " The success-rate for treatment ", \mmlm(Col, b), " is ",
		 \mmlm(Col, (dfrac(O_B, (1 + O_B)) * 100)) ].

% Forgot decimal conversion of sr_a.
buggy(oddsratio, stage(2), X, Y, [step(buggy, cona, [SR_A])]) :-
	X = odd(SR_A, OR),
	Y = odds1(omit_right(bug(cona), SR_A / (100 - SR_A)), OR).

feedback(oddsratio, cona, [SR_A], Col, FB) :-
	FB = [ "Please remember to convert ", \mmlm(Col, color(cona, SR_A)), " to ",
	       "odds." ].

hint(oddsratio, cona, [SR_A], Col, Hint) :-
	Hint = [ "You should concider converting ", \mmlm(Col, color(cona, SR_A)), 
		 " to odds before continuing." ].

% Forgot to divide o_a and or.
buggy(oddsratio, stage(2), X, Y, [step(buggy, divi, [O_A, OR])]) :-
	X = odds1(O_A, OR),
	Y = odds2(omit_right(bug(divi), O_A / OR)).

feedback(oddsratio, divi, [O_A, OR], Col, FB) :-
	FB = [ "It appears you forgot to divide ", \mmlm(Col, color(divi, O_A)), " by ",
	       \mmlm(Col, color(divi, OR)) ].

hint(oddsratio, divi, [O_A, OR], Col, Hint) :-
	Hint = [ "Do not forget to divide ",\mmlm(Col, color(divi, O_A)), " by ",
		 \mmlm(Col, color(divi, OR)) ].

% Multiplied O_A and OR rather than deviding them.
buggy(oddsratio, stage(2), X, Y, [step(buggy, mult, [O_A, OR])]) :-
	X = odds1(O_A, OR),
	Y = odds2(instead(bug(mult), (O_A * OR), (O_A / OR))).

feedback(oddsratio, mult, [O_A, OR], Col, FB) :-
	FB = [ "It seems you multiplied ", \mmlm(Col, color(mult, O_A)), " with ", 
	\mmlm(Col, color(mult, OR)), " rather than dividing them." ].

hint(oddsratio, mult, [O_A, OR], Col, Hint) :-
	Hint = [ "If I were you I would divide ", \mmlm(Col, color(divi, O_A)), " by ",
		 \mmlm(Col, color(divi, OR)), " rather then multiplying them." ].

% Forgot percentage conversion of o_b.
buggy(oddsratio, stage(2), X, Y, [step(buggy, conb, [SR_B])]) :-
	X = odds2(O_B),
	SR_B = dfrac(O_B, (1 + O_B)),
	Y = omit_right(bug(conb), SR_B * 100).

feedback(oddsratio, conb, [SR_B], Col, FB) :-
	FB = [ "Please keep in mind that you need to convert ", 
	       \mmlm(Col, color(conb, SR_B)), " to percent." ].

hint(oddsratio, conb, [SR_B], Col, Hint) :-
	Hint = [ "You almost got it right. Just don't forget to convert ", 
		 \mmlm(Col, color(conb, SR_B)), " to percent." ].

% Forgot to convert Odds for B to Probability.
buggy(oddsratio, stage(2), X, Y, [stage(buggy, srb, [O_B])]) :-
	X = odds2(O_B),
	Y = instead(bug(srb), O_B, (dfrac(O_B, (1 + O_B)) * 100)).

feedback(oddsratio, sr_b, [O_B], Col, FB) :-
	FB = [ "It looks like you forgot to convert ", \mmlm(Col, color(srb, O_B)), 
	       " back into a probability." ].

hint(oddsratio, srb, [O_B], Col, Hint) :-
	Hint = [ "Remember that your end result should be a probabitly in ",
       		 "percent rather than the odds ", \mmlm(Col, color(srb, O_B)) ].
