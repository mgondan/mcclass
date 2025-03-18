:- module(regression, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(interval/interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(regression, ["regression"]).

task(bcoef).
task(correlation).
task(pvalue).

label(bcoef, "bcoef").
label(correlation, "correlation").
label(pvalue, "pvalue").

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering

math_hook(n, 'N').

% R definitions

r_hook(n).
r_hook(mean_Sleep).
r_hook(sd_Sleep).
r_hook(mean_Dep).
r_hook(sd_Dep).
r_hook(dep).
r_hook(sleep).

r_hook(bcoef/2).

% Task description

render
--> { start(item(N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _Dep, _Sleep)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Linear regression"),
		      p(class('card-text'),
                [ "Consider a clinical study researching the relationship between sleep duration ",
                "(X, in hours per night) and depression severity ", 
                "(Y, measured on a standardized scale) in a sample of patients ",
                \mmlm(N = r(N)),
                " diagnosed with major depressive disorder."
                ]),
          \download(regression)
          ]))]).

% Question for b-coefficient

task(bcoef)
--> { start(item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _Dep, _Sleep)),
      session_data(resp(regression, bcoef, Resp), resp(regression, bcoef, '#.##'))
    },
	html(\htmlform(["Calculate the b-coefficient for the sleep duration."], bcoef, Resp)).

% Expert rule for b-coefficient

intermediate(bcoef, item).
start(item(n, mean_Sleep, sd_Sleep, mean_Dep, sd_Dep, dep, sleep)).

% First step

intermediate(bcoef, correct).
expert(bcoef, stage(2), X, Y, [step(expert, correct, [])]) :-
    X = item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, Dep, Sleep),
    Y = { '<-'(b, regression0(Dep, Sleep)) }.

feedback(correct, [], _Col, F)
 => F = ["Test1"].

hint(correct, [], _Col, F)
 => F = ["Test1"].

% Second step

expert(bcoef, stage(2), X, Y, [step(expert, bcoef, [Dep, Sleep])]) :-
    X = correct(Dep, Sleep),
    Y = bcoef(Dep, Sleep).

feedback(bcoef, [_,_], _Col, F)
 => F = ["Test2"].

hint(bcoef, [_,_], _Col, F)
 => F = ["Test2"].

% Buggy rules for b-coefficient
/*
% Switched dependent and independent variable

buggy(bcoef, stage(1), X, Y, [step(buggy, switch, [Dep, Sleep])]) :-
    X = regression0(Dep, Sleep),
    Y = regression1(instead(switch, (Sleep, Dep), (Dep, Sleep))).

feedback(switch, [_, _], _, F)
 => F = ["Test"].

hint(switch, [_,_], _, F)
 => F = ["Test"].
*/

/*
% Expert rule for correlation

intermediate(correlation, item).

% First step

intermediate(correlation, correct).
expert(correlation, stage(2), X, Y, [step(expert, correct, [])]) :-
    X = item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _B_Coef, _Intercept, Cor_Value, _P_Value),
    Y = { '<-'(c, correct(Cor_Value)) }.

feedback(correct, [], _Col, F)
 => F = ["Test1"].

hint(correct, [], _Col, F)
 => F = ["Test1"].

% Second step

expert(correlation, stage(2), X, Y, [step(expert, correlation, [Cor_Value])]) :-
    X = correct(Cor_Value),
    Y = Cor_Value.

feedback(correlation, [_], _Col, F)
 => F = ["Test2"].

hint(correlation, [_], _Col, F)
 => F = ["Test2"].


% Expert rule for p-value

intermediate(pvalue, item).

% First step

intermediate(pvalue, correct).
expert(pvalue, stage(2), X, Y, [step(expert, correct, [])]) :-
    X = item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _B_Coef, _Intercept, _Cor_Value, P_Value),
    Y = { '<-'(p, correct(P_Value)) }.

feedback(correct, [], _Col, F)
 => F = ["Test1"].

hint(correct, [], _Col, F)
 => F = ["Test1"].

% Second step

expert(pvalue, stage(2), X, Y, [step(expert, pvalue, [P_Value])]) :-
    X = correct(P_Value),
    Y = P_Value.

feedback(pvalue, [_], _Col, F)
 => F = ["Test2"].

hint(pvalue, [_], _Col, F)
 => F = ["Test2"].
 */