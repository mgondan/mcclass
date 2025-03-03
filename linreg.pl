:- module(linreg, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(linreg, ["linear regression"]).

task(bcoef).
task(correlation).
task(pvalue).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

% Prettier symbols for mathematical rendering

math_hook(n, 'N').
math_hook(mean_Sleep, 'Mean_Sleep').
math_hook(sd_Sleep, 'SD_Sleep').
math_hook(b_coef, 'B_Coef').

% R definitions

r_hook(n).
r_hook(mean_Sleep).
r_hook(sd_Sleep).
r_hook(mean_Dep).
r_hook(sd_Dep).
r_hook(b_coef).
r_hook(intercept).
r_hook(cor_value).
r_hook(p_value).

% Task description

render
--> { start(item(N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _B_Coef, _Intercept, _Cor_Value, _P_Value)) },
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
          \download(linreg)
          ]))]).

% Question for b-coefficient

task(bcoef)
--> { start(item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, _B_Coef, _Intercept, _Cor_Value, _P_Value)),
      session_data(resp(linreg, bcoef, Resp), resp(linreg, bcoef, '#.##'))
    },
	html(\htmlform(["Calculate the b-coefficient for the sleep duration."], bcoef, Resp)).

% Expert rule for b-coefficient

intermediate(bcoef, item).
start(item(n, mean_Sleep, sd_Sleep, mean_Dep, sd_Dep, b_coef, intercept, cor_value, p_value)).

% First step

intermediate(bcoef, correct).
expert(bcoef, stage(2), X, Y, [step(expert, correct, [])]) :-
    X = item(_N, _Mean_Sleep, _SD_Sleep, _Mean_Dep, _SD_Dep, B_Coef, _Intercept, _Cor_Value, _P_Value),
    Y = { '<-'(b, correct(B_Coef)) }.

feedback(correct, [], _Col, F)
 => F = ["Test1"].

hint(correct, [], _Col, F)
 => F = ["Test1"].

% Second step

expert(bcoef, stage(2), X, Y, [step(expert, bcoef, [B_Coef])]) :-
    X = correct(B_Coef),
    Y = B_Coef.

feedback(bcoef, [_], _Col, F)
 => F = ["Test2"].

hint(bcoef, [_], _Col, F)
 => F = ["Test2"].


% Buggy rules for b-coefficient:

% read intercept instead of sleep


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