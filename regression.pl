:- module(regression, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(util).
:- use_module(r_session).
:- use_module(interval).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(regression, ["regression"]).

task(bcoef).
task(pvalue).
label(bcoef, "Estimate").
label(pvalue, [math(mi(p)), "-value"]).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4.

% Prettier symbols for mathematical rendering
math_hook(n, 'N').
math_hook(lm0(Y, X, "intercept"), ["Intercept: ", ~(r(Y), r(X))]).
math_hook(lm0(Y, X, "coef"), ["Estimate: ", ~(r(Y), r(X))]).
math_hook(lm0(Y, X, "pval:coef"), ["Estimate", p, "-value: ", ~(r(Y), r(X))]).
math_hook(lm0(Y, X, "pval:intercept"), ["Intercept", p, "-value: ", ~(r(Y), r(X))]).

% R definitions
r_hook(n).
r_hook(y).
r_hook(x).

r_hook(lm0/3).
mono(lm0/3, [/, /, /]).

% Task description
render(Flags)
--> { start(item(N, _Y, _X)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Sleep and Depression"),
		      p(class('card-text'),
                [ "Consider a clinical study researching the relationship between sleep duration ",
                "(hours per night) and depression severity measured on the Hamilton Rating Scale for Depression ", 
                "(HDRS, range from best = 0 to worst = 42) in a sample of patients (",
                \mmlm(Flags, N = r(N)),
                "). The dataset can be downloaded below."
                ]),
          \download(regression)
          ]))]).

% Question for b-coefficient
task(_Flags, bcoef)
--> { start(item(_N, _Y, _X)),
      session_data(resp(regression, bcoef, Resp), resp(regression, bcoef, '#.##'))
    },
	html(\htmlform(["How does the depression severity change (increase/decrease in HDRS score) for every hour of additional sleep?"], bcoef, Resp)).

% Question for p-value 
task(Flags, pvalue)
--> { start(item(_N, _Y, _X)),
      session_data(resp(regression, pvalue, Resp), resp(regression, pvalue, '#.##'))
    },
	html(\htmlform(["What is the ", span(class('text-\nowrap'), [\mmlm(Flags, p), "-value"]), " of the estimate?"], pvalue, Resp)).

%
% Expert rule for b-coefficient
%
start(item(n, y, x)).
intermediate(bcoef, item).

% First step: recognize the problem
intermediate(bcoef, problem).
expert(bcoef, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(_N, Y, X),
    To = { '<-'(b, linearmodel(Y, X)) }.

feedback(problem, [], _Col, F)
 => F = ["Correctly recognised the problem as a linear regression."].

hint(problem, [], _Col, F)
 => F = ["This is a linear regression."].

% Second step: extract coefficient from linear model
intermediate(bcoef, linearmodel).
expert(bcoef, stage(1), From, To, [step(expert, linearmodel, [Y, X])]) :-
    From = linearmodel(Y, X),
    To = lm0(Y, X, "coef").

feedback(linearmodel, [_,_], _Col, F)
 => F = ["Correctly extracted the ", span(class('text-\nowrap'), [\mmlm(b), "-coefficient for the sleep duration."])].

hint(linearmodel, [_,_], _Col, F)
 => F = ["Report the coefficient for the sleep duration."].


%
% Buggy-Rules for b-coefficient task
%

% Buggy-Rule: switched outcome and predictor
buggy(bcoef, stage(1), From, To, [step(buggy, switch, [Y, X])]) :-
    From = linearmodel(Y, X),
    To = instead(switch, lm0(X, Y, "coef"), lm0(Y, X, "coef")).

feedback(switch, [_, _], _, F)
 => F = ["The predictor and outcome variable of the model were switched."].

hint(switch, [_,_], _, F)
 => F = ["Make sure to define the correct outcome variable and predictor in the model."].

% Buggy-Rule: reported intercept instead of predictor 
buggy(bcoef, stage(1), From, To, [step(buggy, intercept, [Y, X])]) :-
    From = lm0(X, Y, "coef"),
    To = lm0(Y, X, "intercept").

feedback(intercept, [_, _], _, F)
 => F = ["The intercept was reported."].

hint(intercept, [_,_], _, F)
 => F = ["Make sure to report the coefficient of the predictor and not of the intercept."].


%
% Expert rule for p-value
%
intermediate(pvalue, item).

% First step: recognize the problem
intermediate(pvalue, problem).
expert(pvalue, stage(2), From, To, [step(expert, problem, [])]) :-
    From = item(_N, Y, X),
    To = { '<-'(p, linearmodel(Y, X)) }.

feedback(problem, [], _Col, F)
 => F = ["Correctly recognised the problem as a linear regression."].

hint(problem, [], _Col, F)
 => F = ["This is a linear regression."].

% Second step: extract p-value of coefficient from linear model
intermediate(pvalue, linearmodel).
expert(pvalue, stage(2), From, To, [step(expert, getpvalue, [Y, X])]) :-
    From = linearmodel(Y, X),
    To = lm0(Y, X, "pval:coef").

feedback(getpvalue, [_Y, _X], _Col, F)
 => F = ["Correctly extracted the ", span(class('text-\nowrap'), [\mmlm(p), "-value"]), " for the estimate of the sleep duration."].

hint(getpvalue, [_Y, _X], _Col, F)
 => F = ["Report the ", span(class('text-\nowrap'), [\mmlm(p), "-value"]), " for the estimate of the sleep duration."].

%
% Buggy-Rules for p-value task
%

% Buggy-Rule: switched outcome and predictor
buggy(pvalue, stage(2), From, To, [step(buggy, switch1, [Y, X])]) :-
    From = linearmodel(Y, X),
    To = instead(switch1, lm0(X, Y, "pval:coef"), lm0(Y, X, "pval:coef")).

feedback(switch1, [_, _], _, F)
 => F = ["The predictor and outcome variable of the model were switched."].

hint(switch1, [_,_], _, F)
 => F = ["Make sure to define the correct outcome variable and predictor in the model."].

% Buggy-Rule: reported intercept instead of predictor 
buggy(pvalue, stage(2), From, To, [step(buggy, intercept1, [Y, X])]) :-
    From = lm0(Y, X, "pval:coef"),
    To = lm0(Y, X, "pval:intercept").

feedback(intercept1, [_, _], _, F)
 => F = ["The ", span(class('text-\nowrap'), [\mmlm(p), "-value"]), " of the intercept was reported."].

hint(intercept1, [_,_], _, F)
 => F = ["Make sure to report the ", span(class('text-\nowrap'), [\mmlm(p), "-value"]), " for predictor and not for the intercept."].
