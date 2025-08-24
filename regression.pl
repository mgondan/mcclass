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

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/3.

% Prettier symbols for mathematical rendering
math_hook(n, 'N').
math_hook(lm0(Y, X), fn(lm, [~(Y, X)])).
math_hook(extract0(M, Coef), fn(coef,[M, Coef])).
math_hook(extract1(M, Coef), fn(pval,[M, Coef])).

% R definitions
macro(n).
macro(y).
macro(x).
macro(m).
macro(extract0/2, all, [/, /], [pattern([string, string])]).
macro(extract1/2, all, [/, /], [pattern([string, string])]).
macro(lm0/2, all, [/, /], [pattern([string, string])]).

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

% Question for b-coefficient task
task(_Flags, bcoef)
--> { start(item(_N, _Y, _X)),
      session_data(resp(regression, bcoef, Resp), resp(regression, bcoef, '#.##'))
    },
    html(\htmlform(
      [ "How does the depression severity ",
        "change (increase/decrease in HDRS score) for every hour of ",
        "additional sleep?"
      ], bcoef, Resp)).

% Question for p-value task
task(Flags, pvalue)
--> { start(item(_N, _Y, _X)),
      session_data(resp(regression, pvalue, Resp), resp(regression, pvalue, '#.##'))
    },
    html(\htmlform(["What is the ", \nowrap([\mmlm(Flags, p), "-value"]), " of the estimate?"], pvalue, Resp)).

start(item(n, y, x)).
%
% Expert rule for bcoef task
%
% First step: recognize the problem
intermediate(bcoef, item).
expert(bcoef, stage(1), From, To, [step(expert, problem, [])]) :-
    From = item(_N, Y, X),
    To = { '<-'(m, linearmodel(Y, X));
           '<-'(b, extract_(m, "predictor"))
         }.

feedback(problem, [], _Col, F)
 => F = ["Correctly recognised the problem as involving a linear regression."].

hint(problem, _Col, H)
 => H = "This is a linear regression.".

% Second step: define outcome and predictor in the regression equation
intermediate(bcoef, linearmodel).
expert(bcoef, stage(2), From, To, [step(expert, equation, [])]) :-
    From = linearmodel(Y, X),
    To = lm0(Y, X).

feedback(equation, [], _Col, F)
 => F = [ "Correctly defined the outcome and the predictor in the regression equation." ].

hint(equation, _Col, H)
 => H = [ "Define the regression equation." ].

% Third step: extract coefficient from resulting linear model
intermediate(bcoef, extract_).
expert(bcoef, stage(3), From, To, [step(expert, extract, [])]) :-
    From = extract_(M, Coef),
    To = extract0(M, Coef).

feedback(extract, [], _Col, F)
 => F = [ "Correctly extracted the ", \nowrap([\mmlm(b), "-coefficient"]),
          " for the predictor."
        ].

hint(extract, _Col, H)
 => H = "Report the coefficient for the predictor.".

%
% Buggy-Rules for b-coefficient task
%
% Buggy-Rule: switched outcome and predictor
buggy(bcoef, stage(2), From, To, [step(buggy, switch, [])]) :-
    From = linearmodel(Y, X),
    To = instead(switch, lm0(X, Y), lm0(Y, X)).

feedback(switch, [], _, F)
 => F = ["The predictor and outcome variable of the model were switched."].

hint(switch, _, F)
 => F = "Define the correct outcome and predictor in the model.".


% Buggy-Rule: reported intercept instead of predictor 
buggy(bcoef, stage(3), From, To, [step(buggy, intercept, [])]) :-
    From = extract_(M, "predictor"),
    To = extract0(M, instead(intercept, "intercept", "predictor")).

feedback(intercept, [], _, F)
 => F = ["The intercept was mistakenly reported."].

hint(intercept, _, H)
 => H = "Report the coefficient of the predictor and not of the intercept.". 

%
% Expert rule for pvalue task
%
% First step: recognize the problem
intermediate(pvalue, item).
expert(pvalue, stage(1), From, To, [step(expert, problem1, [])]) :-
    From = item(_N, Y, X),
    To = { '<-'(m, linearmodel(Y, X));
           '<-'(p, extract_(m, "predictor"))
         }.

feedback(problem1, [], _Col, F)
 => F = ["Correctly recognised the problem as involving a linear regression."].

hint(problem1, _Col, H)
 => H = "This is a linear regression.".

% Second step: define outcome and predictor in the regression equation
intermediate(pvalue, linearmodel).
expert(pvalue, stage(2), From, To, [step(expert, equation1, [])]) :-
    From = linearmodel(Y, X),
    To = lm0(Y, X).

feedback(equation1, [], _Col, F)
 => F = [ "Correctly defined the outcome and the predictor in the regression equation." ].

hint(equation1, _Col, H)
 => H = [ "Define the regression equation." ].

% Third step: extract p-value of predictor from linear model
intermediate(pvalue, extract_).
expert(pvalue, stage(3), From, To, [step(expert, extract1, [])]) :-
    From = extract_(M, Coef),
    To = extract1(M, Coef).

feedback(extract1, [], _Col, F)
 => F = [ "Correctly extracted the ", \nowrap([\mmlm(p), "-value"]), 
          " for the the predictor."
        ].

hint(extract1, Col, H)
 => H = ["Report the ", \nowrap([\mmlm(Col, p), "-value"]), " for the predictor."].

%
% Buggy-Rules for pvalue task
%
% Buggy-Rule: switched outcome and predictor
buggy(pvalue, stage(2), From, To, [step(buggy, switch1, [])]) :-
    From = linearmodel(Y, X),
    To = instead(switch, lm0(X, Y), lm0(Y, X)).

feedback(switch1, [], _, F)
 => F = ["The predictor and outcome variable of the model were switched."].

hint(switch1, _, F)
 => F = "Define the correct outcome and predictor in the model.".


% Buggy-Rule: reported p-value of intercept instead of predictor 
buggy(pvalue, stage(3), From, To, [step(buggy, intercept1, [])]) :-
    From = extract_(M, "predictor"),
    To = extract1(M, instead(intercept, "intercept", "predictor")).

feedback(intercept1, [], _, F)
 => F = ["The ", \nowrap([\mmlm(p), "-value"]), " of the intercept was mistakenly reported."].

hint(intercept1, Col, F)
 => F = ["Report the ", \nowrap([\mmlm(Col, p), "-value"]), " for the predictor and not for the intercept."].
