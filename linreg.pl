:- module(linreg, []).

:- use_module(library(http/html_write)).
:- use_module(session).
:- use_module(table).
:- use_module(r_session).
:- use_module(library(mcclass)).
:- use_module(mathml).

:- use_module(navbar).
navbar:page(linreg, ["linear regression"]).

task(b_coef).
task(correlation).
task(p-value).

:- discontiguous intermediate/2, expert/5, buggy/5, feedback/4, hint/4, r_hook/1.

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
--> { start(item(n)) },
	html(
      [ div(class(card),
          div(class('card-body'),
            [ h1(class('card-title'), "Linear regression"),
		      p(class('card-text'),
                [ "Test."
                ])]))]).

start(item(n)).
