:- module(test_tasks, [test_tasks/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../tasks').

test_tasks :-
    run_tests([anova, binomial, chisq, odds, regression, t, ztrans]).

% Anova
:- begin_tests(anova).

test(baseline_fratio) :-
    tasks:tasks(baseline, fratio).

test(baseline_pvalue) :-
    tasks:tasks(baseline, pvalue).

test(baseline_cibase) :-
    tasks:tasks(baseline, cibase).

test(subgroups_fratio) :-
    tasks:tasks(subgroups, fratio).

test(subgroups_pvalue) :-
    tasks:tasks(subgroups, pvalue).

test(subgroups_cibase) :-
    tasks:tasks(subgroups, cibase).

:- end_tests(anova).

% Binomial distribution
:- begin_tests(binomial).

test(dbinom_exactprob) :-
    tasks:tasks(dbinom, exactprob).

test(testbinom_powbinom) :-
    tasks:tasks(testbinom, powbinom).
    
test(testbinom_critical) :-
    tasks:tasks(testbinom, critical).

test(testbinom_pvalue) :-
    tasks:tasks(testbinom, pval).

test(power_power) :-
    tasks:tasks(power, power).

:- end_tests(binomial).

% Chi-square
:- begin_tests(chisq).

test(chisq_chisq) :-
    tasks:tasks(chisq, chisq).

:- end_tests(chisq).

% Odds ratio
:- begin_tests(odds).

test(easyodds_oratio) :-
    tasks:tasks(easyodds, oratio).

test(oddsratio_oratio) :-
    tasks:tasks(oddsratio, oratio).

test(oddsratio2_oratio) :-
    tasks:tasks(oddsratio2, oratio).

:- end_tests(odds).

% Lineare regression
:- begin_tests(regression).

test(regression_bcoef) :-
    tasks:tasks(regression, bcoef).

test(regression_pvalue) :-
    tasks:tasks(regression, pvalue).

:- end_tests(regression).

% t-test: independent, paired
:- begin_tests(t).

test(tgroups_s2p) :-
    tasks:tasks(tgroups, s2p).

test(tgroups_tratio) :-
    tasks:tasks(tgroups, tratio).

test(tgroups_cigroups) :-
    tasks:tasks(tgroups, cigroups).

test(tpaired_tratio) :-
    tasks:tasks(tpaired, tratio).

test(tpaired_pvalue) :-
    tasks:tasks(tpaired, pvalue).

test(tpaired_cipaired) :-
    tasks:tasks(tpaired, cipaired).

test(tpairedupper_tratio) :-
    tasks:tasks(tpairedupper, tratio).

test(tpairedupper_pvalue) :-
    tasks:tasks(tpairedupper, pvalue).

test(tpairedupper_cipaired) :-
    tasks:tasks(tpairedupper, cipaired).

test(tpaired1tlow_tratio) :-
    tasks:tasks(tpaired, tratio).

test(tpaired1tlow_pvalue) :-
    tasks:tasks(tpaired, pvalue).

test(tpaired1tlow_cipaired) :-
    tasks:tasks(tpaired, cipaired).

:- end_tests(t).

% z-transformation
:- begin_tests(ztrans).

test(ztrans_prob) :-
    tasks:tasks(ztrans, prob).

test(ztrans_quantile) :-
    tasks:tasks(ztrans, quantile).

:- end_tests(ztrans).