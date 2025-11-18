:- module(test_tasks, [test_tasks/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../tasks').

test_tasks :-
    run_tests([anova, binomial, chisq, odds, regression, t, ztrans]).

test_task_(Topic, Task, NumberSolutions, NumberMistakes) :-
    open_null_stream(Null),
    with_output_to(Null, call(tasks:tasks(Topic, Task))),
    tasks:taskdata(Topic, Task, _, [solutions(Sol), mistakes(Mist)]),
    length(Sol, NumberSolutions),
    length(Mist, NumberMistakes).

% Message in case of fail
test_task_(Topic, Task, NumberSolutions, NumberMistakes) :-
    tasks:taskdata(Topic, Task, _, [solutions(S), mistakes(M)]),
    length(S, ActualNumberSolutions),
    length(M, ActualNumberMistakes),
    atomics_to_string([Topic, " ", Task, " - Solutions: actual=", ActualNumberSolutions, ", exptected=", NumberSolutions,
                       " - Mistakes: actual=", ActualNumberMistakes, ", exptected=", NumberMistakes], Message),
    writeln(Message),
    fail.

% Anova
:- begin_tests(anova).

test(baseline_fratio) :-
    test_task_(baseline, fratio, 1, 29).

test(baseline_pvalue) :-
    test_task_(baseline, pvalue, 1, 29).

test(baseline_cibase) :-
    test_task_(baseline, cibase, 1, 29).

test(subgroups_fratio) :-
    test_task_(subgroups, fratio, 1, 49).

test(subgroups_pvalue) :-
    test_task_(subgroups, pvalue, 1, Mistakes),
    (Mistakes = 85
    ;
    Mistakes = 87).

test(subgroups_cibase) :-
    test_task_(subgroups, cibase, 1, 179).

:- end_tests(anova).

% Binomial distribution
:- begin_tests(binomial).

test(dbinom_exactprob) :-
    test_task_(dbinom, exactprob, 2, 18).

test(dbinom_exactseq) :-
    test_task_(dbinom, exactseq, 1, 0).

test(dbinom_succrun) :-
    test_task_(dbinom, succrun, 1, 0).

test(sequence_exactprob) :-
    test_task_(sequence, exactprob, 2, 18).

test(sequence_exactseq) :-
    test_task_(sequence, exactseq, 1, 0).

test(sequence_succrun) :-
    test_task_(sequence, succrun, 1, 0).

test(testbinom_powbinom) :-
    test_task_(testbinom, powbinom, 1, 11).
    
test(testbinom_critical) :-
    test_task_(testbinom, critical, 1, 3).

test(testbinom_pvalue) :-
    test_task_(testbinom, pval, 1, 11).

:- end_tests(binomial).

% Chi-square
:- begin_tests(chisq).

test(chisq_chisq) :-
    test_task_(chisq, chisq, 1, 35).

:- end_tests(chisq).

% Odds ratio
:- begin_tests(odds).

test(oddsratio_oratio) :-
    test_task_(oddsratio, oratio, 1, 17).

test(oddsratio_successprob) :-
    test_task_(oddsratio, successprob, 1, 11).

:- end_tests(odds).

% Lineare regression
:- begin_tests(regression).

test(regression_bcoef) :-
    test_task_(regression, bcoef, 1, 3).

test(regression_pvalue) :-
    test_task_(regression, pvalue, 1, 3).

:- end_tests(regression).

% t-test: independent, paired
:- begin_tests(t).

test(tgroups_s2p) :-
    test_task_(tgroups, s2p, 1, 3).

test(tgroups_tratio) :-
    test_task_(tgroups, tratio, 1, 35).

test(tgroups_cigroups) :-
    test_task_(tgroups, cigroups, 1, 79).

test(tpaired_tratio) :-
    test_task_(tpaired, tratio, 2, 86).

test(tpaired_pvalue) :-
    test_task_(tpaired, pvalue, 1, 3).

test(tpaired_cipaired) :-
    test_task_(tpaired, cipaired, 1, 15).

test(tpairedupper_tratio) :-
    test_task_(tpairedupper, tratio, 1, 50).

test(tpairedupper_pvalue) :-
    test_task_(tpairedupper, pvalue, 2, 6).

test(tpairedupper_cipaired) :-
    test_task_(tpairedupper, cipaired, 1, 15).

test(tpairedlower_tratio) :-
    test_task_(tpairedlower, tratio, 1, 50).

test(tpairedlower_pvalue) :-
    test_task_(tpairedlower, pvalue, 2, 6).

test(tpairedlower_cipaired) :-
    test_task_(tpairedlower, cipaired, 1, 9).

:- end_tests(t).

% z-transformation
:- begin_tests(ztrans).

test(ztrans_prob) :-
    test_task_(ztrans, prob, 1, 6).

test(ztrans_quantile) :-
    test_task_(ztrans, quantile, 1, 23).

:- end_tests(ztrans).