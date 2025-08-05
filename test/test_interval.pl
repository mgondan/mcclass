:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../interval').

test_interval :-
    run_tests([fractions, number_digit, bugs, multiply, available, equality, ci, pm, denote, color, semicolon, curly, cbinom, pwbinom]).

:- begin_tests(fractions).

test(frac) :-
    A = 1...2,
    B = 2...4,
    interval(frac(A, B), Res),
    equal(Res, 0.2468...1.0101).

test(dfrac) :-
    A = 1...2,
    B = 2...4,
    interval(dfrac(A, B), Res),
    equal(Res, 0.2468...1.0101).

:- end_tests(fractions).

:- begin_tests(number_digit).

test(tstat_atomic) :-
    A = 1,
    B = 3,
    interval(tstat(A / B), L...U),
    L = 0.33,
    U = 0.34.

test(tstat_interval) :-
    A = 1...5,
    B = 3...6,
    interval(tstat(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(hdrs_atomic) :-
    A = 1,
    B = 3,
    interval(hdrs(A / B), L...U),
    L = 0.3,
    U = 0.4.

test(hdrs_interval) :-
    A = 1...5,
    B = 3...6,
    interval(hdrs(A / B), L...U),
    L is 0.1,
    U is 1.7.

test(chi2ratio_atomic) :-
    A = 1,
    B = 3,
    interval(chi2ratio(A / B), L...U),
    L = 0.33,
    U = 0.34.

test(chi2ratio_interval) :-
    A = 1...5,
    B = 3...6,
    interval(chi2ratio(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(pval_atomic) :-
    A = 1,
    B = 3,
    interval(pval(A / B), Res),
    Res = pval(R),
    R > 0.333, 
    R < 0.334.

test(pval_interval) :-
    A = 1...5,
    B = 3...6,
    interval(pval(A / B), Res),
    Res = pval(L...U),
    L > 0.1666,
    L < 0.1667,
    U > 1.6666,
    U < 1.6667.

:- end_tests(number_digit).

:- begin_tests(bugs).

test(omit_right_atomic) :-
    Bug = bug1,
    A = 5,
    B = 4,
    interval(omit_right(Bug, A - B), Res),
    Res = A.

test(omit_right_interval) :-
    Bug = bug1,
    A = 11...12,
    B = 20...21,
    interval(omit_right(Bug, A - B), Res),
    Res = A.

test(omit_left_atomic) :-
    Bug = bug1,
    A = 5,
    B = 4,
    interval(omit_left(Bug, A - B), Res),
    Res = B.

test(omit_left_interval) :-
    Bug = bug1,
    A = 11...12,
    B = 20...21,
    interval(omit_left(Bug, A - B), Res),
    Res = B.

test(omit) :-
    A = bug1,
    B = expr,
    interval(omit(A, B), Res),
    Res = na.

test(instead1) :-
    Bug = bug1,
    Wrong = 2+1,
    Correct = 2-1,
    interval(instead(Bug, Wrong, Correct), Res),
    Res = 3.

test(instead2) :-
    Bug = bug1,
    Wrong = 2+1,
    Correct = 2-1,
    Correct0 = 1+1,
    interval(instead(Bug, Wrong, Correct, Correct0), Res),
    Res = 3.

test(drop_right1) :-
    Bug = bug1,
    A = (3+1) / (1+1),
    interval(drop_right(Bug, A), Res),
    Res = 4.

test(drop_left1) :-
    Bug = bug1,
    A = (3+1) / (1+1),
    interval(drop_left(Bug, A), Res),
    Res = 2.  

test(add_right1) :-
    Bug = bug1,
    A = (3+1) / (2+2),
    interval(add_right(Bug, A), Res),
    Res = 1.  

test(add_left1) :-
    Bug = bug1,
    A = (3+1) / (2+2),
    interval(add_left(Bug, A), Res),
    Res = 1.  

:- end_tests(bugs).

:- begin_tests(multiply).

test(dot_atomic) :-
    A = 2,
    B = 3,
    interval(dot(A, B), Res),
    Res is 6.

test(dot_interval) :-
    A = 2...3,
    B = 3...4,
    interval(dot(A, B), L...U),
    L is 6,
    U is 12.

:- end_tests(multiply).

:- begin_tests(available).

test(available_atomic1) :-
    A = 1,
    B = 2,
    interval(available(A + B), Res),
    Res = true.

test(available_atomic2) :-
    A = 0,
    B = 0,
    interval(available(A / B), Res),
    Res = false.

test(available_interval) :-
    A = 1...3,
    B = 2...5,
    interval(available(A + B), Res),
    Res = true.

test(available_float) :-
    A = 1.1...3.1,
    B = 2.1...5.1,
    interval(available(A + B), Res),
    Res = true.

test(not_available_nan) :-
    interval(available(0 / 0), Res),
    Res = false.

test(available_ci_atomic) :-
    interval(available(ci(5 + 1, 6 + 1)), Res),
    Res = true.

test(not_available_ci_atomic) :-
    interval(available(ci(0 / 0, 1)), Res),
    Res = false.

test(available_ci_interval) :-
    interval(available(ci(5...6 / 2...3, 5...6 / 1...2)), Res),
    Res = true.

test(available_ci_neginf) :-
    interval(available(ci(5...6 / 2...3, 1.0Inf)), Res),
    Res = true.

test(available_ci_ninfpos) :-
    interval(available(ci(-1.0Inf, 5...6 / 2...3)), Res),
    Res = true.

:- end_tests(available).

:- begin_tests(equality).

test(equality1) :-
    interval(5 =@= 5, Res),
    Res = true.

test(equality2) :-
    interval(5 =@= 4, Res),
    Res = false.

test(equality3) :-
    interval(1...2 =@= 1, Res),
    Res = true.

test(equality4) :-
    interval(1...2 =@= 2, Res),
    Res = true.

test(equality5) :-
    interval(1...2 =@= 1.5, Res),
    Res = true.

test(equality6) :-
    interval(1...2 =@= 0.9, Res),
    Res = false.

test(equality7) :-
    interval(1...2 =@= 2.1, Res),
    Res = false.

test(equality8) :-
    interval(1 =@= 1...2, Res),
    Res = true.

test(equality9) :-
    interval(2 =@= 1...2, Res),
    Res = true.

test(equality10) :-
    interval(1.5 =@= 1...2, Res),
    Res = true.

test(equality11) :-
    interval(0.9 =@= 1...2, Res),
    Res = false.

test(equality12) :-
    interval(2.1 =@= 1...2, Res),
    Res = false.

test(equality13) :-
    interval(1...2 =@= 3...4, Res),
    Res = false.

test(equality14) :-
    interval(1...2 =@= 2...3, Res),
    Res = true.

test(equality15) :-
    interval(1...2 =@= 1.5...4, Res),
    Res = true.

test(equality16) :-
    interval(1...2 =@= 1...4, Res),
    Res = true.

test(equality17) :-
    interval(1...2 =@= 0...4, Res),
    Res = true.

test(equality18) :-
    interval(ci(1, 2) =@= ci(2, 4), Res),
    Res = false.

test(equality19) :-
    interval(ci(1, 2) =@= ci(3, 4), Res),
    Res = false.

test(equality20) :-
    interval(ci(1, 2) =@= ci(1, 4), Res),
    Res = false.

test(equality21) :-
    interval(ci(1, 2) =@= ci(1, 2), Res),
    Res = true.

test(equality22) :-
    interval(ci(1...2, 3...4) =@= ci(3...4, 5...6), Res),
    Res = false.

test(equality23) :-
    interval(ci(1...2, 3...4) =@= ci(3...4, 4...6), Res),
    Res = false.

test(equality24) :-
    interval(ci(1...2, 3...4) =@= ci(3...4, 3.5...6), Res),
    Res = false.

test(equality25) :-
    interval(ci(1...2, 3...4) =@= ci(1.5...4, 3.5...6), Res),
    Res = true.

:- end_tests(equality).

:- begin_tests(ci).

test(ciplus1) :-
    interval(ci(1, 2) + 3, ci(4, 5)).

test(ciplus2) :-
    interval(3 + ci(1, 2), ci(4, 5)).

test(ciminus) :-
    interval(ci(1, 2) - 3, ci(-2, -1)).

test(cimult) :-
    interval(ci(1, 2) * 3, ci(3, 6)).

test(cidiv) :-
    interval(ci(2, 4) / 8, ci(0.25, 0.5)).

test(ciexp) :-
    interval(exp(ci(1, 2)), ci(A, B)),
    test_interval:equal(A, A1),
    test_interval:equal(B, B1),
    A1 = 2.7182...2.7183,
    B1 = 7.389...7.3891.

test(onetailed_neginf) :-
    A = 1...2,
    interval(neginf(A), Res),
    Res = ci(A, 1.0Inf).

test(onetailed_ninfpos) :-
    A = 1...2,
    interval(ninfpos(A), Res),
    Res = ci(-1.0Inf, A).

test(assign_ci1) :-
    A = ci(5.1, 5.6),
    interval(<-(ci,A), Res),
    interval(r(ci), Res).

test(assign_ci2) :-
    A = ci(5.1...5.6, 5.9...6.5),
    interval(<-(ci,A), Res),
    interval(r(ci), Res).

:- end_tests(ci).

:- begin_tests(pm).

test(pm) :-
    A = 0,
    B = 1,
    interval(pm(A, B), Res),
    Res = ci(-1, 1).

:- end_tests(pm).

/* :- begin_tests(eq).

test(equ1) :-
    Name = mean,
    A = (1 + 2)/2,
    interval(Name=A, Res),
    Res = (mean=1.5). 

:- end_tests(eq). */

:- begin_tests(denote).

test(denote1) :-
    Sym = sd_pool,
    A = 5 + 5,
    Text = 'Pooled standard deviation',
    interval(denote(Sym, A, Text), Res),
    Res = 10. 

:- end_tests(denote).

:- begin_tests(color).

test(color1) :-
    Col = red,
    A = 5 + 5,
    interval(color(Col, A), Res),
    Res = 10. 

:- end_tests(color).

:- begin_tests(semicolon).

test(semicolon1) :-
    A = 1 + 2,
    B = 5 + 5,
    interval(';'(A, B), Res),
    Res = 10. 

:- end_tests(semicolon).

:- begin_tests(curly).

test(curly1) :-
    A = 1 + 2,
    interval('{}'(A), Res),
    Res = 3. 

:- end_tests(curly).

:- begin_tests(cbinom).

test(cbinom1) :-
    interval(cbinom(0.5, 20, 0.7, "upper", "min"), Res),
    Res = 15.0. 

test(cbinom2) :-
    interval(cbinom(0.5, 20, 0.7, "lower", "max"), Res),
    Res = 13.0. 

test(cbinom3) :-
    interval(cbinom(0.5...0.6, 20...21, 0.7...0.8, "upper", "min"), Res),
    Res = 15.0...18.0. 

test(cbinom4) :-
    interval(cbinom(0.5...0.6, 20...21, 0.7...0.8, "lower", "max"), Res),
    Res = 13.0...16.0. 

:- end_tests(cbinom).

:- begin_tests(pwbinom).

test(pwbinom1) :-
    interval(pwbinom(10, 20, 0.7, "lower"), Res),
    equal(Res, 0.0479...0.0480).

test(pwbinom2) :-
    interval(pwbinom(10, 20, 0.7, "upper"), Res),
    equal(Res, 0.9828...0.9829).

test(pwbinom3) :-
    interval(pwbinom(10, 20, 0.7, "densi"), Res),
    equal(Res, 0.0308...0.0309).

test(pwbinom4) :-
    interval(pwbinom(10, 20, 0.7...0.8, "lower"), Res),
    equal(Res, 0.0025...0.0480).

test(pwbinom5) :-
    interval(pwbinom(10, 20, 0.7...0.8, "upper"), Res),
    equal(Res, 0.9828...0.9995).

:- end_tests(pwbinom).

:- begin_tests(input).

test(input1) :-
    interval(input(0.7), Res),
    Res = 0.695...0.705.

test(input2) :-
    interval(input(0), Res),
    Res = -0.005...0.005.

test(input3) :-
    interval(input(ci(1, 2)), Res),
    Res = ci(0.995...1.005, 1.995...2.005).

test(input4) :-
    interval(input(ci(1...2, 3...4)), Res),
    Res = ci(0.995...2.005, 2.995...4.005).

:- end_tests(input).
% Helper predicate to check equality
equal(Res0, Res) :-
    interval(round(Res0, 4), Res).