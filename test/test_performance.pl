:- module(test_performance, [test_performance/0, test_performance/1, clear_log/0]).

:- use_module('../interval.pl').
:- use_module('../tasks.pl').
:- use_module(library(date)). 

:- initialization(init).

init :-
    tasks:interval(1 + 1, _Res).

log_file(Path) :-
    source_file(test_performance:test_performance, Module),  
    file_directory_name(Module, Dir),  
    atomic_list_concat([Dir, 'performance_log.txt'], '/', Path).

clear_log :-
    log_file(Path),
    exists_file(Path), 
    delete_file(Path).

log_header :-
    get_time(Timestamp),                  
    format_time(atom(Date), '%Y-%m-%d %H:%M:%S', Timestamp),
    log_file(Path),
    open(Path, append, Stream),  
    format(Stream, '---------\n~w \n', [Date]), 
    close(Stream).

log_result(Test, Inferences, Opt) :-
    log_file(Path),
    open(Path, append, Stream),  
    format(Stream, '~w | Inferences: ~d~w~n', [Test, Inferences, Opt]), 
    format('~w | Inferences: ~d~w~n', [Test, Inferences, Opt]), 
    close(Stream). 

log_trailer :-
    log_file(Path),
    open(Path, append, Stream),  
    writeln(Stream, '---------'), 
    nl(Stream),
    close(Stream).

test_(Name, Call, Opt) :-
    open_null_stream(Null),
    statistics(inferences, Start),
    with_output_to(Null, call(Call)),
    statistics(inferences, End),
    Inferences is End - Start - 3,
    log_result(Name, Inferences, Opt). 

% Tests
test_performance :-
    use_rs_rolog,
    test_performance([test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13]).

test_performance([H | T]) :-
    use_rs_rolog,
    log_header,
    maplist(call, [H | T]),
    log_trailer.

use_rs_rolog :-
    b_setval(http_session_id, default_session), 
    rint:r_initialize(default_session). 

% Numbers
test1 :-
    Call = interval(frac(1.5 + (1 + 1.5), (1 + 1.5) - 0.5) + ((5 * 2)^2 / 10), _Res),
    test_('test1', Call, '').  

% Intervals
test2 :-
    Call = interval(frac(1.5...2.5 + (1.2...2.2 + 1.5...2.3), (1.1...2.2 + 1.5...2.1) - 0.5...0.7) + ((5.1...6.1 * 2.3...5.1)^2 / 10.1...10.9), _Res),
    test_('test2', Call, '').  

% Confidence intervals
test3 :-
    Call = interval(ci(1...2 * 0.5...0.7, 5...7 / 0.4...0.5) - (5...6 + 1...2)^2 , _Res),
    test_('test3', Call, ''). 

% Rint
test4 :-
    Call = interval(pbinom(13, 20, 0.5...0.6, true) + qbinom(0.8...0.9, 20, 0.5...0.6, true) - dbinom(11...12, 20, 0.5...0.6), _Res),
    test_('test4', Call, '').     

% R assignment
test5 :-
    Call = (interval(a <- 5, _), interval(r(a), _)),
    test_('test5', Call, '').  

% Nested expressions
test6 :-
    Level = 6,
    generate(Level, Call),
    atomic_concat(' | Levels=', Level, Opt),
    test_('test6', interval(Call, _), Opt).

test7 :-
    Level = 7,
    generate(Level, Call),
    atomic_concat(' | Levels=', Level, Opt),
    test_('test7', interval(Call, _), Opt).

test8 :-
    Level = 8,
    generate(Level, Call),
    atomic_concat(' | Levels=', Level, Opt),
    test_('test8', interval(Call, _), Opt).

test9 :-
    Level = 9,
    generate(Level, Call),
    atomic_concat(' | Levels=', Level, Opt),
    test_('test9', interval(Call, _), Opt).

generate(Level, 2 + X) :-
    Level > 0,
    Level1 is Level -1,
    generate(Level1, X).

generate(0, 2).

test10 :-
    Topic = easyodds,
    Task = oratio,
    Call = tasks:tasks(Topic, Task),
    open_null_stream(Null),
    with_output_to(Null, call(Call)), 
    atomic_list_concat([' | ', Topic, ':', Task], Opt),
    test_('test10', Call, Opt).

test11 :-
    Topic = testbinom,
    Task = powbinom,
    Call = tasks:tasks(Topic, Task),
    open_null_stream(Null),
    with_output_to(Null, call(Call)), 
    atomic_list_concat([' | ', Topic, ':', Task], Opt),
    test_('test11', Call, Opt).


test12 :-
    Topic = tpaired,
    Task = tratio,
    Call = tasks:tasks(Topic, Task),
    open_null_stream(Null),
    with_output_to(Null, call(Call)), 
    atomic_list_concat([' | ', Topic, ':', Task], Opt),
    test_('test12', Call, Opt).

test13 :-
    Topic = baseline,
    Task = fratio,
    Call = tasks:tasks(Topic, Task),
    open_null_stream(Null),
    with_output_to(Null, call(Call)), 
    atomic_list_concat([' | ', Topic, ':', Task], Opt),
    test_('test13', Call, Opt).