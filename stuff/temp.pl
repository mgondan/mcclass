% Data files
:- dynamic temp/3.
:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).

% Create CSV file from R dataframe
csvfile(Id, _) :-
    temp(Id, _, csv),
    !.

csvfile(Id, Data) :-
    format(string(File), "~k.csv", [Id]),
    r_data_frame_colnames(Data, Names),
    r_data_frame_to_rows(Data, row, Rows),
    Header =.. [row | Names],
    csv_write_file(File, [Header | Rows], [separator(0';), encoding(utf8)]),
    assert(temp(Id, File, csv)).

xlsxfile(Id, _) :-
    temp(Id, _, xlsx),
    !.

xlsxfile(Id, Data) :-
    absolute_file_name(Id, Absolute),
    format(string(File), "~w.xlsx", [Absolute]),
    <- library('WriteXLS'),
    <- 'WriteXLS'(Data, File),
    assert(temp(Id, File, xlsx)).
