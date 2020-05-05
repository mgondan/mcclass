% Temporary files in http sessions

:- dynamic temp/2.
:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).

% Check if temporary file exists
tempfile(Id, File) :-
    temp(Id, File).

% Create CSV file from R dataframe
csvfile(Id, _) :-
    temp(Id, _),
    !.

csvfile(Id, Data) :-
    format(string(File), "~k.csv", [Id]),
    r_data_frame_colnames(Data, Names),
    r_data_frame_to_rows(Data, row, Rows),
    Header =.. [row | Names],
    csv_write_file(File, [Header | Rows], [separator(0';), encoding(utf8)]),
    assert(temp(Id, File)).

