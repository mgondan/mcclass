% Temporary files in http sessions

:- dynamic temp/2.
:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).

% Check if temporary file exists
tempfile(Id, File) :-
    temp(Id, File).

% Create temporary file and register it in the session
tempfile(Id, File, Stream) :-
    tempfile(Id, File, Stream, [encoding(utf8), extension(txt)]).

tempfile(Id, File, Stream, Options) :-
    !, 
    tmp_file_stream(File, Stream, Options),
    assert(temp(Id, File)).

% Create CSV file from R dataframe
csvfile(Id, _) :-
    temp(Id, _),
    !.

csvfile(Id, Data) :-
    tempfile(Id, _, Stream, [encoding(utf8), extension(csv)]),
    r_data_frame_colnames(Data, Names),
    r_data_frame_to_rows(Data, row, Rows),
    Header =.. [row | Names],
    csv_write_stream(Stream, [Header | Rows], [separator(0';)]),
    close(Stream).

