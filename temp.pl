% Temporary files in http sessions

:- use_module(library(http/http_session)).
:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).

% Check if temporary file exists
tempfile(Id, File) :-
    http_session_data(tempfile(Id, File)).

% Create temporary file and register it in the session
tempfile(Id, File, Stream) :-
    tempfile(Id, File, Stream, [encoding(utf8), extension(txt)]).

tempfile(Id, File, Stream, Options) :-
    tmp_file_stream(File, Stream, Options),
    ( http_in_session(_)
      -> http_session_assert(tempfile(Id, File))
       ; true
    ).

% Delete temporary files at session cleanup
:- listen(http_session(end(Session, _Peer)), delete_session_temp(Session)).

delete_session_temp(Session) :-
    findall(F, (http_session_data(tempfile(_, F), Session), delete_file(F)), _).

% Create CSV file from R dataframe
csvfile(Id, Data) :-
    tempfile(Id, _, Stream, [encoding(utf8), extension(csv)]),
    r_data_frame_colnames(Data, Names),
    r_data_frame_to_rows(Data, row, Rows),
    Header =.. [row | Names],
    csv_write_stream(Stream, [Header | Rows], [separator(0';)]),
    close(Stream).
