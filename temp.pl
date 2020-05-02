% Temporary files in http sessions

:- use_module(library(http/http_session)).
:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).

% Check if temporary file exists
tempfile(Id, File, SessionId) :-
    http_session_data(temp(Id, File), SessionId).

% Create temporary file and register it in the session
tempfile(Id, File, Stream, SessionId) :-
    tempfile(Id, File, Stream, [encoding(utf8), extension(txt)], SessionId).

tempfile(_Id, File, Stream, Options, []) :-
    !, 
    tmp_file_stream(File, Stream, Options).

tempfile(Id, File, Stream, Options, SessionId) :-
    tempfile(Id, File, Stream, Options, []),
    http_session_assert(temp(Id, File), SessionId).

% Delete temporary files at session cleanup
:- listen(http_session(end(SessionId, _Peer)), delete_session_temp(SessionId)).

delete_session_temp(SessionId) :-
    findall(F, (http_session_data(temp(_, F), SessionId), delete_file(F)), _).

% Create CSV file from R dataframe
csvfile(Id, Data, SessionId) :-
    tempfile(Id, _, Stream, [encoding(utf8), extension(csv)], SessionId),
    r_data_frame_colnames(Data, Names),
    r_data_frame_to_rows(Data, row, Rows),
    Header =.. [row | Names],
    csv_write_stream(Stream, [Header | Rows], [separator(0';)]),
    close(Stream).
