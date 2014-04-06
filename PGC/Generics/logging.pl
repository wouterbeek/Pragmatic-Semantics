:- module(
  logging,
  [
    append_to_log/1, % +Format:atom
    append_to_log/2, % +Format:atom
                     % +Arguments:list(term)
    append_to_log/3, % +Category:atom
                     % +Format:atom
                     % +Arguments:list(term)
    close_log_stream/0,
    current_log_file/1, % ?File:file
    current_log_stream/1, % ?Stream:stream
    current_situation/1, % ?Situation:atom
    send_current_log_file/0,
    set_current_log_file/1, % ?File:atom
    set_current_log_stream/1, % ?Stream:stream
    set_situation/1, % +Situation:atom
    start_log/0
  ]
).

/** <module> Logging

Methods for logging.

Logging is not possible if only the PGC is loaded, since it
requires an outer project (with directory mnemonic =project=).

The situation is used to tell different uses of the same codebase
into account (examples of uses are =debug=, =production=, =experiment7=).

Any logging has to be preceded by a call to start_log/0 which
initializes the log file and stream.

Logs are stored in a hidden subdirectory of the user's home.
The individual files are stored by situation, year, month, and day.
The file name indicates the hour, minute, and second at which
logging started.

@author Wouter Beek
@author Sander Latour
@version 2012/05-2012/07, 2013/03-2013/07, 2013/09, 2013/11, 2014/01-2014/02
*/

:- use_module(generics(codes_ext)).
:- use_module(generics(db_ext)).
:- use_module(library(ansi_term)). % Used in markup.
:- use_module(library(debug)).
:- use_module(library(http/http_client)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(dir_infra)).
:- use_module(os(file_ext)).

:- discontiguous(prolog:message/1).
:- multifile(prolog:message/1).

:- dynamic(current_log_file/1).
:- dynamic(current_log_stream/1).
:- dynamic(situation/1).

:- db_add_novel(user:prolog_file_type(log, log)).



%! append_to_log(+Format:atom) is det.
% Logs the given message in the current log file.
%
% @arg Format An atomic message.
% @see Like format/1.

append_to_log(Format):-
  append_to_log(Format, []).

%! append_to_log(+Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file.
%
% @arg Format An atomic message.
% @arg Arguments A list of terms.
% @see Like format/2.

append_to_log(Format, Arguments):-
  append_to_log(generic, Format, Arguments).

%! append_to_log(+Category:atom, +Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file under the given category.
%
% @arg Category An atom.
% @arg Format An atomic message.
% @arg Arguments A list of terms.

append_to_log(Category, Format, Arguments):-
  format(atom(Msg), Format, Arguments),
  append_to_log_(Category, Msg).

append_to_log_(Category, Msg):-
  \+ current_log_stream(_Stream), !,
  print_message(warning, cannot_log(Category, Msg)).
append_to_log_(Category, Msg1):-
  current_log_stream(Stream), !,
  iso8601_dateTime(DateTime),
  atom_codes(Msg1, Codes1),
  codes_remove(Codes1, [10,13], Codes2),
  atom_codes(Msg2, Codes2),
  csv_write_stream(
    Stream,
    [row(DateTime, Category, Msg2)],
    [file_type(comma_separated_values)]
  ),
  flush_output(Stream).
prolog:message(cannot_log(Kind, Msg)):-
  [
    ansi([bold], '[~w] ', [Kind]),
    ansi([], 'Could not log message "', []),
    ansi([faint], '~w', [Msg]),
    ansi([], '".', [])
  ].

%! close_log_stream is det.
% Closes the current log stream.

close_log_stream:-
  \+ current_log_stream(_Stream), !,
  print_message(warning, no_current_log_stream).
close_log_stream:-
  current_log_stream(Stream),
  flush_output(Stream),
  close(Stream).
prolog:message(no_current_log_stream):-
  [ansi([], 'There is no current log stream.', [])].

%! create_log_file(-File:atom, -Stream) is det.
% Creates a log file in the log file directory and returns the absolute
% path of that file as well as its stream name.
%
% @arg File The atomic name of a file's path.
% @arg Stream The atomic name of a file's stream.

create_log_file(File, Stream):-
  current_situation(Situation),
  create_log_file(Situation, File, Stream).

%! create_log_file(+Situation:atom, -File:atom, -Stream:atom) is det.
% Creates a log file in the log file directory and returns the absolute
% path of that file as well as its stream.
%
% @arg Situation An atomic descriptor of a logging situation.
% @arg File The atomic name of a file's path.
% @arg Stream The atomic name of a file's stream.

create_log_file(Situation, AbsoluteFile, Stream):-
  absolute_file_name(log(Situation), Dir),
  date_directories(Dir, LogDir),
  current_time(FileName),
  create_file(LogDir, FileName, log, AbsoluteFile),
  open(
    AbsoluteFile,
    write,
    Stream,
    [close_on_abort(true),encoding(utf8),type(test)]
  ).

current_situation(Situation):-
  situation(Situation), !.
current_situation(no_situation).

%! end_log is det.
% Ends the current logging activity.

end_log:-
  \+ current_log_file(_File), !.
end_log:-
  append_to_log(build, 'Goodnight!', []),
  send_current_log_file,
  close_log_stream.

init:-
  file_search_path(log, _Dir), !.
init:-
  create_personal_subdirectory('Logging', Absolute),
  assert(user:file_search_path(log, Absolute)).

%! send_current_log_file(File) is det.
% Sends the log that is stored in the given file to the logging server.
%
% @arg File The atomic name of a file.
% @tbd Construct the URL using =library(uri)=.
% @tbd Add the PHP script (I seem to have deleted it on the remote :-().

send_current_log_file:-
  \+ current_log_file(_File), !,
  print_message(warning, no_current_log_file).
send_current_log_file:-
  current_log_file(File),
  open(File, read, Stream, [encoding(utf8),type(test)]),
  read_stream_to_codes(Stream, Codes),
  file_base_name(File, Base),
  format(atom(URL), 'http://www.wouterbeek.com/post.php?filename=~w', [Base]),
  catch(
    http_post(URL, codes('text/xml;charset=utf-8', Codes), Reply, []),
    _Error,
    fail
  ),
  debug(logging, 'HTTP reply upon sending the log file:\n~w', [Reply]).
prolog:message(no_current_log_file):-
  [ansi([], 'There is no current log file.', [])].

%! set_current_log_file(+File:atom) is det.
% Sets the current file where logging messages are stored to.
%
% @arg File The atomic name of a file.

set_current_log_file(File):-
  exists_file(File),
  retractall(current_log_file(_File)),
  assert(current_log_file(File)).

%! set_current_log_stream(+Stream) is det.
% Sets the current stream where logging messages are written to.
%
% @arg Stream A stream.

set_current_log_stream(Stream):-
  is_stream(Stream),
  retractall(current_log_stream(_Stream)),
  assert(current_log_stream(Stream)).

set_situation(Situation):-
  nonvar(Situation),
  db_replace_novel(_, Situation).

%! start_log is det.
% Starts logging.
% This does nothing in case log mode is turned off.

start_log:-
  current_log_stream(_Stream), !,
  print_message(warning, already_logging).
start_log:-
  init,
  create_log_file(File, Stream),
  set_current_log_file(File),
  set_current_log_stream(Stream),
  append_to_log(build, 'Goodday!', []),
  % Make sure logging stops once the program ends.
  at_halt(end_log).
prolog:message(already_logging):-
  [ansi([], 'Logging was already started. End logging before starting it again.', [])].

