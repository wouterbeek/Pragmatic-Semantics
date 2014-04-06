:- module(
  java_ext,
  [
    java_output_stream/1, % -Output
    java_error_stream/1, % -Error
    run_jar/2, % +JAR_File:atom
               % +CommandlineArguments:list(atom
    run_jar/4 % +JAR_File:atom
              % +CommandlineArguments:list(atom
              % +Output
              % +Error
  ]
).

/** <module> Java extensions

Extensions for executing Java JARs from within Prolog.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_remote_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(process)).
:- use_remote_module(os(run_ext)).

% JAR
:- db_add_novel(user:prolog_file_type(jar, jar)).

% LOG
:- db_add_novel(user:prolog_file_type(log, log)).



java_output_stream(Output):-
  absolute_file_name(
    data(java_output),
    OutputFile,
    [access(write),file_type(log)]
  ),
  open(OutputFile, append, Output, []).


java_error_stream(Error):-
  absolute_file_name(
    data(java_error),
    ErrorFile,
    [access(write),file_type(log)]
  ),
  open(ErrorFile, append, Error, []).


%! run_jar(+JAR_File:atom, +CommandlineArguments:list(atom)) is det.
% Runs the given JAR file with the given commandline arguments,
% and uses the Java error and output streams as defined by this module:
%   - java_error_stream/1
%   - java_output_stream/1

run_jar(JAR_File, Args):-
  setup_call_cleanup(
    (
      java_output_stream(Output1),
      java_error_stream(Error1)
    ),
    (
      run_jar(JAR_File, Args, pipe(Output2), pipe(Error2)),
      copy_stream_data(Output2, Output1),
      copy_stream_data(Error2, Error1)
    ),
    maplist(close, [Error1,Error2,Output1,Output2])
  ).


%! run_jar(
%!   +JAR_File:atom,
%!   +CommandlineArguments:list(atom),
%!   +Error:stream,
%!   +Output:stream
%! ) is det.
% Runs the given JAR file with the given commandline arguments,
% and uses the given output streams for Java error and Java output.

run_jar(JAR_File, Args, Error, Output):-
  process_create(
    path(java),
    ['-jar',file(JAR_File)|Args],
    [process(PID),stderr(Error),stdout(Output)]
  ),
  process_wait(PID, exit(ShellStatus)),
  
  % Process shell status.
  file_base_name(JAR_File, JAR_Name),
  format(atom(Program), 'Java/JAR ~a', [JAR_Name]),
  exit_code_handler(Program, ShellStatus).

