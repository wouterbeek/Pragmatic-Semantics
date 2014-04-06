:- module(
  shell_ext,
  [
    terminal_screen_width/1, % -ScreenWidth:nonneg
    windows_shell_command/1, % +Command
    windows_shell_command/2 % +Command
                            % +File:atom
  ]
).

/** <module> Shell extensions

Communication with the shell and terminal properties.

@author Wouter Beek
@version 2013/06, 2013/11, 2014/01
*/

:- use_module(library(process)).
:- use_module(os(os_ext)).



%! terminal_screen_width(-ScreenWidth:nonneg) is det.

% Use the `termcap` library.
terminal_screen_width(ScreenWidth):-
  os_dependent_call(terminal_screen_width(ScreenWidth)).
:- if(is_unix).
terminal_screen_width_unix(ScreenWidth):-
  tty_get_capability(co, number, ScreenWidth).
:- endif.
:- if(is_windows).
terminal_screen_width_windows(80).
:- endif.


%! windows_shell_command(+Command) is det.
%! windows_shell_command(+Command, +File:atom) is det.
% @tbd Test this.

windows_shell_command(Command):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C',Command],
    [stdin(std),stdout(std),stderr(std)]
  ).

windows_shell_command(Command, File):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C',Command,file(File)],
    [stdin(std),stdout(std),stderr(std)]
  ).

