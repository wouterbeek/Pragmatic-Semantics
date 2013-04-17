% This file loads the PraSem project in a developer-centric way (the only
% 'proper' way :-)).
% Maybe the developer/user distinction makes no sense for an NWO project
% like PraSem anyway, but I see this distinction as one of the remnants
% in my mind from the D*L*n project, where the User was always more
% important than performing proper research (I'm just quoting the evaluation
% report of the project here).

:- use_module(library(http/http_path)).
:- use_module(library(pldoc)).
:- use_module(library(portray_text)).

documentation(off).
%documentation(on).

%wallace(off).
wallace(on).

:-
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),
  % The generic load file, shared by the developed and startup/user files.
  ensure_loaded('load.pl'),
  logging:enable_log_mode,
  set_situation(developer),

  % Before doing anything else, we start the documentation server that
  % generates Web sites based on the plDoc commenting in the swipl code files.
  (
    documentation(on)
  *->
    doc_server(2222, [edit(true)])
  ;
    true
  ),

  % Enforce more stringent style checking.
  style_check(+string),
  style_check(+charset),

  % This library allows for exploiting the color and attribute facilities
  % of most modern terminals using ANSI escape sequences.
  % The Windows console (swipl-win) does not (yet) support ANSI (color)
  % codes.
  ensure_loaded(library(ansi_term)),

  % Write lists of ASCII numbers as strings to the terminal.
  portray_text(true),

  % Set the swipl terminal state via PCE.
  % When swipl is started from within a terminal this does not change
  % anything, so this setting applies e.g. to contexts where PraSem
  % would be loaded by a shortcut on the Desktop.
  %ignore(send(@pce, show_console, iconic)),
  ignore(send(@pce, show_console, open)),

  % Debug monitor.
  % [TODO] The PCE-based debug monitor in swipl is not the most versatile
  % debug tool in existence. I would like to write a Web-based version at
  % some point.
  %prolog_ide(debug_monitor),

  % Now comes PraSem.
  ensure_loaded(prasem(prasem)),
  start_prasem.

