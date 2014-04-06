:- module(ckan_clas, []).

/** <module> CLAN command-line arguments

Command-line argument handling for the CKAN project.

@author Wouter Beek
@version 2014/04
*/

:- use_module(ckan(ckan_catalog)).
:- use_module(ckan(ckan_lod)).
:- use_module(library(aggregate)).

:- dynamic(site/1).

:- multifile(user:cmd_option/4).
:- multifile(user:process_cmd_option/4).



% Site

user:cmd_option(_, site, atom, 'CKAN site operated on.').

user:process_cmd_option(site(Site)):-
  site(Site), !.
user:process_cmd_option(site(Site)):-
  assert(site(Site)).



% Command

user:cmd_option(_, command, atom, 'Command that is executed.').

user:process_cmd_option(command(Command)):-
  command(Command), !.
user:process_cmd_option(command(Command)):-
  print_message(warning, unsupported_command(Command)).

prolog:message(unsupported_command(Command)) -->
  [
    'Command ',
    Command,
    ' is not supported.~nThe following commands are supported:~n'
  ],
  commands.

command(Command) -->
  ['  * ',Command,'~n'].

commands -->
  {
    aggregate_all(
      set(Command),
      command(Command),
      Commands
    )
  },
  commands(Commands).

commands([]) --> [].
commands([H|T]) -->
  command(H),
  commands(T).


command(download_catalog):- !,
  forall(
    site(Site),
    (
      ckan_download_catalog(Site, File),
      prolog_message(informational, ckan_download_catalog(Site, File))
    )
  ).
command(download_lod):- !,
  forall(
    site(Site),
    (
      ckan_download_lod(Site),
      prolog_message(informational, ckan_download_lod(Site))
    )
  ).
command(download_lod):- !,
  aggregate_all(
    set(Site),
    ckan_properties(Site, _),
    Sites
  ),
  prolog_message(informational, ckan_list_sites(Sites)).

prolog:message(ckan_download_catalog(Site, File)) -->
  ['CKAN site ',Site,'\'s catalog was downloaded to file ',File,'.~n'].
prolog:message(ckan_download_lod(Site)) -->
  ['CKAN site ',Site,'\'s LOD was downloaded.~n'].
prolog:message(ckan_list_sites([])) --> [].
prolog:message(ckan_list_sites([H|T])) -->
  ['  * ',H,'~n'],
  prolog:message(ckan_list_sites(T)).

