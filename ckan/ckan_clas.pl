/* CKAN command-line arguments

Command-line argument handling for the CKAN project.

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(option)).

:- use_remote_module(pl(pl_clas)).

:- use_remote_module(prasem, ckan(ckan_catalog)).
:- use_remote_module(prasem, ckan(ckan_db)).
:- use_remote_module(prasem, ckan(ckan_lod)).

:- dynamic(ckan_site/1).

:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).

:- discontiguous(user:process_option/1).
:- multifile(user:process_option/1).

:- multifile(prolog:message//1).

:- discontiguous(command/1).
:- discontiguous(run_command/1).



% Option: site.

user:option_specification([
  help('CKAN site operated on.'),
  longflags([site]),
  opt(site),
  shortflags([]),
  type(atom)
]).


user:process_option(site(Site)):-
  ckan_site(Site), !,
  print_message(information, duplicate_ckan_site(Site)).
user:process_option(site(Site)):-
  \+ ckan_property(Site, _), !,
  print_message(information, unknown_ckan_site(Site)).
user:process_option(site(Site)):-
  assert(ckan_site(Site)).


prolog:message(duplicate_ckan_site(Site)) -->
  ['CKAN site ',Site,' was already registered.'].
prolog:message(no_ckan_site) -->
  ['No CKAN site was specified.'],
  ckan_sites.
prolog:message(unknown_ckan_site(Site)) -->
  ['No CKAN site named \"',Site,'\" is know to this library.~n'],
  ckan_sites.

ckan_sites -->
  ['The following CKAN sites are supported:~n'],
  {aggregate_all(set(Site), ckan_property(Site, _), Sites)},
  ckan_sites(Sites).

ckan_sites([]) --> [].
ckan_sites([H|T]) -->
  ['  * ',H,'~n'],
  ckan_sites(T).



% Option: command.

user:option_specification([
  help('Command that is executed.'),
  longflags([command]),
  opt(command),
  shortflags(c),
  type(atom)
]).


user:process_option(command(Command)):-
  command(Command), !,
  run_command(Command).
user:process_option(command(Command)):-
  print_message(information, ckan_unsupported_command(Command)),
  halt.


command(download_catalog).
run_command(download_catalog):- !,
  forall(
    ckan_site(Site),
    (
      ckan_download_catalog(Site, File),
      print_message(information, ckan_download_catalog(Site, File))
    )
  ).
command(download_lod).
run_command(download_lod):- !,
  forall(
    ckan_site(Site),
    (
      ckan_download_lod(Site),
      print_message(information, ckan_download_lod(Site))
    )
  ).
command(list_sites).
run_command(list_sited):- !,
  aggregate_all(
    set(Site),
    ckan_properties(Site, _),
    Sites
  ),
  print_message(information, ckan_list_sites(Sites)).


prolog:message(ckan_download_catalog(Site, File)) -->
  ['CKAN site ',Site,'\'s catalog was downloaded to file ',File,'.~n'].
prolog:message(ckan_download_lod(Site)) -->
  ['CKAN site ',Site,'\'s LOD was downloaded.~n'].
prolog:message(ckan_list_sites([])) --> [].
prolog:message(ckan_list_sites([H|T])) -->
  ['  * ',H,'~n'],
  prolog:message(ckan_list_sites(T)).
prolog:message(ckan_unsupported_command(Command)) -->
  ['Command ',Command,' is not supported.~n'],
  commands.

commands -->
  ['The following commands are supported:~n'],
  {
    aggregate_all(
      set(Command),
      command(Command),
      Commands
    )
  },
  commands(Commands).

command(Command) -->
  ['  * ',Command,'~n'].

commands([]) --> [].
commands([H|T]) -->
  command(H),
  commands(T).

