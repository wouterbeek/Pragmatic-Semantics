:- module(
  prasem_clas,
  [
    prasem_process_options/0
  ]
).

/** <module> PraSem command-line arguments

Command-line argument handling for the PraSem project collection.

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(option)).

:- use_remote_module(pl(pl_clas)).

:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).



% Option: project.

user:option_specification([
  help('Load a PraSem subproject.'),
  longflags([project]),
  opt(project),
  shortflags([p]),
  type(atom)
]).

% Load a specific PraSem project.
cmd_prasem_project(O1):-
  option(project(Project), O1),
  nonvar(Project),
  prasem_project(Project), !,
  option(debug(Debug), O1, false),
  load_prasem_project(Project, Debug).
% Unknown project name given.
cmd_prasem_project(O1):-
  option(project(Project), O1),
  nonvar(Project), !,
  print_message(information, unknown_prasem_project(Project)),
  halt.
% No project name given.
cmd_prasem_project(_):-
  print_message(information, no_prasem_project),
  halt.


%! debug_prasem_project(+Project:atom, +Debug:boolean) is det.
% Loads the debug tools for the given PraSem project,
% in case `Debug=true`.

debug_prasem_project(Project, true):- !,
  file_prasem_project(Project, debug).
debug_prasem_project(_, false).


%! file_prasem_project(+Project:atom, +File:atom) is det.
% Loads the given Prolog file
% within the direct context of the given PraSem project.

file_prasem_project(Project1, Base):-
  downcase_atom(Project1, Project2),
  absolute_file_name(
    prasem(Project2),
    Dir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    Base,
    File,
    [access(read),file_errors(fail),file_type(prolog),relative_to(Dir)]
  ),
  ensure_remote_loaded(File).


%! load_prasem_project(+Project:atom, +Debug:boolean) is det.
% Loads the project with the given name.
%
% @arg Project Registered with prasem_project/1.

load_prasem_project(Project, Debug):-
  file_prasem_project(Project, load),
  debug_prasem_project(Project, Debug).


%! prasem_project(+ProjectName:atom) is semidet.
%! prasem_project(-ProjectName:atom) is nondet.
% Enumeration of supported PraSem projects.

prasem_project('Beekeeper').
prasem_project('CKAN').
prasem_project('DataHives').
prasem_project('EnergyLabels').
prasem_project(humR).
prasem_project('IDEAology').
prasem_project('IOTW').
prasem_project('LODObs').
prasem_project('PGC').
prasem_project('SemanticURIs').
prasem_project('STCN').
prasem_project('SWAG').
prasem_project('WebQR').



%! prasem_process_options is det.
% Reads the command-line arguments and executes those
% that are common among the PGC-using projects,

prasem_process_options:-
  read_options(O1), !,
  cmd_prasem_project(O1).



% Messages.

prolog:message(unknown_prasem_project(Project)) -->
  [Project,' is not a known PraSem project.~n'],
  prasem_projects.

prolog:message(no_prasem_project) -->
  ['No PraSem project was specified.~n'],
  prasem_projects.

prasem_projects -->
  ['The following PraSem projects are supported:~n'],
  {
    aggregate_all(
      set(Project),
      prasem_project(Project),
      Projects
    )
  },
  prasem_projects(Projects).

prasem_projects([]) --> [].
prasem_projects([H|T]) -->
  ['  * ',H,'~n'],
  prasem_projects(T).

