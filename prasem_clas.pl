:- module(
  prasem_clas,
  [
    process_options/0
  ]
).

/** <module> PraSem command-line arguments

Command-line argument handling for the PraSem project collection.

@author Wouter Beek
@version 2014/04
*/

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
  prasem_subproject(Name), !,
  option(debug(Debug), O1, false),
  load_prasem_project(Project, Debug).
% Unknown project name given.
cmd_prasem_project(_):-
  option(project(Project), O1),
  nonvar(Project), !,
  print_message(information, unknown_prasem_project(Project)).
% No project name given.
cmd_prasem_project(_):-
  print_message(information, no_prasem_project).


%! debug_prasem_project(+Project:atom, +Debug:boolean) is det.
% Loads the debug tools for the given PraSem project,
% in case `Debug=true`.

debug_prasem_project(Project, true):- !,
  file_prasem_project(Project, debug).
debug_prasem_project(Project, false).


%! file_prasem_project(+Project:atom, +File:atom) is det.
% Loads the given Prolog file
% within the direct context of the given PraSem project.

file_prasem_project(Project, Base):-
  absolute_file_name(
    prasem(Project),
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
% @arg Project Registered with prasem_subproject/1.

load_prasem_project(Project, Debug):-
  file_prasem_project(Project, load),
  debug_prasem_project(Project, Debug).


%! prasem_subproject(+ProjectName:atom) is semidet.
%! prasem_subproject(-ProjectName:atom) is nondet.
% Enumeration of supported PraSem projects.

prasem_subproject('Beekeeper').
prasem_subproject('CKAN').
prasem_subproject('DataHives').
prasem_subproject('EnergyLabels').
prasem_subproject(humR).
prasem_subproject('IDEAology').
prasem_subproject('IOTW').
prasem_subproject('LODObs').
prasem_subproject('PGC').
prasem_subproject('SemanticURIs').
prasem_subproject('STCN').
prasem_subproject('SWAG').
prasem_subproject('WebQR').



%! process_options is det.
% Reads the command-line arguments and executes those
% that are common among the PGC-using projects,

process_options:-
  read_options(O1), !,
  cmd_prasem_project(O1).

