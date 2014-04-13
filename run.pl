% The run file for the PraSem project.

:- set_prolog_flag(encoding, utf8).

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

:- use_module(optparse2).

:- meta_predicate(user:ensure_remote_loaded(:)).
:- meta_predicate(user:reexport_remote_module(:)).
:- meta_predicate(user:reexport_remote_module(+,:)).
:- meta_predicate(user:reexport_remote_module(+,:,+)).
:- meta_predicate(user:use_remote_module(:)).
:- meta_predicate(user:use_remote_module(+,:)).
:- meta_predicate(user:use_remote_module(+,:,+)).

:- multifile(user:project/2).
user:project('PraSem', 'Pragmatic Semantics for the Web of Data').

:- multifile(prolog:message//1).

:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).

:- initialization(run_prasem).



% Option: project.

user:option_specification([
  help('Load a PraSem subproject.'),
  longflags([project]),
  opt(project),
  shortflags([p]),
  type(atom)
]).

user:process_cmd_option(project(Name)):-
  prasem_subproject(Name), !,
  
  % Execute the project load file.
  load_project(Name),
  
  % Execute the project debug file, if any.
  debug_project(Name).
user:process_cmd_option(project(Name)):-
  print_message(warning, unknown_project(Name)).

user:process_cmd_options(O1):-
  (
    memberchk(project(_), O1)
  ->
    true
  ;
    print_message(information, no_project)
  ).

prolog:message(no_project) -->
  ['No PraSem project selected.~n'],
  {
    findall(
      Name,
      prasem_subproject(Name),
      Names1
    ),
    sort(Names1, Names2)
  },
  ['The following projects are supported:~n'],
  list_projects(Names2).
prolog:message(unknown_project(Name)) -->
  ['No project named ', Name, '~n'].


list_projects([]) --> !, [].
list_projects([H|T]) -->
  [H,'~n'],
  list_projects(T).



run_prasem:-
  % Load the index.
  source_file(run_prasem, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  ensure_loaded(index),
  index(ThisDir)m
  
  % PraSem
  use_remote_module(prasem(prasem)),
  
  % Enumerate the external program support
  % for the currently loaded modules.
  use_remote_module(os(run_ext)),
  list_external_programs.


load_or_debug(Project):-
  predicate_property(user:debug_mode, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).


load_pgc(_Project):-
  user:file_search_path(plc, _Spec), !.
load_pgc(Project):-
  Spec =.. [Project,'PGC'],
  assert(user:file_search_path(plc, Spec)),
  load_or_debug(plc).


%! load_project(+Project:atom) is det.
% Loads the project with the given name.
%
% @arg Project Registered with prasem_subproject/1.

load_project(Project):-
  file_project(Project, load).

debug_project(Project):-
  % Only in debug mode.
  predicate_property(user:debug_mode, visible),
  file_project(Project, debug).
% Never fail, e.g. when not in debug mode or when there is no debug file.
debug_project(_).

file_project(Project, FileName):-
  absolute_file_name(
    prasem(Project),
    Dir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    FileName,
    File,
    [access(read),file_errors(fail),file_type(prolog),relative_to(Dir)]
  ),
  ensure_loaded(File).


%! prasem_subproject_match(Arg) is semidet.
% Succeeds if the given argument contains a PraSem project in its prefix.
%
% This allows the use of autocompletion in the terminal,
% since the project names correspond to the names of subdirectories.
% E.g. `SemanticURIs/` matches `SemanticURIs`.

prasem_subproject_match(Arg):-
  prasem_subproject(ProjectName),
  atom_prefix(Arg, ProjectName).


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


user:ensure_remote_loaded(CallingModule:CalledModuleSpec):-
  CallingModule:ensure_loaded(CalledModuleSpec).

user:reexport_remote_module(CallingModule:CalledModuleSpec):-
  CallingModule:reexport(CalledModuleSpec).

user:reexport_remote_module(_, ModuleSpec):-
  user:reexport_remote_module(ModuleSpec).

user:reexport_remote_module(_, CallingModule:CalledModuleSpec, Import):-
  CallingModule:reexport(CalledModuleSpec, Import).

user:use_remote_module(CallingModule:CalledModuleSpec):-
  CallingModule:use_module(CalledModuleSpec).

user:use_remote_module(_, ModuleSpec):-
  user:use_remote_module(ModuleSpec).

user:use_remote_module(_, CallingModule:CalledModuleSpec, ImportList):-
  CallingModule:use_module(CalledModuleSpec, ImportList).

