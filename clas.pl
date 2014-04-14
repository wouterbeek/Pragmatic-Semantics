:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).

:- discontiguous(user:process_option/1).
:- multifile(user:process_option/1).

:- multifile(prolog:message//1).



% Option: project.

user:option_specification([
  help('Load a PraSem subproject.'),
  longflags([project]),
  opt(project),
  shortflags([p]),
  type(atom)
]).

user:process_option(project(Name)):-
  project(Name), !,

  % Execute the project load file.
  load_project(Name),

  % Execute the project debug file, if any.
  debug_project(Name).
user:process_option(project(Name)):-
  print_message(warning, unknown_project(Name)),
  halt.


prolog:message(unknown_project(Name)) -->
  ['No project named ', Name, '~n'],
  projects.

projects -->
  {
    aggregate_all(
      set(Project),
      project(Project),
      Projects
    )
  },
  ['The following projects are supported:~n'],
  projects(Projects).

projects([]) --> !, [].
projects([H|T]) -->
  [H,'~n'],
  projects(T).


%! project(+ProjectName:atom) is semidet.
%! project(-ProjectName:atom) is nondet.
% Enumeration of supported PraSem projects.

project('Beekeeper').
project('CKAN').
project('DataHives').
project('EnergyLabels').
project(humR).
project('IDEAology').
project('IOTW').
project('LODObs').
project('PGC').
project('SemanticURIs').
project('STCN').
project('SWAG').
project('WebQR').


%! load_project(+Project:atom) is det.
% Loads the project with the given name.
%
% @arg Project Registered with project/1.

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

