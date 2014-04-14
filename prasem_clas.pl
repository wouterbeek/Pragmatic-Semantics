:- use_module(pl(pl_clas)).

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

user:process_option(project(Project)):-
  var(Project), !,
  print_message(warning, no_project),
  halt.
user:process_option(project(Project)):-
  project(Project), !,

  % Execute the project load file.
  load_project(Project),
  load_clas(Project),

  % Execute the project debug file, if any.
  debug_project(Project).
user:process_option(project(Project)):-
  print_message(warning, unknown_project(Project)),
  halt.


prolog:message(no_project) -->
  ['No project specified.~n'],
  projects.
prolog:message(unknown_project(Project)) -->
  ['No project named ', Project, '~n'],
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
  ['  * ',H,'~n'],
  projects(T).


%! project(+ProjectName:atom) is semidet.
%! project(-ProjectName:atom) is nondet.
% Enumeration of supported PraSem projects.

project(beekeeper).
project(ckan).
project(data_hives).
project(energy_labels).
project(humr).
project(ideaology).
project(iotw).
project(lodobs).
project(semantic_uris).
project(stcn).
project(swag).
project(webqr).


load_clas(Project):-
  atomic_list_concat([Project,clas], '_', Base),
  file_project(Project, Base).


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

