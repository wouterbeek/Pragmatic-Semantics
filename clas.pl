:- use_module(library(aggregate)).

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
  var(Name), !,
  print_message(warning, no_project),
  halt.
user:process_option(project(Name)):-
  project(_, Name), !,
  load_project(Name).
user:process_option(project(Name)):-
  print_message(warning, unknown_project(Name)),
  halt.


prolog:message(no_project) -->
  ['No project specified.~n'],
  projects.
prolog:message(unknown_project(Name)) -->
  ['No project named ', Name, '~n'],
  projects.

projects -->
  {
    aggregate_all(
      set(Name),
      project(_, Name),
      Names
    )
  },
  ['The following projects are supported:~n'],
  projects(Names).

projects([]) --> !, [].
projects([H|T]) -->
  ['  * ',H,'~n'],
  projects(T).

