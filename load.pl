% The load file for the PraSem project.

:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(os(run_ext)).

:- multifile(user:project_name/1).
user:project_name('PraSem').

:- initialization(load_prasem).



load_prasem:-
  % PraSem
  use_module(prasem(prasem)),
  
  current_prolog_flag(argv, Argv),
  include(prasem_subproject_match, Argv, Subprojects),
  (
    Subprojects == []
  ->
    setoff(
      Name,
      prasem_subproject(Name),
      Names
    ),
    forall(
      nth1(I, Names, Name),
      format(user_output, '~:d\t~w\n', [I,Name])
    ),
    halt
  ;
    maplist(load_project, Subprojects)
  ),
  
  % Load the various projects.

  % Enumerate the external program support
  % for the currently loaded modules.
  list_external_programs,
  
  % Allow the Hoax to be loaded using `swipl -s debug.pl LODObs -g sw_hoax`
  use_module(prasem(sw_hoax)).

load_project(Project):-
  absolute_file_name(
    prasem(Project),
    Dir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    load,
    File,
    [access(read),file_type(prolog),relative_to(Dir)]
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

