% The run file for the PraSem project.

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(optparse2).

:- multifile(user:project/2).
user:project('PraSem', 'Pragmatic Semantics for the Web of Data').

:- meta_predicate(user:ensure_remote_loaded(:)).
:- meta_predicate(user:reexport_remote_module(:)).
:- meta_predicate(user:reexport_remote_module(+,:)).
:- meta_predicate(user:reexport_remote_module(+,:,+)).
:- meta_predicate(user:use_remote_module(:)).
:- meta_predicate(user:use_remote_module(+,:)).
:- meta_predicate(user:use_remote_module(+,:,+)).

:- multifile(prolog:message//1).

:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).

:- initialization(run_prasem).

run_prasem:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),
  
  % Load PraSem index.
  source_file(run_prasem, ThisFile),
  file_directory_name(ThisFile, PrasemDir),
  load_index(PrasemDir),
  
  % Load PLC index.
  directory_file_path(ThisDir, plc, PlcDir),
  load_index(PlcDir),
  
  % Load PraSem.
  use_module(prasem(prasem_clas)).


%! load_index(+ParentDir:atom) is det.
% Load the repository index located in the given parent directory.

load_index(ParentDir):-
  absolute_file_name(
    index,
    IndexFile,
    [access(read),file_type(prolog),relative_to(ParentDir)]
  ),
  setup_call_cleanup(
    ensure_loaded(IndexFile),
    index(ParentDir),
    unload_file(IndexFile)
  ).


% Do not load code remotely, but only locally.

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

