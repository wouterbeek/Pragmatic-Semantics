% Index for the Pragmatic Semantics repository.
% The file search path `prasem` must be set for this.

:- initialization(init_file_search_path).

init_file_search_path:-
  forall(
    project(Alias, Name),
    assert(user:file_search_path(Alias, prasem(Name)))
  ).


%! project(+Alias:atom, +Name:atom) is semidet.
%! project(+Alias:atom, -Name:atom) is det.
%! project(-Alias:atom, +Name:atom) is det.
%! project(-Alias:atom, -Name:atom) is nondet.
% The supported PraSem projects.

project(iaaa,   'IAAA').
project(bk,     'Beekeeper').
project(ckan,   'CKAN-2-RDF').
project(dh,     'DataHives').
project(el,     'EnergyLabels').
project(humr,   humR).
project(id,     'IDEAology').
project(iotw,   'IOTW').
project(lodobs, 'LODObs').
project(plc,    'PLC').
project(semuri, 'SemanticURIs').
project(stcn,   'STCN').
project(swag,   'SWAG').
project(webqr,  'WebQR').



debug_project(Name):-
  % Only in debug mode.
  predicate_property(user:debug_mode, visible),
  project(Alias, Name),
  Spec =.. [Alias,debug],
  absolute_file_name(
    Spec,
    _,
    [access(read),file_errors(fail),file_type(prolog)]
  ), !,
  ensure_loaded(Spec).
% Never fail, e.g. when not in debug mode or when there is no debug file.
debug_project(_).


%! load_clas(Project):-
% Load the command-line arguments support
% for the PraSem project with the given name.

load_clas(Name):-
  project(Alias, Name),
  Spec =.. [Alias,clas],
  ensure_loaded(Spec).


%! load_project(+Name:atom) is det.
% Loads the PraSem project with the given name.

load_project(Name):-
  project(Alias, Name),
  Spec =.. [Alias,load],
  ensure_loaded(Spec).

