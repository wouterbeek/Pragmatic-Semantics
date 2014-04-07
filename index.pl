% Index for the Pragmatic Semantics repository.

pl_repository_index(Dir):-
  % Importing project directory.
  assert(user:file_search_path(project, Dir)),
  
  assert(user:file_search_path(bk,     project(beekeeper      ))),
  assert(user:file_search_path(ckan,   project(ckan           ))),
  assert(user:file_search_path(dh,     project(data_hives     ))),
  assert(user:file_search_path(el  ,   project(energy_labels  ))),
  assert(user:file_search_path(humr,   project(humr           ))),
  assert(user:file_search_path(id,     project(ideaology      ))),
  assert(user:file_search_path(iotw,   project(iotw           ))),
  assert(user:file_search_path(lodobs, project(lod_observatory))),
  assert(user:file_search_path(su,     project(semantic_uris  ))),
  assert(user:file_search_path(stcn,   project(stcn           ))),
  assert(user:file_search_path(swag,   project(swag           ))),
  assert(user:file_search_path(webqr,  project(webqr          ))).

