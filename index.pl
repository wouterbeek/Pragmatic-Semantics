% Index for the Pragmatic Semantics repository.

index(PrasemDir):-
  assert(user:file_search_path(prasem, PrasemDir)),
    assert(user:file_search_path(bk,     prasem(beekeeper      ))),
    assert(user:file_search_path(ckan,   prasem(ckan           ))),
    assert(user:file_search_path(dh,     prasem(data_hives     ))),
    assert(user:file_search_path(el  ,   prasem(energy_labels  ))),
    assert(user:file_search_path(humr,   prasem(humr           ))),
    assert(user:file_search_path(id,     prasem(ideaology      ))),
    assert(user:file_search_path(iotw,   prasem(iotw           ))),
    assert(user:file_search_path(lodobs, prasem(lod_observatory))),
    assert(user:file_search_path(su,     prasem(semantic_uris  ))),
    assert(user:file_search_path(stcn,   prasem(stcn           ))),
    assert(user:file_search_path(swag,   prasem(swag           ))),
    assert(user:file_search_path(webqr,  prasem(webqr          ))).

