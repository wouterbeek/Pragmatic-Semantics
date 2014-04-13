% Index for the Pragmatic Semantics repository.

index(PrasemDir):-
  assert_index(prasem, PrasemDir),
    assert_index(bk,     prasem(beekeeper)),
    assert_index(ckan,   prasem(ckan)),
    assert_index(dh,     prasem(data_hives)),
    assert_index(el  ,   prasem(energy_labels)),
    assert_index(humr,   prasem(humr)),
    assert_index(id,     prasem(ideaology)),
    assert_index(iotw,   prasem(iotw)),
    assert_index(lodobs, prasem(lod_observatory)),
    assert_index(su,     prasem(semantic_uris)),
    assert_index(stcn,   prasem(stcn)),
    assert_index(swag,   prasem(swag)),
    assert_index(webqr,  prasem(webqr)).

