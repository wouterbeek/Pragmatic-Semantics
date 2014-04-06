% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.

:- set_prolog_flag(encoding, utf8).

:- initialization(load_pgc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_pgc:-
  use_module(library(apply)),
  use_module(library(prolog_pack)),

  source_file(load_pgc, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(pgc, ThisDirectory)),

  % If there is no outer project, then PGC is the project.
  once((
    user:file_search_path(pgc, _)
  ;
    assert(user:file_search_path(pgc, ThisDirectory))
  )),

  set_project,

  assert(user:prolog_file_type(html, 'text/html')),
  assert(user:prolog_file_type(md,   'text/markdown')),
  assert(user:prolog_file_type(txt,  'text/plain')),

  % Assert the various search paths.
  assert(user:file_search_path(ap,              pgc('AP'      ))),
  assert(user:file_search_path(datasets,        pgc('Datasets'))),
    assert(user:file_search_path(dbpedia,         datasets('DBpedia'))),
  assert(user:file_search_path(dcg,             pgc('DCG'))),
    assert(user:file_search_path(flp,             dcg('Formal Languages'     ))),
    assert(user:file_search_path(plp,             dcg('Programming Languages'))),
    assert(user:file_search_path(nlp,             dcg('NLP'                  ))),
  assert(user:file_search_path(generics,        pgc('Generics'    ))),
  assert(user:file_search_path(graph_theory,    pgc('Graph Theory'))),
    assert(user:file_search_path(dgraph,          graph_theory('DGRAPH'   ))),
    assert(user:file_search_path(rdf_graph,       graph_theory('RDF Graph'))),
    assert(user:file_search_path(ugraph,          graph_theory('UGRAPH'   ))),
  assert(user:file_search_path(ilp,             pgc('ILP'))),
  assert(user:file_search_path(lod,           pgc('LOD'))),
    assert(user:file_search_path(owl,             lod('OWL' ))),
    assert(user:file_search_path(rdf,             lod('RDF' ))),
      assert(user:file_search_path(rdf_conv,        rdf('Conversion'))),
      assert(user:file_search_path(rdf_file,        rdf('File'))),
      assert(user:file_search_path(rdf_man,         rdf('Management'))),
      assert(user:file_search_path(rdf_mt,          rdf('RDF MT'    ))),
      assert(user:file_search_path(rdf_reasoning,   rdf('Reasoning' ))),
      assert(user:file_search_path(rdf_term,        rdf('Term'      ))),
      assert(user:file_search_path(rdf_web,         rdf('Web'       ))),
    assert(user:file_search_path(rdfs,            lod('RDFS'))),
    assert(user:file_search_path(skos,            lod('SKOS'))),
    assert(user:file_search_path(sparql,        lod('SPARQL'))),
    assert(user:file_search_path(void,            lod('VoID'))),
  assert(user:file_search_path(logic,           pgc('Logic'      ))),
  assert(user:file_search_path(math,            pgc('Math'       ))),
  assert(user:file_search_path(os,              pgc('OS'         ))),
  assert(user:file_search_path(programming,     pgc('Programming'))),
    assert(user:file_search_path(pl,              programming('Prolog'))),
      assert(user:file_search_path(pl_web,        pl('Web'))),
  assert(user:file_search_path(ps,              pgc('PS'         ))),
    assert(user:file_search_path(tms,             ps('TMS'))),
      assert(user:file_search_path(atms,            tms('ATMS' ))),
      assert(user:file_search_path(doyle,           tms('Doyle'))),
  assert(user:file_search_path(server,          pgc('Server'))),
  assert(user:file_search_path(standards,       pgc('Standards'))),
    assert(user:file_search_path(datetime,        standards('DateTime' ))),
    assert(user:file_search_path(geo,             standards('Geography'))),
    assert(user:file_search_path(gv,              standards('GraphViz' ))),
    assert(user:file_search_path(html,            standards('HTML'     ))),
    assert(user:file_search_path(http,            standards('HTTP'     ))),
      assert(user:file_search_path(http_headers,    http('Headers'   ))),
      assert(user:file_search_path(http_parameters, http('Parameters'))),
    assert(user:file_search_path(lang,            standards('Language'))),
    assert(user:file_search_path(latex,           standards('LaTeX'   ))),
    assert(user:file_search_path(svg,             standards('SVG'     ))),
    assert(user:file_search_path(tests,           standards('Tests'   ))),
    assert(user:file_search_path(uri,             standards('URI'     ))),
    assert(user:file_search_path(xml,             standards('XML'     ))),
      assert(user:file_search_path(xsd,             xml('XSD'))),
  assert(user:file_search_path(stat,            pgc('Stats'))),
  assert(user:file_search_path(web,             pgc('Web'  ))),
    assert(user:file_search_path(crawler,         web('Crawler'))),
  
  ensure_loaded(pgc(init)).


% If there is no outer project, then PGC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PGC', 'Prolog Generics Collection')).

