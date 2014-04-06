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
  assert(user:file_search_path(ap,              pgc(ap))),
  assert(user:file_search_path(datasets,        pgc(datasets))),
    assert(user:file_search_path(dbpedia,         datasets(dbpedia))),
  assert(user:file_search_path(dcg,             pgc(dcg))),
    assert(user:file_search_path(flp,             dcg(flp))),
    assert(user:file_search_path(nlp,             dcg(nlp))),
  assert(user:file_search_path(generics,        pgc(generics))),
  assert(user:file_search_path(graph_theory,    pgc(graph_theory))),
    assert(user:file_search_path(dgraph,          graph_theory(dgraph))),
    assert(user:file_search_path(rdf_graph,       graph_theory(rdf_graph))),
    assert(user:file_search_path(ugraph,          graph_theory(ugraph))),
  assert(user:file_search_path(ilp,             pgc(ilp))),
  assert(user:file_search_path(lod,             pgc(lod))),
    assert(user:file_search_path(owl,             lod(owl))),
    assert(user:file_search_path(rdf,             lod(rdf))),
      assert(user:file_search_path(rdf_conv,        rdf(conversion))),
      assert(user:file_search_path(rdf_file,        rdf(file))),
      assert(user:file_search_path(rdf_man,         rdf(management))),
      assert(user:file_search_path(rdf_mt,          rdf(rdf_mt))),
      assert(user:file_search_path(rdf_reasoning,   rdf(reasoning))),
      assert(user:file_search_path(rdf_term,        rdf(term))),
      assert(user:file_search_path(rdf_web,         rdf(web))),
    assert(user:file_search_path(rdfs,            lod(rdfs))),
    assert(user:file_search_path(skos,            lod(skos))),
    assert(user:file_search_path(sparql,          lod(sparql))),
    assert(user:file_search_path(void,            lod(void))),
  assert(user:file_search_path(logic,           pgc(logic))),
  assert(user:file_search_path(math,            pgc(math))),
  assert(user:file_search_path(os,              pgc(os))),
  assert(user:file_search_path(programming,     pgc(programming))),
    assert(user:file_search_path(pl,              programming(prolog))),
      assert(user:file_search_path(pl_web,        pl(web))),
  assert(user:file_search_path(ps,              pgc(ps))),
    assert(user:file_search_path(tms,             ps(tms))),
      assert(user:file_search_path(atms,            tms(atms))),
      assert(user:file_search_path(doyle,           tms(doyle))),
  assert(user:file_search_path(server,          pgc(server))),
  assert(user:file_search_path(standards,       pgc(standards))),
    assert(user:file_search_path(datetime,        standards(date_time))),
    assert(user:file_search_path(geo,             standards(geography))),
    assert(user:file_search_path(gv,              standards(graphviz))),
    assert(user:file_search_path(html,            standards(html))),
    assert(user:file_search_path(http,            standards(http))),
      assert(user:file_search_path(http_headers,    http(header))),
      assert(user:file_search_path(http_parameters, http(parameters))),
    assert(user:file_search_path(lang,            standards(language))),
    assert(user:file_search_path(latex,           standards(latex))),
    assert(user:file_search_path(svg,             standards(svg))),
    assert(user:file_search_path(uri,             standards(uri))),
    assert(user:file_search_path(xml,             standards(xml))),
      assert(user:file_search_path(xsd,             xml(xsd))),
  assert(user:file_search_path(web,             pgc(web))),
    assert(user:file_search_path(crawler,         web(crawler))),
  
  ensure_loaded(pgc(init)).


% If there is no outer project, then PGC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PGC', 'Prolog Generics Collection')).

