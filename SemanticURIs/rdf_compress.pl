:- module(
  rdf_compress,
  [
    rdf_compress/3, % +FromDirectory:atom
                    % +ToDirectory:atom
                    % +ApStage:iri
    rdf_compress_rnd/3 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % +ApStage:iri
  ]
).

/** <module> SemanticURIs compression

RDF compression for the Semantic URIs project.
To be used in Automated Process stages.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(generics(codes_ext)).
:- use_module(os(java_ext)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(su(su_stats)).



rdf_compress(FromDir, ToDir, ApStage):-
  % DEB
  ap_stage_resource(ApStage, Resource, _),
  (
    rdf_string(Resource, ckan:name, Name, _), !
  ;
    Name = noname
  ),
  Args = [Resource,Name],
  % Create segements in both the error and the output streams
  % for each set of arguments with which we execute the JAR.
  setup_call_cleanup(
    java_output_stream(Output),
    format(Output, '\n\n~w\n~w\n', Args),
    close(Output)
  ),
  setup_call_cleanup(
    java_error_stream(Error),
    format(Error, '\n\n~w\n~w\n', Args),
    close(Error)
  ),

  absolute_file_name(su('RDFmodel'), JAR, [access(read),file_type(jar)]),
  run_jar(JAR, [preprocess,file(FromDir),file(ToDir)]),
  run_jar(JAR, [compress,file(ToDir)]),

  absolute_file_name(
    stats,
    StatisticsFile,
    [access(read),file_type(json),relative_to(ToDir)]
  ),
  su_stats(ApStage, compression_stats, StatisticsFile).


rdf_compress_rnd(FromDir, ToDir, ApStage):-
  % Link to the literals and triples (structure) from the previous stage.
  absolute_file_name(
    lits,
    FromFileLiterals,
    [access(read),extensions([txt]),relative_to(FromDir)]
  ),
  absolute_file_name(
    triples,
    FromFileTriples,
    [access(read),extensions([dat]),relative_to(FromDir)]
  ),
  maplist(link_file(ToDir), [FromFileLiterals,FromFileTriples]),

  % Generate random IRIs.
  absolute_file_name(
    uris,
    FromFileURIs,
    [access(read),extensions([txt]),relative_to(FromDir)]
  ),
  file_lines(FromFileURIs, Lines),
  file_alternative(FromFileURIs, ToDir, _, _, ToFileURIs),
  setup_call_cleanup(
    open(ToFileURIs, write, Stream, []),
    forall(
      between(1, Lines, _),
      put_random_iri(Stream)
    ),
    close(Stream)
  ),

  % Perform the compression using the random IRIs.
  absolute_file_name(su('RDFmodel'), JAR, [access(read),file_type(jar)]),
  run_jar(JAR, [compress,file(ToDir)]),

  absolute_file_name(
    stats,
    StatisticsFile,
    [access(read),file_type(json),relative_to(ToDir)]
  ),
  su_stats(ApStage, rnd_compression_stats, StatisticsFile).


put_random_iri(Stream):-
  put_codes(Stream, `http://`),
  forall(
    between(1, 15, _),
    (
      random_between(97, 122, Code),
      put_code(Stream, Code)
    )
  ),
  put_code(Stream, 10). %LF

