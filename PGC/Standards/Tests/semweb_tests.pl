:- module(
  semweb_tests,
  [
    run_semweb_tests/0
  ]
).

/** <module> RDF(S) tests

# MANIFEST HEADER

~~~{.xml}
<?xml version="1.0"?>
<rdf:RDF
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  xmlns:test="http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#"
>
~~~

# MANIFEST FOOTER

~~~{.xml}
</rdf:RDF>
~~~

# Test types

## Miscellaneous Tests

This manifest entry is used to describe test cases that do not fall into
one of the earlier categories. It may have several associated files,
indicated in <test:document> elements.

## Negative Entailment Tests

These tests are specified using a similar structure to the Positive
Entailment Tests. The test is failed if the conclusion can be drawn from
the premises using the rules of RDF- or RDFS-entailment. The test is
considered to be passed when it can be conclusively demonstrated that the
conclusion cannot be so drawn. In practice, the test may be considered to
be passed when a thorough attempt to fail the test is unable to achieve
failure.

## Negative Parser Tests

These tests consist of one input document. The document is not legal RDF/XML.
A parser is considered to pass the test if it correctly holds the input
document to be in error.

## Positive Entailment Tests

These tests are specified by one or more premise documents (in RDF/XML or
N-Triples) together with a single conclusion document.

## Positive Parser Tests

These tests consist of one (or more) input documents in RDF/XML as is
revised in [RDF-SYNTAX]. The expected result is defined using the N-Triples
syntax (Section 3). A parser is considered to pass the test if it produces
a graph equivalent to the graph described by the N-triples output document,
according to the definition of graph equivalence given in [RDF-CONCEPTS].
Where the input document(s) are legal RDF/XML, but a warning may be
generated, this is indicated in the test manifest.

# Q&A

Q: What is the intended use of rdf_db:rdf_global_object/2?

Q: What is the intended use of directive (public/1)?

Q: How should option =|base_uri(+URI)|= for =|rdf_load/2|= be used?
   When I add it RDF/XML files load 0 triples!

@author Wouter Beek
@version 2013/04, 2013/06-2013/07, 2014/03
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(test, 'http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#').

:- rdf_meta(run_test(r)).
:- rdf_meta(run_test0(r,-)).



load_manifest:-
  absolute_file_name(
    tests(testSchema),
    TestSchema,
    [access(read), file_type(rdf)]
  ),
  rdf_load([], test_schema, TestSchema),
  absolute_file_name(
    tests('Manifest'),
    TestFile,
    [access(read), file_type(rdf)]
  ),
  rdf_load([], test, TestFile).

run_semweb_tests:-
  load_manifest,
  forall(
    (
      rdf_member(
        TestClass,
        [
          test:'MiscellaneousTest',
          test:'NegativeEntailmentTest',
          test:'NegativeParserTest',
          test:'PositiveEntailmentTest',
          test:'PositiveParserTest'
        ]
      ),
      rdfs_individual_of(Test, TestClass)
    ),
    run_test(Test)
  ).

run_test(Test):-
  % Clean up the graphs that were used in the previous test.
  maplist(rdf_unload_graph, [conclusion,doc,in,out,premise]),

  rdfs_individual_of(Test, Class),
  rdf_global_id(_Namespace:LocalName, Class),
  uri_to_subdirectory_file_fragment(
    Test,
    TestDirectory,
    _TestFileName,
    TestFragment
  ),
  format(
    user_output,
    '<~w - ~w - ~w>\n',
    [LocalName, TestDirectory, TestFragment]
  ),

  % If available, print the description to the screen.
  (
    rdf_string(Test, test:description, Description1, _),
    strip_atom([' ','\n','\t'], Description1, Description2)
  ->
    format(user_output, 'Test description: ~w\n', [Description2])
  ;
    format(user_output, 'No description for test ~w.\n', [Test])
  ),

  % Run the actual test.
  run_test0(Test, Status),

  % Write the status to the terminal.
  member_default(Status-Color, ['FAIL'-red, 'PASS'-green], _Default-black),
  ansi_format([bold, fg(Color)], 'STATUS: ~w\n\n', [Status]).

run_test0(Test, Status):-
  once(rdf_string(Test, test:status, Status, _)),
  memberchk(Status, ['NOT_APPROVED','OBSOLETE','OBSOLETED','WITHDRAWN']), !.
% A miscellaneous test with a single document.
% These tests are required to throw an exception upon attemption to
% load the 'RDF graph'.
run_test0(Test, 'PASS'):-
  (
    rdfs_individual_of(Test, test:'MiscellaneousTest')
  ;
    rdfs_individual_of(Test, test:'NegativeParserTest')
  ),

  % Load the document.
  (
    rdf(Test, test:document, URI)
  ;
    rdf(Test, test:inputDocument, URI)
  ),
  download_to_file(URI, File),

  % Throw an exception or fail, please.
  catch(
    \+ rdf_load([], doc, File),
    _Exception,
    true
  ),
  !.
run_test0(Test, 'SKIPPED'):-
  rdfs_individual_of(Test, test:'NegativeEntailmentTest'), !.
/*
  % The premise graph.
  rdf(Test, test:premiseDocument, Premise_URI),
  download_to_file(Premise_URI, Premise_File),
  %%%%rdf_load([base_uri(Premise_URI)], in, Premise_File),
  rdf_load([], premise, Premise_File),
*/
% A test with an input and an output document.
run_test0(Test, 'PASS'):-
  rdfs_individual_of(Test, test:'PositiveEntailmentTest'),

  % The premise graph.
  rdf(Test, test:premiseDocument, Premise_URI),
  download_to_file(Premise_URI, Premise_File),
  rdf_load([base_uri(Premise_URI)], premise, Premise_File),

  % Run materialization.
  rdf_materialize(premise),

  % The conclusion graph.
  % This is loaded after materialization (which cannot be restricted to
  % a graph).
  rdf(Test, test:conclusionDocument, Conclusion_URI),
  (
    rdf_is_bnode(Conclusion_URI)
  ->
    % The premise graph must be inconsistent.
    rdfs_inconsistent(premise)
  ;
    download_to_file(Conclusion_URI, Conclusion_File),
    rdf_load([base_uri(Conclusion_URI)], conclusion, Conclusion_File),
    % The materialized premise graph must be equivalent to the
    % conclusion graph.
    rdf_graph_equivalence(premise, conclusion)
  ),
  !.
run_test0(Test, 'PASS'):-
  rdfs_individual_of(Test, test:'PositiveParserTest'),

  % The input graph.
  rdf(Test, test:inputDocument, Input_URI),
  download_to_file(Input_URI, Input_File),
  %%%%rdf_load([base_uri(Input_URI)], in, Input_File),
  rdf_load([], in, Input_File),

  % The output graph.
  rdf(Test, test:outputDocument, Output_URI),
  download_to_file(Output_URI, Output_File),
  %%%%rdf_load([base_uri(Output_URI)], out, Output_File),
  rdf_load([], out, Output_File),

  % The graphs must be equivalent.
  rdf_graph_equivalence(in, out), !.
run_test0(_Test, 'FAIL').

%! download_to_file(+URI:uri, -File:atom) is det.
% Returns the atomic name of the locally stored file that the given URI
% refers to.
% This assumes that the test files are stored locally.

download_to_file(URI, File):-
  uri_to_subdirectory_file_fragment(URI, Directory, FileName, _Fragment),
  absolute_file_name(
    tests(Directory),
    SubDirectory,
    [access(read), file_type(directory)]
  ),
  absolute_file_name(
    FileName,
    File,
    [access(read), relative_to(SubDirectory)]
  ).

%! uri_to_subdirectory_file_fragment(
%!   +URI:uri,
%!   -Directory:atom,
%!   -FileName:atom,
%!   -Fragment:atom
%! ) is det.

uri_to_subdirectory_file_fragment(URI, Directory, FileName, Fragment):-
  uri_components(
    URI,
    uri_components(http, 'www.w3.org', URI_Path, _Search, Fragment)
  ),
  atomic_list_concat(List, '/', URI_Path), % split
  nth0_minus(0, List, FileName),
  nth0_minus(1, List, Directory).

