:- module(
  entropy_and_mutual_information,
  [
% READ
    entropy/4, % +Resource:iri
               % +Column:oneof([property_set,type_set,fingerprint,partition_cell])
               % +Row:oneof([property_set,type_set,fingerprint,partition_cell])
               % -Entropy:float
    mutual_information/4, % +Resource:iri
                          % +Column:oneof([property_set,type_set,fingerprint,partition_cell])
                          % +Row:oneof([property_set,type_set,fingerprint,partition_cell])
                          % -MutualInformation:float
    normalized_mutual_information/4, % +Resource:iri
                                     % +Column:oneof([property_set,type_set,fingerprint,partition_cell])
                                     % +Row:oneof([property_set,type_set,fingerprint,partition_cell])
                                     % -NormalizedMutualInformation:between(0.0,1.0)
% AP
    entropy_and_mutual_information/3 % +FromDirectory:atom
                                     % +ToDirectory:atom
                                     % +ApStage:iri
  ]
).

/** <module> SemanticURIs Entropy

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(dcg(dcg_content)).
:- use_module(generics(db_ext)).
:- use_module(generics(row_ext)).
:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_table)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_web(rdf_html_table)). % XML namespace.
:- use_module(rdfs(rdfs_build)).
:- use_module(server(web_modules)).

http:location(su, root(su), []).
:- http_handler(su(emi), su_entropy_and_mutual_information, []).

user:web_module(
  'SemanticURIs Entropy & Mutual Information',
  su_entropy_and_mutual_information
).

:- db_add_novel(user:prolog_file_type(csv, csv)).
:- db_add_novel(user:prolog_file_type(perl, perl)).



su_entropy_and_mutual_information(_Request):-
  reply_html_page(
    app_style,
    title('Semantic URIs - Entropy & Mutual Information'),
    html(\entropy_order)
  ).


entropy_order -->
  {
    findall(
      Resource,
      (
        entropy(Resource, property_set, type_set, _),
        entropy(Resource, fingerprint, type_set, _)
      ),
      Resources
    ),
    partition(entropy_compare, Resources, Less, Equal, Greater),
    maplist(length, [Less,Equal,Greater], [L,E,G]),
    findall(
      [Resource,Triples],
      (
        member(Resource, Equal),
        rdf_datatype(Resource, void:triples, Triples, xsd:integer, _)
      ),
      EqualRows
    ),
    findall(
      [Name,M,E1,E2,Delta,Triples],
      (
        member(Resource, Greater),
        entropy(Resource, property_set, type_set, E1),
        entropy(Resource, fingerprint, type_set, E2),
        mutual_information(Resource, type_set, type_set, M),
        Delta is ((E1 - E2) / M) * 100,%) / M * 100,
        rdf(Resource, su:compression_stats, Stats),
        rdf_datatype(Stats,    su:typesets,     X1,      xsd:integer, _),
        rdf_datatype(Stats,    su:propsets,     X2,      xsd:integer, _),
        rdf_datatype(Stats,    su:fingerprints, X3,      xsd:integer, _),
        rdf_datatype(Resource, void:triples,    Triples, xsd:integer, _),
        resource_name(Resource, Name)
      ),
      GreaterRows
    ),
    findall(
      [Resource,Name,E1,E2,Delta,M,X1,X2,X3,Triples,M0,M1,M2,M3],
      (
        member(Resource, Resources),
        entropy(Resource, property_set, type_set, E1),
        entropy(Resource, fingerprint, type_set, E2),
        mutual_information(Resource, type_set, type_set, M),
        Delta is E1 - E2,
        rdf(Resource, su:compression_stats, Stats),
        rdf_datatype(Stats,    su:typesets,     X1,      xsd:integer, _),
        rdf_datatype(Stats,    su:propsets,     X2,      xsd:integer, _),
        rdf_datatype(Stats,    su:fingerprints, X3,      xsd:integer, _),
        rdf_datatype(Resource, void:triples,    Triples, xsd:integer, _),
        Triples > 10000,
        mutual_information(Resource, property_set, property_set, M0),
        mutual_information(Resource, property_set, type_set,     M1),
        mutual_information(Resource, fingerprint,  type_set,     M2a),
        M2 is M2a / M * 100,
        mutual_information(Resource, fingerprint,  property_set, M3),
        resource_name(Resource, Name)
      ),
      AllRows
    ),
    findall(
      [Name,X1,X2,X3,M2,Triples],
      (
        member(Resource, Resources),
        rdf(Resource, su:compression_stats, Stats),
        rdf_datatype(Stats,    su:typesets,     X1,      xsd:integer, _),
        rdf_datatype(Stats,    su:propsets,     X2,      xsd:integer, _),
        rdf_datatype(Stats,    su:fingerprints, X3,      xsd:integer, _),
        rdf_datatype(Resource, void:triples,    Triples, xsd:integer, _),
        Triples > 10000,
        mutual_information(Resource, type_set, type_set, M),
        mutual_information(Resource, fingerprint,  type_set,     M2a),
        M2 is M2a / M * 100,
        resource_name(Resource, Name)
      ),
      Rows1
    )
  },
  html([
    \html_table(
      [header_row(true)],
      html('H(type_set|property_set) > H(type_set | fingerprint)'),
      [['Less','Equal','Greater'],[L,E,G]]
    ),
    \rdf_html_table(
      [header_row(true)],
      html('Enumeration of smaller.'),
      [['Resource','Number of triples']|EqualRows]
    ),
    \rdf_html_table(
      [header_row(true)],
      html('Enumeration of greater.'),
      [[
        'Resource',
        'H(type_set)',
        'H(type_set | property_set)',
        'H(type_set | fingerprint)',
        'Delta',
        %'Number of typesets',
        %'Number of propertysets',
        %'Number of fingerprints',
        'Number of Triples'
      ] | GreaterRows]
    ),
    \rdf_html_table(
      [header_row(true)],
      html('Enumeration of all.'),
      [[
        'Resource link',
        'Resource name',
        'H(type_set | property_set)',
        'H(type_set | fingerprint)',
        'Delta',
        'H(type_set)',
        'Number of typesets',
        'Number of propertysets',
        'Number of fingerprints',
        'Number of Triples',
        'H(propertyset)',
        'I(propertyset : typeset)',
        'I(fingerprint : typeset)',
        'I(fingerprint : propertyset)'
      ] | AllRows]
    ),
    \rdf_html_table(
      [header_row(true)],
      html('Enumeration of all.'),
      [[
        'Resource name',
        'Number of typesets',
        'Number of propertysets',
        'Number of fingerprints',
        'Number of Triples'
      ] | Rows1]
    )
  ]).
resource_name(Resource, Name):-
  rdf(Package, ckan:resources, Resource),
  rdf_string(Package, ckan:name, Name, _), !.
resource_name(_, dummy).

entropy_compare(Resource, Order):-
  entropy(Resource, property_set, type_set, E1),
  entropy(Resource, fingerprint, type_set, E2),
  compare(Order, E1, E2).


html_mi -->
  {L = [property_set,type_set,fingerprint,partition_cell]},
  html_mi1(L, L).

html_mi1([], _) --> !, [].
html_mi1([H|T], L) -->
  html_mi2(H, L),
  html_mi1(T, L).

html_mi2(_, []) --> !, [].
html_mi2(Column, [Row|T]) -->
  {
    findall(
      N-Resource,
      normalized_mutual_information(Resource, Column, Row, N),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [N,M,Resource,URL],
      (
        member(N-Resource, Pairs3),
        rdf_string(Resource, ckan:url, URL, _),
        mutual_information(Resource, Column, Row, M)
      ),
      Rows
    )
  },
  html(
    \rdf_html_table(
      [header_row(true),indexed(true)],
      html([
        'Overview of Mutual Information between ',
        tt(Column),
        ' and ',
        tt(Row),
        '.'
      ]),
      [['Normalized Mutual Information','Mutual Information','Resource','URL']
       |Rows]
    )
  ),
  html_mi2(Column, T).


%! entropy(
%!   +Resource:iri,
%!   +Column:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   +Row:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   -Entropy:float
%! ) is det.

entropy(Resource, Column, Row, Entropy):-
  rdf(Resource, rdf_table:table, Table),
  rdfs_individual_of(Table, rdf_table:'Entropy'),
  rdfs_individual_of(Table, rdf_table:'URIsAndBNodes'),
  rdf(Table, rdf_table:cell, Cell),
  rdf_string(Cell, rdf_table:column, Column, _),
  rdf_string(Cell, rdf_table:row, Row, _),
  rdf_datatype(Cell, rdf:value, Entropy, xsd:float, _).


%! mutual_information(
%!   +Resource:iri,
%!   +Column:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   +Row:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   -MutualInformation:float
%! ) is det.

mutual_information(Resource, Column, Row, MutualInformation):-
  rdf(Resource, rdf_table:table, Table),
  rdfs_individual_of(Table, rdf_table:'MutualInformation'),
  rdfs_individual_of(Table, rdf_table:'URIsAndBNodes'),
  rdf(Table, rdf_table:cell, Cell),
  rdf_string(Cell, rdf_table:column, Column, _),
  rdf_string(Cell, rdf_table:row, Row, _),
  rdf_datatype(Cell, rdf:value, MutualInformation, xsd:float, _).


%! normalized_mutual_information(
%!   +Resource:iri,
%!   +Column:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   +Row:oneof([property_set,type_set,fingerprint,partition_cell]),
%!   -NormalizedMutualInformation:between(0.0,1.0)
%! ) is det.

normalized_mutual_information(
  Resource,
  Column,
  Row,
  NormalizedMutualInformation
):-
  mutual_information(Resource, Column, Column, SelfInformation),
  mutual_information(Resource, Column, Row, MutualInformation),
  div_zero(MutualInformation, SelfInformation, NormalizedMutualInformation).


entropy_and_mutual_information(FromDir, _, ApStage):-
  absolute_file_name(
    terms,
    File,
    [access(read),file_type(csv),relative_to(FromDir)]
  ),
  absolute_file_name(
    su(entropies_csv),
    Script,
    [access(read),file_type(perl)]
  ),
  process_create(Script, [file(File)], [stdout(pipe(Out))]),
  read_stream_to_codes(Out, Codes),
  phrase(csv(Rows), Codes),
  categorize_rows([uri,bnode], Rows, UriRows, BNodeRows, BothRows),
  ap_stage_resource(ApStage, Resource, Graph),
  forall(
    member(
      TableClassName,
      ['URIs','BNodes','URIsAndBNodes','Entropy','MutualInformation']
    ),
    (
      rdf_global_id(rdf_table:TableClassName, TableClass),
      rdfs_assert_subclass(TableClass, rdf_table:'Table', Graph)
    )
  ),
  maplist(
    tables(Graph),
    [UriRows,BNodeRows,BothRows],
    ['URIs','BNodes','URIsAndBNodes'],
    [URITable1,BNodeTable1,BothTable1],
    [URITable2,BNodeTable2,BothTable2]
  ),
  forall(
    member(
      Table,
      [URITable1,BNodeTable1,BothTable1,URITable2,BNodeTable2,BothTable2]
    ),
    (
      rdf_assert(Resource, rdf_table:table, Table, Graph),
      add_table(ApStage, Table)
    )
  ).


tables(Graph, Rows, Caption1, EntropyTable, MutualInformationTable):-
  categorize_rows(
    [entropy_and_conditional_entropy],
    Rows,
    EntropyRows,
    MutualInformationRows
  ),
  maplist(
    assert_table(Graph, Caption1),
    ['Entropy','MutualInformation'],
    [EntropyRows,MutualInformationRows],
    [EntropyTable,MutualInformationTable]
  ).


assert_table(Graph, Caption1, Caption2, Rows1, Table):-
  nth0_column(Rows1, 0, Names, Rows2),
  
  % Create the table.
  atomic_list_concat([Caption1,Caption2], ' - ', Caption),
  rdf_assert_table(Graph, Caption, Names, Names, Rows2, Table),
  
  % The table belongs to two classes.
  rdf_global_id(rdf_table:Caption1, Class1),
  rdf_assert_individual(Table, Class1, Graph),
  rdf_global_id(rdf_table:Caption2, Class2),
  rdf_assert_individual(Table, Class2, Graph).

