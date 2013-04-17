:- module(
  db,
  [
% GENERIC
    db_web/1,
    load_db/0,
    store_db/0,

% READ
    author/1,
    author_class/1,
    birth_date/2,
    death_date/2,
    first_name/2, % ?Person:uri
                  % ?First:atom
    last_name/2, % ?Person:uri
                 % ?Last:atom
    person/1,
    person_class/1,

% WRITE
    add_author/2,
    add_birth_date/2,
    add_death_date/2,
    add_first_name/2,
    add_last_name/2,
    add_name/2,

% ASSERT
    assert_author/1 % +Name:atom
  ]
).

/** <module> DB

Test module for creating and maintaining a database.

@author Wouter Beek
@version 2012/09-2012/10, 2012/12, 2013/03
*/

:- use_module(datasets(dbpedia)).
:- use_module(generic(file_ext)).
:- use_module(generic(meta_ext)).
:- use_module(generic(os_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(sparql(sparql)).

:- rdf_register_prefix(db, 'http://www.wouterbeek.com/prasem/db.owl#', [keep(true)]).
:- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/', [keep(true)]).



% GENERIC %

db_web([
  element(h1, [], ['Persons:']),
  element(ul, [], ListElements)
]):-
  setoff(
    ListElement,
    person_web(ListElement),
    ListElements
  ).

load_db:-
  absolute_file_name(debug(.), Dir, [access(write), file_type(directory)]),
  date_directories(Dir, DB_Dir),
  format(atom(RE), '~w/*.rdf', [DB_Dir]),
  expand_file_name(RE, Files),
  latest_file(Files, File),
  !,
  rdf_load2(File, xml, db).
load_db.

person_web(element(li, [], [element(b, [], [String])])):-
  person(Person),
  first_name(Person, First),
  last_name(Person, Last),
  birth_date(Person, Birth),
  death_date(Person, Death),
  format(atom(String), '~w ~w (~w-~w)', [First, Last, Birth, Death]).

store_db:-
  absolute_file_name(debug(.), Dir, [access(write), file_type(directory)]),
  date_directories(Dir, DB_Dir),
  hash_date(FileName),
  create_file(DB_Dir, FileName, rdf, RDF_File),
  rdf_save2(db, xml, RDF_File).



% READ %

author(Person):-
  rdfs_individual_of(Person, db:author).

author_class(Class):-
  rdf_global_id(db:author, Class).

birth_date(Person, Birth):-
  nonvar(Person),
  !,
  birth_date0(Person, Birth),
  !.
birth_date(Person, Birth):-
  birth_date0(Person, Birth).

birth_date0(Person, Birth):-
  rdf_datatype(Person, db:birth_date, gYear, Birth, db).

death_date(Person, Death):-
  nonvar(Person),
  !,
  death_date0(Person, Death),
  !.
death_date(Person, Death):-
  death_date0(Person, Death).

death_date0(Person, Death):-
  rdf_datatype(Person, db:death_date, gYear, Death, db).

first_name(Person, First):-
  nonvar(Person),
  !,
  first_name0(Person, First),
  !.
first_name(Person, First):-
  first_name0(Person, First).

first_name0(Person, First):-
  rdf_literal(Person, foaf:givenname, First, db).

last_name(Person, Last):-
  nonvar(Person),
  !,
  last_name0(Person, Last),
  !.
last_name(Person, Last):-
  last_name0(Person, Last).

last_name0(Person, Last):-
  rdf_literal(Person, foaf:family_name, Last, db).

person(Person):-
  rdfs_individual_of(Person, foaf:'Person').

person_class(Class):-
  rdf_global_id(foaf:'Person', Class).



% WRITE %

add_author(Author, Name):-
  author_class(Authors),
  rdfs_assert_individual(Author, Authors, db),
  rdfs_assert_label(Author, Name, db).

%% add_death_date(+Person:uri, +Birth:integer) is det.
% Asserts that the given person was born in the given year.
%
% @param Person A person resource.
% @param Birth An integer representing a year.

add_birth_date(Person, Birth):-
  rdf_assert_datatype(Person, db:birth_date, gYear, Birth, db).

%% add_death_date(+Person:uri, +Death:integer) is det.
% Asserts that the given person died in the given year.
%
% @param Person A person resource.
% @param Death An integer representing a year.

add_death_date(Person, Death):-
  rdf_assert_datatype(Person, db:death_date, gYear, Death, db).

add_first_name(Author, First):-
  rdf_assert_literal(Author, foaf:givenname, First, db).

add_last_name(Author, Last):-
  rdf_assert_literal(Author, foaf:family_name, Last, db).

add_name(Author, Name):-
  rdf_assert_literal(Author, foaf:name, Name, db).



% ASSERT %

assert_author(Name):-
  format(atom(Filter), '  FILTER regex(?label, "~w", "i")', [Name]),
  formulate_sparql(
    [rdf, rdfs, umbel],
    'SELECT DISTINCT ?author',
    ['  ?author rdf:type umbel:Writer .',
     '  ?author rdfs:label ?label .',
     Filter],
    1,
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Results),
  Results = [row(Subject)],
  describe_resource(Subject, Rows),
  forall(
    member(row(Predicate, Object), Rows),
    rdf_assert(Subject, Predicate, Object, db)
  ).

