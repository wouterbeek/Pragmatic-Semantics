:- module(xfn, []).

/** <module> XFN

@author Wouter Beek
@version 2012/07, 2013/05
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(xfn, 'http://vocab.sindice.com/xfn#').



export_contacts:-
  flag(contacts_id, ID, ID + 1),
  format(atom(Name), 'contacts_~w', [ID]),
  create_file(debug(ce), Name, graphviz, File),
  open(
    File,
    write,
    _Stream,
    [alias(xfn),close_on_abort(true),encoding(utf8),type(test)]
  ),

  format(xfn, 'digraph circuit {\n', []),
  forall(
    rdfs_individual_of(Person, xfn:person),
    (
      rdf_string(Person, xfn:name, Name, xfn),
      format(xfn, '  ~w\n', [Name])
    )
  ),
  forall(
    rdfs_subproperty_of(Relationship, xfn:relation),
    (
      (
        rdf(Person1, Relationship, Person2, xfn)
      ->
        rdf_string(Person1, xfn:name, Name1, xfn),
        rdf_global_id(xfn:RelationshipName, Relationship),
        rdf_string(Person2, xfn:name, Name2, xfn),
        format(
          xfn,
          '  ~w -> ~w [label="~w"]\n',
          [Name1, Name2, RelationshipName]
        )
      ;
        true
      )
    )
  ),

  format(xfn, '}\n', []),
  close(xfn).

find_contacts(_Person1, []):- !.
find_contacts(Person1, List):-
  is_list(List), !,
  maplist(find_contacts(Person1), List).
find_contacts(Person1, element(a, LinkAttributes, [Name])):- !,
  find_link(LinkAttributes, Person2),
  find_relationship(LinkAttributes, RelationshipsAtom),
  atomic_list_concat(RelationshipAtoms, ' ', RelationshipsAtom),
  rdf_assert_string(Person2, xfn:name, Name, xfn),
  forall(
    member(RelationshipAtom, RelationshipAtoms),
    (
      rdf_global_id(xfn:RelationshipAtom, Relationship),
      (
        rdfs_subproperty_of(Relationship, xfn:relation),
        !
      ;
        rdf_assert(Relationship, rdfs:subPropertyOf, xfn:relation, xfn)
      ),
      rdf_assert(Person1, Relationship, Person2, xfn),
      rdf_assert_individual(Person2, xfn:person, xfn)
    )
  ).
find_contacts(Person1, element(_Tag, _Attributes, ListOfContents)):- !,
  find_contacts(Person1, ListOfContents).
find_contacts(_Person1, _Atom).

find_link(L, Link):-
  memberchk(href=Link, L).
find_link(_L, nil).

find_relationship(L, Relationship):-
  memberchk(rel=Relationship, L).
find_relationship(_L, nil).

parse_html(File, DOM):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    (
      dtd(html, DTD),
      load_structure(
        stream(Stream),
        DOM,
        [
          dialect(sgml),
          dtd(DTD),
          max_errors(-1),
          shorttag(false),
          syntax_errors(quiet)
        ]
      )
    ),
    close(Stream, [force(true)])
  ).



:- begin_tests(xfn).

test(xfn, [true]):-
  absolute_file_name(debug(tests), AbsoluteFileName, [file_type(hypertext)]),
  parse_html(AbsoluteFileName, DOM),
  format(user_output, '~w', [DOM]),
  rdf_assert_individual('http://www.wouterbeek.com', xfn:person, xfn),
  rdf_assert_string('http://www.wouterbeek.com', xfn:name, 'Wouter Beek', xfn),
  find_contacts('http://www.wouterbeek.com', DOM),
  export_contacts.

:- end_tests(xfn).

