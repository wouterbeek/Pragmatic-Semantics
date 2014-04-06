:- module(
  mime_type,
  [
% READ
    mime_type/1, % ?MIME:atom
    mime_type/2, % ?Type:atom
                 % ?Subtype:atom
    mime_type_file_extension/2, % ?MIME:atom
                                % ?DefaultExtension:atom
% BUILD
    mime_register_type/3 % +Type:atom
                         % +Subtype:atom
                         % +DefaultExtension:atom
  ]
).

/** <module> MIME type

Support for IANA-registered MIME types.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)). % RDF-serialization.
:- use_module(library(semweb/rdf_turtle_write)). % RDF-serialization.
:- use_module(library(xpath)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(standards(iana_to_rdf)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- initialization(init_mime).



mime_type(MIME):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  mime_type(Type, Subtype).
mime_type(MIME):-
  mime_type(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).

mime_type(Type, Subtype):-
  rdfs_individual_of(Registration, iana:'Registration'),
  mime_type(Registration, Type, Subtype).

mime_type(Registration, Type, Subtype):-
  rdfs_label(Registration, Subtype),
  once(rdf(Registration, rdf:type, Class)),
  once(rdfs_label(Class, Type)).


%! mime_type_file_extension(+MIME:atom, +DefaultExtension:atom) is semidet.
%! mime_type_file_extension(-MIME:atom, +DefaultExtension:atom) is semidet.
%! mime_type_file_extension(+MIME:atom, -DefaultExtension:atom) is semidet.
%! mime_type_file_extension(?MIME:atom, ?DefaultExtension:atom) is nondet.

mime_type_file_extension(MIME, DefaultExtension):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  rdf_string(Registration, iana:template, Subtype, _),
  rdf(Registration, rdf:type, Class),
  rdfs_label(Class, _, Type),
  rdf_string(Registration, iana:default_extension, DefaultExtension, _).
mime_type_file_extension(MIME, DefaultExtension):-
  rdf_string(Registration, iana:default_extension, DefaultExtension, _),
  mime_type(Registration, Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).


%! mime_register_type(
%!   +Type:atom,
%!   +Subtype:atom,
%!   +DefaultExtension:atom
%! ) is det.

% Already registered.
mime_register_type(Type, Subtype, _):-
  mime_type(Type, Subtype), !.
% New registration.
mime_register_type(Type, Subtype, DefaultExtension):-
  mime_register_type(Type, Subtype, DefaultExtension, mime_ext).

mime_register_type(Type1, Subtype, DefaultExtension, G):-
  % The table of the Website we scrape for file extensions
  %  contains a typo: `applicaiton` i.o. `application`.
  (Type1 == applicaiton -> Type2 = application ; Type2 = Type1),

  % Assert type.
  rdf_global_id(iana:Type2, Class),
  rdf_assert(Class, rdfs:subClassOf, iana:'Registration', G),
  rdf_assert(Class, rdfs:label, literal(type(xsd:string,Type2)), G),

  % Assert subtype.
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, G),
  rdf_assert(Registration, iana:template, literal(type(xsd:string,Subtype)), G),
  rdf_assert(Registration, rdfs:label, literal(type(xsd:string,Subtype)), G),

  assert_mime_schema_ext(G),
  rdf_assert(Registration, iana:default_extension, literal(type(xsd:string,DefaultExtension)), G),

  atomic_list_concat([Type2,Subtype], '/', MIME),
  db_add_novel(user:prolog_file_type(DefaultExtension, MIME)).

assert_mime_schema_ext(G):-
  % Property default file extension.
  rdf_assert_property(iana:default_extension, G),
  rdf_assert(iana:default_extension, rdfs:domain, iana:'Registration', G),
  rdf_assert(iana:default_extension, rdfs:range, xsd:string, G),
  rdf_assert(iana:default_extension, rdfs:label, literal(type(xsd:string,'default file extension')), G).


init_mime:-
  absolute_file_name(
    data(mime),
    File,
    [access(read),extensions([ttl]),file_errors(fail)]
  ), !,
  rdf_load(File, [format(turtle),graph(mime)]),
  mime_register_type(application, 'atom+xml',         atom),
  mime_register_type(application, 'x-rar-compressed', rar ),
  mime_register_type(application, 'x-bibtex',         bib ),
  mime_register_type(application, 'n-quads',          nq  ),
  mime_register_type(application, 'n-triples',        nt  ).
init_mime:-
  assert_iana(
    mime,
    'http://www.iana.org/assignments/media-types/',
    iana:'MIMERegistration',
    [application,audio,image,message,model,multipart,text,video]
  ),

  assert_mime_extensions(mime),

  absolute_file_name(data('mime.ttl'), File, [access(write)]),
  rdf_save_turtle(File, [graph(mime)]),

  init_mime.


assert_mime_extensions(G):-
  download_html(
    [html_dialect(html4)],
    'http://www.webmaster-toolkit.com/mime-types.shtml',
    DOM
  ),
  forall(
    (
      member(Class, [tablerowdark,tablerowlight]),
      xpath(DOM, //tr(@class=Class), TR),
      xpath(TR, td(1,content), [DefaultExtension]),
      xpath(TR, td(2,content), [MIME])
    ),
    (
      atomic_list_concat([Type,Subtype], '/', MIME),
      mime_register_type(Type, Subtype, DefaultExtension, G)
    )
  ).

