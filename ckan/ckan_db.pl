 :- module(
  ckan_db,
  [
    ckan_file/4, % +Site:atom
                 % +Name:atom
                 % +Extension:atom
                 % -File:atom
    ckan_graph/2, % +Site:atom
                  % -RdfGraph:atom
    ckan_properties/2, % +Site:atom
                       % -Properties:list(compound)
    ckan_uri/2 % +Site:atom
               % -Uri:uri
  ]
).

/** <module> CKAN database

Registers information about CKAN Web sites.

@author Wouter Beek
@version 2014/04
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(generics(user_input)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(xml(xml_namespace)).

:- use_module(library(uri)).

:- xml_register_namespace(ckan, 'http://www.wouterbeek.com/ckan#').

:- multifile(user:api_key_hook/2).



%! api_key(+Site:atom, -Key:atom) is det.
% Allows the user to enter an API key for a CKAN site.

api_key(Site, Key):-
  user:api_key_hook(Site, Key), !.
api_key(Site, Key):-
  format(atom(Msg), 'Enter the API key for ~a.', [Site]),
  user_input(Msg, codes, Key).


%! ckan_file(+Site:atom, +Name:atom, +Extension:atom, -File:atom) is det.

ckan_file(Site, Base, Ext, File):-
  ckan_uri(Site, Uri),
  file_name_extension(Base, Ext, Name),
  url_nested_directory(data(.), Uri, File).


%! ckan_graph(+Site:atom, -RdfGraph:atom) is det.

ckan_graph(Site, RdfGraph):-
  ckan_uri(Site, Uri),
  url_rdf_graph(Uri, RdfGraph).


%! ckan_property(+Site:atom, +Property:compound) is semidet.
%! ckan_property(+Site:atom, -Property:compound) is det.
%! ckan_property(-Site:atom, -Property:compound) is nondet.
% Properties of CKAN sites that are needed to perform queries.
%
% The following properties are supported:
%   * `api_key(?HasKey:boolean)`
%   * `api_version(?Version:positive_integer)`
%   * `authority(?UriAuthority:atom)`
%   * `deprecated(?AllowDeprecatedParameters:boolean)`
%   * `paginated(?UsePaginatedResults:boolean)`
%   * `scheme(?UriScheme:atom)`

ckan_property(data_gov_uk, api_version(3)).
ckan_property(data_gov_uk, authority('data.gov.uk')).
ckan_property(data_gov_uk, deprecated(true)).
ckan_property(data_gov_uk, paginated(true)).
ckan_property(data_gov_uk, scheme(http)).
ckan_property(data_overheid_nl, authority('data.overheid.nl')).
ckan_property(data_overheid_nl, scheme(https)).
ckan_property(datahub_io, api_version(3)).
ckan_property(datahub_io, authority('datahub.io')).
ckan_property(datahub_io, deprecated(true)).
ckan_property(datahub_io, paginated(true)).
ckan_property(datahub_io, scheme(http)).


%! ckan_properties(+Site:atom, -Properties:list(nvpair)) is det.
% Returns all CKAN properties for a specific site
% that are needed to perform queries.

ckan_properties(Site, [api_key(Key)|T]):-
  api_key(Site, Key),
  findall(
    Property,
    ckan_property(Site, Property),
    T
  ).


%! ckan_uri(+Site:atom, -Uri:uri) is det.

ckan_uri(Site, Uri):-
  ckan_property(Site, scheme(Scheme)),
  ckan_property(Site, authority(Auth)),
  uri_components(Uri, uri_components(Scheme,Auth,_,_,_)).

