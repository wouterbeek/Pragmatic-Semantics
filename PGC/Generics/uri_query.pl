:- module(
  uri_query,
  [
    request_query_read/3, % +Request:list(nvpair)
                          % +QueryName:atom
                          % -PlTerm:term
    uri_query_add/4, % +FromUri:uri
                     % +QueryName:atom
                     % +PlTerm:term
                     % -ToUri:uri
    uri_query_read/3 % +Uri:uri
                     % +QueryName:atom
                     % -PlTerm:term
  ]
).

/** <module> URI query

Support for the query string part of URIs.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(option_ext)).
:- use_module(library(uri)).



%! request_query_read(+Request:list(nvpair), +QueryName:atom, -PlTerm:term) is det.

request_query_read(Request, QueryName, PlTerm):-
  memberchk(search(SearchPairs), Request),
  memberchk(QueryName=Atom, SearchPairs), !,
  read_term_from_atom(Atom, PlTerm, []).


%! uri_query_add(+FromUri:uri, +QueryName:atom, +PlTerm:atom, -ToUri:atom) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_query_add(Uri1, Name, Value1, Uri2):-
  % Disasseble the old URI.
  uri_components(
    Uri1,
    uri_components(Scheme, Authority, Path, SearchString1, Fragment)
  ),

  % When an URI has no search parameters,
  % its search string is uninstantiated.
  catch(
    uri_query_components(SearchString1, SearchOptions1),
    error(instantiation_error,_),
    SearchOptions1 = []
  ),

  % Make sure that we can read the Prolog value back later.
  with_output_to(atom(Value2), write_canonical(Value1)),

  % Search parameters are represented as option lists.
  add_option(SearchOptions1, Name, Value2, SearchOptions2),

  % Construct the new URI.
  uri_query_components(SearchString2, SearchOptions2),
  uri_components(
    Uri2,
    uri_components(Scheme, Authority, Path, SearchString2, Fragment)
  ).


%! uri_query_read(+Uri:uri, +QueryName:atom, -PlTerm:term) is semidet.
% Returns the value for the query item with the given name, if present.
%
% @tbd Can the same query name occur multiple times?

uri_query_read(Uri, Name, PlTerm):-
  uri_components(Uri, UriComponents),
  uri_data(search, UriComponents, QueryString),
  uri_query_components(QueryString, QueryPairs),
  memberchk(Name=Atom, QueryPairs),
  read_term_from_atom(Atom, PlTerm, []).

