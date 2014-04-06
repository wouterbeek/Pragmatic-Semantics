:- module(
  webqr_generic,
  [
    request_to_user_name/2, % +Request:list
                            % -User:atom
    user_pref_label/3, % +User:atom
                       % +GlobalURI:atom
                       % -Label:atom
    webqr_exists/3, % +Request:list
                    % +User:atom
                    % +LocalURI:atom
    webqr_global_graph/2, % +User:atom
                          % -GlobalGraph:atom
    webqr_local_graph/2 % +User:atom
                        % -LocalGraph:atom
  ]
).

/** <module> WebQR generic

Generic predicates for WebQR.

@author Wouter Beek
@version 2013/12-2014/01
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(server(user_db)).



% @tbd Use the HTTP cookie header for this.
request_to_user_name(Request, User):-
  memberchk(session(User), Request).


user_pref_label(_User, GlobalURI, Label):-
  %user_property(User, natural_languages, Langs),
  Langs = [nl,en],
  rdfs_preferred_label(Langs, GlobalURI, _, Label, _), !.
user_pref_label(_, _, 'No label').


%! webqr_exists(+Request:list, +User:atom, +LocalURI:atom) is det.
% Succeeds if the given URI exists in the user's local graph.

webqr_exists(_, User, LocalURI):-
  webqr_local_graph(User, LocalGraph),
  rdf(LocalURI, _, _, LocalGraph), !.
webqr_exists(Request, _, _):-
  http_404([], Request).


%! webqr_global_graph(+User:atom, -GlobalGraph:atom) is det.
%! webqr_local_graph(+User:atom, -LocalGraph:atom) is det.
% We have a local and a global RDF graph.
%
% The **global RDF graph** represents the user's QR model
%  that can be saved to a file,
%  that can be shared with others,
%  and that can be used to enrich the LOD cloud.
%
% The **local RDF graph** represents details about
%  when and how the user created the QR model.
% This data should not be
%  saved to a file,
%  shared with other users,
%  and it would not be correct to enrich the LOD cloud with it.

webqr_global_graph(User, GlobalGraph):-
  atomic_list_concat([User,global], '_', GlobalGraph).

webqr_local_graph(User, LocalGraph):-
  atomic_list_concat([User,local], '_', LocalGraph).

