:- module(
  ckan_query,
  [
    ckan_query/3 % +Site:atom
                 % +Predicate:atom
                 % +Arguments:list
  ]
).

/** <module> CKAN query

Support for querying CKAN.

@author Wouter Beek
@version 2014/03
*/

:- use_remote_module(ckan(ckan_api)).
:- use_remote_module(ckan(ckan_db)).



%! ckan_query(+Site:atom, +Predicate:atom, +Arguments:list) is det.
% Performs a specific CKAN query.

ckan_query(Site, Pred, Args):-
  ckan_properties(Site, Properties),
  Call =.. [Pred,Properties|Args],
  call(Call).

