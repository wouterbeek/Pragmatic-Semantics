:- module(
  lod_location,
  [
    lod_header/3, % +Prefix:atom
                    % ?Name:atom
                    % ?Value:atom
    lod_location/2, % +Prefix:atom
                      % -Location:atom
    lod_register_header/3, % +Prefix:atom
                           % +Name:atom
                           % +Value:atom
    lod_register_location/2 % +Prefix:atom
                            % +Location:atom
  ]
).

/** <module> LOD location

Support for Web locations that store LOD descriptions.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(db_ext)).

:- dynamic(lod_header_/3).
:- dynamic(lod_location_/2).



% lod_header(+Prefix:atom, ?Name:atom, ?Value:atom) is nondet.

lod_header(Prefix, Name, Value):-
  lod_header_(Prefix, Name, Value).

lod_register_header(Prefix, Name, Value):-
  db_add_novel(lod_header_(Prefix, Name, Value)).


% lod_location(+Prefix:atom, -Location:iri) is det.
% Used in case the XML namespace does not denote
%  a machine-readable description of the vocabulary, e.g. Dublin Core.

lod_location(Prefix, Location):-
  lod_location_(Prefix, Location).

lod_register_location(Prefix, Location):-
  db_add_novel(lod_location_(Prefix, Location)).

