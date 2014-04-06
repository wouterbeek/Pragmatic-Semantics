:- module(
  stcn_generic,
  [
    ppn//0,
    ppn//1, % -PPN:atom
    ppn//2, % +Category:atom
            % -PPN:atom
    ppn_resource/3 % +Category:atom
                   % +PPN_Name:atom
                   % -PPN:iri
  ]
).

/** <module> STCN generics

Things that are used throughout the STCN project,
but are not generic enough to be in PGC.

@author Wouter Beek
@version 2013/06, 2013/09-2013/10
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').

:- rdf_meta(category_class(?,r)).



ppn -->
  ppn(_PPN).

%! ppn(-PPN:atom)//

ppn(PPN) -->
  dcg_multi1(ppn_char, 9, PPN, [convert(codes_atom)]).

%! ppn(+Category:atom, -PPN:iri)// is det.
% Parses a PPN identifier and returns its IRI.
%
% There are 3*10**8 possibilities, so don't use this for generation.

ppn(Category, PPN2) -->
  ppn(PPN1),
  {ppn_resource(Category, PPN1, PPN2)}.

ppn_char(C) --> decimal_digit(C).
ppn_char(C) --> x(C).

%! ppn_resource(+Category:atom, +PPN_Name:atom, -PPN:iri) is det.

ppn_resource(Category, PPN_Name1, PPN):-
  atomic_list_concat([Category,PPN_Name1], '/', PPN_Name2),
  rdf_global_id(stcn:PPN_Name2, PPN).

