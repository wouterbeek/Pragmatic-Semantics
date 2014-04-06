:- module(
  typecheck,
  [
    atom_to_value/3, % +Atom
                     % +Type
                     % -Value
    prolog_convert_value/4 % +FromDatatype:atom
                           % +FromValue
                           % +ToDatatype:atom
                           % -ToValue
  ]
).
:- reexport(library(error), [
  is_of_type/2, % +Type
                % @Term
  must_be/2 % +Type
            % @Term
]).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

| *Type*               | *|Defined here|* | *|Atom-conversion|* |
| atom                 |                  | Yes                 |
| atomic               |                  |                     |
| between/2            |                  | Yes                 |
| boolean              |                  | Yes                 |
| callable             |                  |                     |
| char                 |                  | Yes                 |
| chars                |                  | Yes                 |
| code                 |                  | Yes                 |
| codes                |                  | Yes                 |
| compound             |                  |                     |
| constant             |                  |                     |
| email                |                  |                     |
| encoding             |                  |                     |
| float                |                  | Yes                 |
| ground               |                  |                     |
| integer              |                  | Yes                 |
| list                 |                  |                     |
| list/1               | Yes              |                     |
| list_or_partial_list |                  |                     |
| negative_integer     |                  | Yes                 |
| nonneg               |                  | Yes                 |
| nonvar               |                  |                     |
| number               |                  | Yes                 |
| oneof/1              |                  | Yes                 |
| or/1                 | Yes              |                     |
| positive_integer     |                  | Yes                 |
| rational             |                  |                     |
| string               |                  | Yes                 |
| symbol               |                  |                     |
| text                 |                  |                     |
| uri                  | Yes              |                     |
| iri                  | Yes              |                     |
| var                  |                  |                     |

--

@author Wouter Beek
@version 2013/01, 2013/08, 2014/01, 2014/03-2014/04
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(atom_ext)).
:- use_module(generics(boolean_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(uri(rfc3987_dcg)).



%! atom_to_value(+Atom:atom, +Type:compound, -Value) is det.
% Interpret `Atom` according to `Type`.

% Atom.
atom_to_value(Atom, atom, Atom):- !.
% Between two integers (inclusive).
atom_to_value(Atom, between(L,H), I):- !,
  atom_to_value(Atom, integer, I),
  L =< I,
  H >= I.
% Boolean.
atom_to_value(Atom, boolean, Boolean):- !,
  to_boolean(Atom, Boolean).
% Character.
atom_to_value(Char, char, Char):- !,
  atom_length(Char, 1).
% List of characters.
atom_to_value(Atom, chars, Chars):- !,
  atom_chars(Atom, Chars).
% Code.
atom_to_value(Atom, code, Code):- !,
  atom_to_value(Atom, char, Char),
  char_code(Char, Code).
% Codes.
atom_to_value(Atom, codes, Codes):- !,
  atom_codes(Atom, Codes).
% Float.
atom_to_value(Atom, float, Float):- !,
  atom_number(Atom, Number),
  Float = float(Number).
% Integer.
atom_to_value(Atom, integer, I):- !,
  atom_number(Atom, I),
  integer(I).
% Negative integer.
atom_to_value(Atom, negative_integer, I):- !,
  atom_to_value(Atom, integer, I),
  I < 0.
% Non-negative integer.
atom_to_value(Atom, nonneg, I):- !,
  atom_to_value(Atom, integer, I),
  I >= 0.
% Number.
atom_to_value(Atom, number, Number):- !,
  atom_number(Atom, Number).
% Positive integer.
atom_to_value(Atom, positive_integer, I):- !,
  atom_to_value(Atom, integer, I),
  I > 0.
% One from a given list of atoms.
atom_to_value(Atom, oneof(L), Atom):- !,
  memberchk(Atom, L).
% String.
atom_to_value(Atom, string, String):- !,
  atom_string(Atom, String).


% char/0
error:has_type(char, Term):-
  once(char_type(Term, _)).
% code/0
error:has_type(code, Term):-
  once(code_type(Term, _)).
% email/0
error:has_type(email, Term):-
  dcg_phrase(email, Term).
% float_between/2, extension of between/2 for floats
% allowing uninstiated upper and lower bounds.
error:has_type(float_between(L,U), X):-
  number(X),
  (number(L) -> X >= L ; true),
  (number(U) -> X =< L ; true).
% or/1
error:has_type(or(Types), Term):-
  member(Type, Types),
  error:has_type(Type, Term), !.
% list/1
error:has_type(list(Type), Term):-
  must_be(list, Term),
  maplist(must_be(Type), Term).
% uri/0
error:has_type(uri, Term):-
  error:has_type(iri, Term).
% iri/0
error:has_type(iri, Term):-
  uri_components(
    Term,
    uri_components(Scheme,Authority,Path,_Search,_Fragment)
  ),
  maplist(nonvar, [Scheme,Authority,Path]).
  % @tbd
  %%%%once(dcg_phrase('IRI'(_), Term)),


:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
email -->
  dcg_until([end_mode(inclusive),output_format(codes)], at_sign, _),
  dcg_all.


%! prolog_convert_value(
%!   +FromDatatype:atom,
%!   +FromValue,
%!   +ToDatatype:atom,
%!   -ToValue
%! ) is det.

prolog_convert_value(_, FromValue, ToDatatype, ToValue):-
  format(atom(Atom), '~w', [FromValue]),
  atom_to_value(Atom, ToDatatype, ToValue).

