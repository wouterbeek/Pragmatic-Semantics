:- module(
  dcg_collection,
  [
    collection//6, % :Begin
                   % :End
                   % :Ordering
                   % :Separator
                   % :ElementWriter
                   % +Elements:list(ground)
    list//2, % :ElementWriter
             % +Elements:list(ground)
    nvpair//3, % :ElementWriter
               % +Name:ground
               % +Value:ground
    pair//3,  % +Mode:oneof([ascii,html])
              % :ElementWriter
              % +Pair:pair(ground)
    pair//4,  % +Mode:oneof([ascii,html])
              % :ElementWriter
              % +Element1:ground
              % +Element2:ground
    set//2,  % :ElementWriter
             % +Elements:list(ground)
    tuple//3 % +Mode:oneof([ascii,html])
             % :ElementWriter
             % +Elements:list(ground)
  ]
).

/** <module> DCG collections

DCG rules for parsing/generating collections.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/01 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_error)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(library(option)).

:- meta_predicate(collection(//,//,2,//,3,+,?,?)).
:- meta_predicate(collection_inner(//,//,2,//,3,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(nvpair(3,+,+,?,?)).
:- meta_predicate(pair(+,3,+,?,?)).
:- meta_predicate(pair(+,3,+,+,?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(tuple(+,3,+,?,?)).



%! collection(
%!   :Begin,
%!   :End,
%!   :Ordering,
%!   :Separator,
%!   :ElementWriter,
%!   +Elements:list
%! )// is det.
% Generates a represention of the collection of given elements.
%
% @arg Begin DCG rule that is called before the first element.
% @arg End DCG rule that is called after the last element.
% @arg Ordering Binary predicate that orders the given elements.
% @arg Separator DCG rules that is called in between each two elements.
% @arg ElementWriter Unary DCG rule that writes a single element.
% @arg Elements A list of ground terms that denote
%      the members of the collection.

collection(Begin, End, Ordering, Separator, ElementWriter, Elements1) -->
  % E.g., list -> set.
  {(
    var(Ordering)
  ->
    Elements2 = Elements1
  ;
    once(call(Ordering, Elements1, Elements2))
  )},

  % Open a collection.
  Begin,

  % The contents of the collection.
  collection_inner(Begin, End, Ordering, Separator, ElementWriter, Elements2),

  % End a collection.
  End.

% Done!
collection_inner(_, _, _, _, _, []) --> !, [].
% Nested collection.
collection_inner(Begin, End, Ordering, Separator, ElementWriter, [H|T]) -->
  {is_list(H)}, !,

  collection(Begin, End, Ordering, Separator, ElementWriter, H),

  collection_inner(Begin, End, Ordering, Separator, ElementWriter, T).
% Next set member.
collection_inner(Begin, End, Ordering, Separator, ElementWriter, [H|T]) -->
  % Write a non-collection element.
  dcg_call(ElementWriter, H),

  % The separator does not occur after the last collection member.
  (
    {T == []}, !
  ;
    Separator
  ),

  collection_inner(Begin, End, Ordering, Separator, ElementWriter, T).


%! list(:ElementWriter, +Elements:list(ground))// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(ElementWriter, Elements) -->
  collection(`[`, `]`, =, `,`, ElementWriter, Elements).


%! nvpair(:ElementWriter, +Name:ground, +Value:ground)// is det.

nvpair(ElementWriter, Name, Value) -->
  collection(``, `;`, =, `: `, ElementWriter, [Name,Value]).


%! pair(
%!   +Mode:oneof([ascii,html]),
%!   :ElementWriter,
%!   +Pairs:pair(ground)
%! )// is det.

pair(Mode, ElementWriter, E1-E2) -->
  pair(Mode, ElementWriter, E1, E2).


%! pair(
%!   +Mode:oneof([ascii,html]),
%!   :ElementWriter,
%!   +Element1:ground,
%!   +Element2:ground
%! )// is det.
% Prints the given pair.
%
% @arg Mode The kind of brackets that are printed for this pair.
%      Either `ascii` for using the ASCII characters `<` and `>` (default)
%      or `html` for using the HTML escape sequences `&lang;` and `&rang;`.
% @arg ElementWriter
% @arg Element1
% @arg Element2

pair(Mode, ElementWriter, E1, E2) -->
  {default(ascii, Mode)},
  collection(langle(Mode), rangle(Mode), =, `,`, ElementWriter, [E1,E2]).


%! set(:ElementWriter, +Elements:list(ground))// is det.

set(ElementWriter, L) -->
  collection(`{`, `}`, list_to_ord_set, `,`, ElementWriter, L).


%! tuple(+Mode:oneof([ascii,html]), :ElementWriter, +Elements:list)// is det.
% Prints a tuple.
%
% @arg Mode The kind of brackets that are printed for this pair.
%      Either `ascii` for using the ASCII characters `<` and `>` (default)
%      or `html` for using the HTML escape sequences `&lang;` and `&rang;`.
% @arg ElementWriter
% @arg Elements

tuple(Mode, ElementWriter, L) -->
  {default(ascii, Mode)},
  collection(langle(Mode), rangle(Mode), =, `,`, ElementWriter, L).



% HELPERS %

langle(ascii) --> `<`.
langle(html) --> `&lang;`.

rangle(ascii) --> `>`.
rangle(html) --> `&rang;`.
