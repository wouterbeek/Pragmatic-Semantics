:- module(
  atom_ext,
  [
    atom_splits/3, % +Splits:list(atom)
                   % +Atom:atom
                   % -Subatoms:list(atom)
    atom_to_term/2, % +Atom:atom
                    % -Term:term
    atom_truncate/3, % +Atom:atom
                     % +MaximumLength:integer
                     % -TruncatedAtom:atom
    atomic_atom/2, % ?AtomicOrCodes:or([atom,list(code),number,string])
                   % ?Atom:atom
    atomic_atom/3, % ?Type:oneof([atom,codes,number,string])
                   % ?AtomicOrCodes:or([atom,list(code),number,string])
                   % ?Atom:atom
    first_split/3, % +Atom:atom
                   % +Split:atom
                   % -FirstSubatom:atom
    format_integer/3, % +Integer:integer
                      % +Length:integer
                      % -Atom:atom
    new_atom/2, % +Old:atom
                % -New:atom
    progress_bar/3, % +Current:number
                    % +End:number
                    % -ProgressBar:atom
    repeating_atom/3, % +SubAtom:atom
                      % +Repeats:integer
                      % -Atom:atom
    split_atom_length/3, % +Atom:atom
                         % +Length:integer
                         % -Subatoms:list(atom)
    strip_atom/3, % +Strips:list(atom)
                  % +In:atom
                  % -Out:atom
    strip_atom_begin/3, % +Strips:list(atom)
                        % +In:atom
                        % -Out:atom
    strip_atom_end/3 % +Strips:list(atom)
                     % +In:atom
                     % -Out:atom
  ]
).

/** <module> Atom extensions

Predicates for manipulating atoms.

We assume that atoms are encoded using ASCII (or an ASCII-compatible)
 encoding scheme.

# Replace

In-atom replacements can best be made using DCGs.
This requires the atom to be translated to/from a list of numeric codes.
For example, escaping spaces and grave accent (e.g. in URIs):

~~~{.pl}
% Escape space (SPACE to `%20`) and grave accent (GRAVE-ACCENT -> `%60`).
dcg_phrase(dcg_maplist(dcg_phrase, [[32],[96]], [[37,50,48],[37,54,48]]), AtomIn, AtomOut)
~~~

# Split

`atomic_list_concat(-,+,+)` performs atom splitting
 according to a single given separator.

For using multiple splits at once, use atom_splits/3.

For splitting by a set length, use atom_split_length/3.

# Strip

Stripping atoms of an arbitrary number of subatoms can be done using
 strip_atom/3, strip_atom_begin/3, and strip_atom_end/3.

# Titlecase

Titlecase atoms can be created using upcase_atom/2.

--

@author Wouter Beek
@version 2011/08-2013/05, 2013/07, 2013/09, 2013/11, 2014/01, 2014/03
*/

:- use_module(library(lists)).



%! atom_splits(+Splits:list(atom), +Atom:atom, -Subatoms:list(atom)) is det.
% Returns the given atom split up in two, according to the given split.
% The first split does not include the split atom, making this method
% exclusive.
%
% @arg Splits Atoms where the main atom with be split.
%        Notice that the order in which the splits appear is significant.
% @arg Atom The original, unsplit atom.
% @arg Subatoms The results of splitting.

atom_splits(Splits, Atom1, [H|T]):-
  member(Split, Splits),
  atom_concat(H, Temp, Atom1),
  atom_concat(Split, Atom2, Temp),
  atom_splits(Splits, Atom2, T).
atom_splits(_, Subatom, [Subatom]).


%! atom_to_term(+Atom:atom, -Term:term) is det.
% Returns the term described by the atom.
%
% @arg Atom An atom.
% @arg Term A term.
% @see Wrapper around atom_to_term/3, omitting the bindings.

atom_to_term(Atom, Term):-
  atom_to_term(Atom, Term, _Bindings).


%! atom_truncate(+Atom:atom, +MaxLength:integer, -TruncatedAtom:atom) is det.
% Returns the truncated version of the given atom.
% Truncated atoms end in `...` to indicate its truncated nature.
% The maximum length indicates the exact maximum.
% Truncation will always result in an atom which has at most `MaxLength`.
%
% @arg Atom The original atom.
% @arg MaxLength The maximum allowed length of an atom.
%        This must be at least 5.
% @arg TruncatedAtom The truncated atom.

% The maximum allowed length is too short to be used with truncation.
atom_truncate(A, Max, A):-
  Max =< 5, !.
% The atom does not have to be truncated, it is not that long.
atom_truncate(A, Max, A):-
  atom_length(A, AL),
  AL =< Max, !.
% The atom exceeds the maximum length, it is truncated.
% For this purpose the displayed length of the atom is
%  the maximum length minus 4 (but never less than 3).
atom_truncate(A1, Max, A3):-
  TruncatedL is Max - 4,
  sub_atom(A1, 0, TruncatedL, _, A2),
  atom_concat(A2, ' ...', A3).


%! atomic_atom(
%!   +Atomic:or([atom,list(code),number,string]),
%!   -Atom:atom) is det.
%! atomic_atom(
%!   -Atomic:or([atom,list(code),number,string]),
%!   +Atom:atom
%! ) is nondet.

atomic_atom(Atomic, Atom):-
  atomic_atom(_, Atomic, Atom).

%! atomic_codes(
%!   ?Type:oneof([atom,codes,number,string]),
%!   +Atomic:or([atom,list(code),number,string]),
%!   -Atom:atom
%! ) is det.
%! atomic_codes(
%!   ?Type:oneof([atom,codes,number,string]),
%!   -Atomic:or([atom,list(code),number,string]),
%!   +Atom:atom
%! ) is nondet.
% Instantiation `(?,-,+)` is non-deterministic since a codelist
% could map to an atom, a number, a codelist, and a string.

atomic_atom(Kind, AtomicOrCodes, Atom):-
  nonvar(AtomicOrCodes), !,
  atomic_atom_nondet(Kind, AtomicOrCodes, Atom), !.
atomic_atom(Kind, AtomicOrCodes, Atom):-
  atomic_atom_nondet(Kind, AtomicOrCodes, Atom).

% Number.
atomic_atom_nondet(number, Number, Atom):-
  \+ ((
    nonvar(Number),
    \+ number(Number)
  )),
  catch(
    atom_number(Atom, Number),
    error(syntax_error(illegal_number),_Context),
    fail
  ).
% String.
atomic_atom_nondet(string, String, Atom):-
  \+ ((
    nonvar(String),
    \+ string(String)
  )),
  atom_string(Atom, String).
% Codes.
atomic_atom_nondet(codes, Codes, Atom):-
  \+ ((
    nonvar(Codes),
    \+ is_list(Codes)
  )),
  atom_codes(Atom, Codes).
% Atom.
atomic_atom_nondet(atom, Atom, Atom):-
  atom(Atom).


%! first_split(+Atom:atom, +Split:atom, -FirstSubatom:atom) is nondet.
% Returns the first split.
% For the first result this is behaviorally equivalent to:
% ~~~{.pl}
% atomic_list_concat(Subatoms, Split, Atom),
% Subatoms = [FirstSubatom|_]
% ~~~

first_split(Atom, Split, FirstSubatom):-
  atom_concat(Subatom, _, Atom),
  atom_concat(FirstSubatom, Split, Subatom).


%! format_integer(+Integer:integer, +Length:integer, -Atom:atom) is det.
% Returns a formatted representation of the given integer
%  that is exactly the given number of characters long.
%
% Fails in case the length of the formatted integer exceeds the given length.
%
% @arg Integer The integer value that is to be formatted.
% @arg Length The exact character length of the formatted integer atom.
% @arg Atom The formatted version of the integer value.
%
% @tbd See whether this can be done using format/2 tab stops,
%      http://www.swi-prolog.org/pldoc/doc_for?object=format/2.

format_integer(I, L, Out):-
  atom_length(I, IL),
  ZeroLength is L - IL,
  repeating_atom('0', ZeroLength, Zeros),
  atomic_concat(Zeros, I, Out).


%! new_atom(+Old:atom, -New:atom) is det.
% Returns a new atom, based on the given atom
%  either by incrementing its index,
%  or by adding such an index.
%
% This predicate comes in handy when creating unique identifiers
%  based on a given base name, e.g. for threads, RDF graphs, files, etc.

new_atom(A1, A2):-
  atomic_list_concat(Splits, '_', A1), % split
  reverse(Splits, [LastSplit|RestSplits]),
  (
    atom_number(LastSplit, OldNumber)
  ->
    NewNumber is OldNumber + 1,
    atom_number(NewLastSplit, NewNumber),
    reverse([NewLastSplit|RestSplits], NewSplits)
  ;
    reverse(['1',LastSplit|RestSplits], NewSplits)
  ),
  atomic_list_concat(NewSplits, '_', A2).


%! progress_bar(+Current:integer, End:integer, ProgressBar:atom) is det.
% Returns an atomic progress bar that displays the current value
%  onto a scale that runs from `1` to the given end value.
%
% @arg Current An integer, representing the current processed value.
% @arg End An integer, representing the last value to be processed.
% @arg ProgressBar The atomic representation of a progress bar.

progress_bar(End, End, ProgressBar2):- !,
  progress_bar_(End, End, ProgressBar1),
  format(atom(ProgressBar2), '~w [done]', [ProgressBar1]).
progress_bar(Current, End, ProgressBar):-
  progress_bar_(Current, End, ProgressBar).

progress_bar_(Current1, End, ProgressBar):-
  (
     End =:= 0
  ->
     Percentage = 100
  ;
     Percentage is round(Current1 / End * 100)
  ),
  format_integer(Percentage, 3, Percentage1),
  (
     End =:= 0
  ->
    Progress = 10
  ;
    Progress is round(Current1 / (End / 10))
  ),
  atom_number(EndAtom, End),
  atom_length(EndAtom, EndLength),
  format_integer(Current1, EndLength, Current2),
  repeating_atom('=', Progress, Bar),
  Fill is 10 - Progress,
  repeating_atom('-', Fill, NonBar),
  format(
    atom(ProgressBar),
    '~w% ~w~w (~w/~w)',
    [Percentage1, Bar, NonBar, Current2, End]
  ).


%! repeating_atom(+SubAtom:atom, +Repeats:integer, -Atom:atom) is det.
% Returns the atom that is the repetition of the given subatom
%  for the given number of times.
%
% @arg SubAtom An atom, the element that gets repeated.
% @arg Repeats A integer, the number of repeats of the subatom.
% @arg Atom An atom, the result of repeating the given atom.

repeating_atom(_SubAtom, 0, ''):- !.
repeating_atom(SubAtom, 1, SubAtom):- !.
repeating_atom(SubAtom, Repeats, Atom):-
  Repeats > 1,
  NewRepeats is Repeats - 1,
  repeating_atom(SubAtom, NewRepeats, Atom1),
  atomic_concat(Atom1, SubAtom, Atom).


%! split_atom_length(
%!   +Atom:atom,
%!   +Length:nonneg,
%!   -Subatoms:list(atom)
%! ) is nondet.
% Splits atoms by length.
% The last subatom is allowed to have a shorter length.
%
% If `Length` is zero this predicate does not terminate.
% This is the correct behavior, since there is an infinite number of
%  empty subatoms in each atom.
%
% @throws domain_error When `Length` is less than zero.

split_atom_length('', _, []):- !.
split_atom_length(A1, L, [H|T]):-
  sub_atom(A1, 0, L, _, H), !,
  atom_concat(H, A2, A1),
  split_atom_length(A2, L, T).
split_atom_length(A, _, [A]).


%! strip_atom(+Strips:list(atom), +In:atom, -Out:atom) is det.
%! strip_atom_begin(+Strips:list(atom), +In:atom, -Out:atom) is det.
%! strip_atom_end(+Strips:list(atom), +In:atom, -Out:atom) is det.
% Strips the given atom's front and/or back for the given character.
%
% Notice that the order in which the strip atoms occur is significant.
%
% @arg Strips A list of atoms that will be stripped.
% @arg In The non-strippped atom
% @arg Out The stripped atom.

strip_atom(Strips, A1, A3):-
  strip_atom_begin(Strips, A1, A2),
  strip_atom_end(Strips, A2, A3).

strip_atom_begin(Strips, A1, A3):-
  member(Strip, Strips),
  atom_concat(Strip, A2, A1), !,
  strip_atom_begin(Strips, A2, A3).
strip_atom_begin(_, A, A).

strip_atom_end(Strips, A1, A3):-
  member(Strip, Strips),
  atom_concat(A2, Strip, A1), !,
  strip_atom_end(Strips, A2, A3).
strip_atom_end(_, A, A).

