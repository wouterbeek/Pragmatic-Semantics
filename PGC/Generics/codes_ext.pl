:- module(
  codes_ext,
  [
    atomic_codes/2, % ?AtomicOrCodes:or([atom,list(code),number,string])
                    % ?Codes:list(code)
    atomic_codes/3, % ?Type:oneof([atom,codes,number,string])
                    % ?AtomicOrCodes:or([atom,list(code),number,string])
                    % ?Codes:list(code)
    atomic_codes_goal/3, % :Goal
                         % ?AtomicOrCodes1:or([atom,list(code),number,string])
                         % ?AtomicOrCodes2:or([atom,list(code),number,string])
    code_remove/3, % +FromCodes:list(code)
                   % +Remove:code
                   % -ToCodes:list(code)
    code_replace/4, % +FromCodes:list(code)
                    % +From:code
                    % +To:code
                    % +ToCodes:list(code)
    codes_remove/3, % +FromCodes:list(code)
                    % +Remove:list(code)
                    % -ToCodes:list(code)
    codes_replace/3, % +FromCodes:list(code)
                     % +Pairs:list(pair(code))
                     % +ToCodes:list(code)
    codes_to_atom/2, % +Codes:list(code)
                     % -Atom:atom
    put_codes/1, % +Codes:list(code)
    put_codes/2, % +Stream:stream
                 % +Codes:list(code)
    strip_codes/3, % +Strip:list(list(code))
                   % +In:list(code)
                   % -Out:list(code)
    strip_codes_begin/3, % +Strip:list(list(code))
                         % +In:list(code)
                         % -Out:list(code)
    strip_codes_end/3 % +Strip:list(list(code))
                      % +In:list(code)
                      % -Out:list(code)
  ]
).

/** <module> Codes extensions

Predicates for handling codes.

# Replace

Replacements in list of codes can be made using:
~~~{.pl}
phrase(dcg_maplist(dcg_replace, [FromDCG|FromDCGs], [ToDCG|ToDCGs]), In, Out)
~~~

# Split

Lists of codes can be splitted using:
~~~{.pl}
phrase(dcg_separated_list(:SeparatorDCG,-Sublists:list(list(code))), Codes)
~~~

# Strip

Stripping codes lists is simply done using append,
 see strip_codes/3, strip_codes_begin/3, and strip_codes_end/3.

--

@author Wouter Beek
@version 2013/05-2013/07, 2013/12-2014/03
*/

:- use_remote_module(generics(codes_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate(atomic_codes_goal(2,?,?)).



%! atomic_codes(
%!   +Atomic:or([atom,list(code),number,string]),
%!   -Codes:list(code)
%! ) is det.
%! atomic_codes(
%!   -Atomic:or([atom,list(code),number,string]),
%!   +Codes:list(code)
%! ) is nondet.

atomic_codes(Atomic, Codes):-
  atomic_codes(_, Atomic, Codes).

%! atomic_codes(
%!   ?Type:oneof([atom,codes,number,string]),
%!   +Atomic:or([atom,list(code),number,string]),
%!   -Codes:list(code)
%! ) is det.
%! atomic_codes(
%!   ?Type:oneof([atom,codes,number,string]),
%!   -Atomic:or([atom,list(code),number,string]),
%!   +Codes:list(code)
%! ) is nondet.
% Instantiation `(?,-,+)` is non-deterministic since a codelist
% could map to an atom, a number, and a codelist.

atomic_codes(Kind, Atomic, Codes):-
  nonvar(Atomic), !,
  atomic_codes_nondet(Kind, Atomic, Codes), !.
atomic_codes(Kind, Atomic, Codes):-
  atomic_codes_nondet(Kind, Atomic, Codes).

% Atom.
atomic_codes_nondet(atom, Atom, Codes):-
  \+ ((
    nonvar(Atom),
    \+ atom(Atom)
  )),
  atom_codes(Atom, Codes).
% Number.
atomic_codes_nondet(number, Number, Codes):-
  \+ ((
    nonvar(Number),
    \+ number(Number)
  )),
  catch(
    number_codes(Number, Codes),
    error(syntax_error(illegal_number),_Context),
    fail
  ).
% String.
atomic_codes_nondet(string, String, Codes):-
  \+ ((
    nonvar(String),
    \+ string(String)
  )),
  string_codes(String, Codes).
% Codes.
atomic_codes_nondet(codes, Codes, Codes):-
  is_list(Codes).


%! atomic_codes_goal(
%!   :Goal,
%!   ?AtomicOrCodes:or([atom,list(code),number,string]),
%!   ?AtomicOrCodes:or([atom,list(code),number,string])
%! ) .

atomic_codes_goal(Goal, From1, To1):-
  atomic_codes(Kind, From1, From2),
  % From codes to codes.
  call(Goal, From2, To2),
  atomic_codes(Kind, To1, To2).


%! code_remove(
%!   +FromCodes:list(code),
%!   +Remove:code,
%!   -ToCodes:list(code)
%! ) is det.
% @see Wrapper around codes_remove/3.

code_remove(FromCodes, Remove, ToCodes):-
  codes_remove(FromCodes, [Remove], ToCodes).


%! code_replace(
%!   +FromCodes:list(code),
%!   +From:code,
%!   +To:code,
%!   +ToCodes:list(code)
%! ) is det.
% @see Wrapper around codes_replace/3.

code_replace(FromCodes, From, To, ToCodes):-
  codes_replace(FromCodes, [From-To], ToCodes).


%! code_remove(
%!   +FromCodes:list(code),
%!   +Remove:list(code),
%!   -ToCodes:list(code)
%! ) is det.

codes_remove([], _, []):- !.
codes_remove([H|T1], Xs, T2):-
  member(H, Xs), !,
  codes_remove(T1, Xs, T2).
codes_remove([H|T1], Xs, [H|T2]):- !,
  codes_remove(T1, Xs, T2).


%! codes_replace(
%!   +FromCodes:list(code),
%!   +Pairs:list(pair(code)),
%!   +ToCodes:list(code)
%! ) is det.

codes_replace([], _, []):- !.
codes_replace([X|T1], Pairs, [Y|T2]):-
  member(X-Y, Pairs), !,
  codes_replace(T1, Pairs, T2).
codes_replace([H|T1], Pairs, [H|T2]):-
  codes_replace(T1, Pairs, T2).


%! codes_to_atom(+Codes:list(code), -Atom:atom) is det.
% This is solely used in contexts where the argument order is fixed,
%  and the codes parameter just happends to occur before the atom parameter.

codes_to_atom(Codes, Atom):-
  atom_codes(Atom, Codes).


%! put_codes(+Codes:list(code)) is det.
%! put_codes(+Out:stream, +Codes:list(code)) is det.
% @see Wrapper around put_code/1 that works on lists of codes
%      and that can write to an arbitrary stream.

put_codes(Codes):-
  maplist(put_code, Codes).
put_codes(Out, Codes):-
  with_output_to(Out, maplist(put_code, Codes)).


%! strip_codes(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
%! strip_codes_begin(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
%! strip_codes_end(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
% Strips the given atom's front and/or back for the given character.
%
% Notice that the order in which the strip atoms occur is significant.
%
% @tbd Do this with DCG rules instead of lists in `Strips`.

strip_codes(Strips, C1, C3):-
  strip_codes_begin(Strips, C1, C2),
  strip_codes_end(Strips, C2, C3).
strip_codes_begin(Strips, C1, C3):-
  member(Strip, Strips),
  append(Strip, C2, C1),
  strip_codes_begin(Strips, C2, C3).
strip_codes_begin(_, C, C).
strip_codes_end(Strips, C1, C3):-
  member(Strip, Strips),
  append(C2, Strip, C1),
  strip_codes_end(Strips, C2, C3).
strip_codes_end(_, C, C).

