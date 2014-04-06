:- module(
  dcg_generic,
  [
    dcg_all//0,
    dcg_all//2, % +Options:list(nvpair)
                % -Result:or([atom,list(code)])
    dcg_between//2, % :Between
                    % :DCG
    dcg_between//3, % :Begin
                    % :DCG
                    % :End
    dcg_copy//0,
    dcg_done//0,
    dcg_end//0,
    dcg_separated_list//2, % :Separator:dcg
                           % ?Codess:list(list(codes))
    dcg_phrase/2, % :DCG
                  % ?AtomicOrCodes:or([atom,list(code),number])
    dcg_phrase/3, % :DCG
                  % ?AtomicOrCodes1:or([atom,list(code),number])
                  % ?AtomicOrCodes2:or([atom,list(code),number])
    dcg_switch//2, % +Value
                   % +Map:list
    dcg_switch//3, % +Value
                   % +Map:list
                   % +Default
    dcg_until//2, % :End:dcg
                  % ?Value
    dcg_until//3, % +Options:list(nvpair)
                  % :End:dcg
                  % ?Value
    dcg_with_output_to/2 % +Output:compound
                         % :DCG
  ]
).

/** <module> DCG generics.

Generic support for DCG rules.

## Concepts

  * *|Lexical analysis|*
    *Tokenization*
    The process of converting characters to tokens
    (i.e., strings of characters).

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2014/01, 2014/03
*/

:- use_remote_module(generics(codes_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).



%! dcg_all// is det.
%! dcg_all(+Options:list(nvpair), -Result:or([atom,list(code)]))// is det.
% The following options are available:
%   * =|output_format(+Format:oneof([atom,codes]))|=

dcg_all -->
  dcg_all([], _).

dcg_all(O1, Result) -->
  dcg_all_(Codes),
  {(
    option(output_format(atom), O1, codes)
  ->
    atom_codes(Result, Codes)
  ;
    Result = Codes
  )}.

dcg_all_([H|T]) -->
  [H],
  dcg_all_(T).
dcg_all_([]) -->
  [].


%! dcg_between(:Between, :DCG)// .
%! dcg_between(:Begin, :DCG, :End)// .

:- meta_predicate(dcg_between(//,//,?,?)).
dcg_between(Between, DCG) -->
  dcg_between(Between, DCG, Between).

:- meta_predicate(dcg_between(//,//,//,?,?)).
dcg_between(Begin, DCG, End) -->
  phrase(Begin),
  phrase(DCG),
  phrase(End).


dcg_copy, [X] -->
  [X],
  dcg_copy.
dcg_copy --> dcg_end.


dcg_done(_, _).


dcg_end([], []).


%! dcg_phrase(:DCG, ?AtomicOrCodes:or([atom,list(code),number]))// is nondet.
%! dcg_phrase(
%!   :DCG,
%!   ?AtomicOrCodes1:or([atom,list(code),number]),
%!   ?AtomicOrCodes2:or([atom,list(code),number])
%! )// is nondet.

dcg_phrase(DCG, X1):-
  atomic_codes(X1, X2),
  phrase(DCG, X2).

dcg_phrase(DCG, X1, Y1):-
  atomic_codes(X1, X2),
  phrase(DCG, X2, Y2),
  atomic_codes(Y1, Y2).


%! dcg_separated_list(
%!   +Separator:dcg_rule,
%!   ?CodeLists:list(list(code))
%! )// is det.
% @tbd This does not work for the following string:
% ~~~
% "error(permission_error(delete,file,\'c:/users/quirinus/.webqr/export.svg\'),context(system:delete_file/1,\'Permission denied\'))"
% ~~~

:- meta_predicate(dcg_separated_list(//,?,?,?)).
dcg_separated_list(Sep, L) -->
  {nonvar(L)}, !,
  dcg_separated_list_nonvar(Sep, L).
dcg_separated_list(Sep, L) -->
  {var(L)}, !,
  dcg_separated_list_var(Sep, L).

:- meta_predicate(dcg_separated_list_nonvar(//,+,?,?)).
dcg_separated_list_nonvar(_Sep, [H]) --> !,
  H.
dcg_separated_list_nonvar(Sep, [H|T]) -->
  H,
  Sep,
  dcg_separated_list_nonvar(Sep, T).

:- meta_predicate(dcg_separated_list_var(//,-,?,?)).
dcg_separated_list_var(Sep, [H|T]) -->
  dcg_until([end_mode(exclusive),output_format(codes)], Sep, H),
  Sep, !,
  dcg_separated_list_var(Sep, T).
dcg_separated_list_var(_Sep, [H]) -->
  dcg_all([], H), !.


%! dcg_switch(+Value, +Maps:list)// is det.

dcg_switch(Value, Maps) -->
  dcg_switch(Value, Maps, dcg_end).

%! dcg_switch(+Value, +Map:list, +Default)// is det.

:- meta_predicate(dcg_switch(+,+,2,?,?)).
dcg_switch(Value, Map, _Default) -->
  {member(Value-Goal, Map)}, !,
  % Make sure the variables in the goal are bound outside the switch call.
  phrase(Goal).
dcg_switch(_Value, _Map, Default) -->
  % Make sure the variables in the goal are bound outside the switch call.
  phrase(Default).


%! dcg_until(:DCG_End, ?Value)// is det.
%! dcg_until(+Options:list(nvpair), :DCG_End, ?Value)// is det.
% Returns the codes that occur before `DCG_End` can be consumed.
%
% The following options are supported:
%   * =|end_mode(?EndMode:oneof([exclusive,inclusive]))|=
%     Whether the codes that satisfy the DCG rule are included in
%     (`inclusive`) or excluded from (`exclusive`, default) the results.
%   * =|output_format(?OutFormat:oneof([atom,codes]))|=
%     Whether the results should be returned in codes (`codes`, default)
%     or as an atom (`atom`).
%
% @arg Options A list of name-value pairs.
% @arg DCG_End Not an arbitrary DCG body, since disjunction
%      does not play out well.
% @arg Value Either an atom or a list of codes (see options).

:- meta_predicate(dcg_until(//,?,?,?)).
dcg_until(DCG_End, Value) -->
  dcg_until([], DCG_End, Value).

:- meta_predicate(dcg_until(+,//,?,?,?)).
dcg_until(O1, DCG_End, Out) -->
  {var(Out)}, !,
  dcg_until_(O1, DCG_End, Codes),
  {
    option(output_format(OutFormat), O1, codes),
    (
      OutFormat == atom
    ->
      atom_codes(Out, Codes)
    ;
      Out = Codes
    )
  }.
dcg_until(O1, DCG_End, In) -->
  {nonvar(In)}, !,
  {
    option(output_format(OutFormat), O1, codes),
    (
      OutFormat == atom
    ->
      atom_codes(In, Codes)
    ;
      Codes = In
    )
  },
  dcg_until_(O1, DCG_End, Codes).

:- meta_predicate(dcg_until_(+,//,?,?,?)).
dcg_until_(O1, DCG_End, EndCodes), InclusiveExclusive -->
  DCG_End, !,
  {
    option(end_mode(EndMode), O1, exclusive),
    (
      EndMode == inclusive
    ->
      InclusiveExclusive = void,
      % This returns the correct list of codes. Wow!
      phrase(DCG_End, EndCodes)
    ;
      InclusiveExclusive = DCG_End,
      EndCodes = []
    )
  }.
dcg_until_(O1, DCG_End, [H|T]) -->
  [H],
  dcg_until_(O1, DCG_End, T).

void --> [].


%! dcg_with_output_to(+Output:compound, :DCG) is det.

:- meta_predicate(dcg_with_output_to(+,//)).
dcg_with_output_to(Out, DCG):-
  once(phrase(DCG, Codes)),
  with_output_to(Out, put_codes(Codes)).

