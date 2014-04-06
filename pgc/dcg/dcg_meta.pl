:- module(
  dcg_meta,
  [
    ';'//2, % :Dcg1
            % :Dcg2
    ';'//3, % :Dcg1
            % :Dcg2
            % :Dcg3
    dcg_apply//2, % :Dcg
                  % +Arguments:list
    dcg_call//1,
    dcg_call//2,
    dcg_call//3,
    dcg_call//4,
    dcg_call//5,
    dcg_calls//2, % :Dcgs:list
                  % :Separator
    dcg_maplist//2, % :Dcg
                    % +Arguments:list
    dcg_maplist//3, % :Dcg
                    % +Arguments1:list
                    % +Arguments2:list
    dcg_nth0_call//3, % :Goal
                      % +Index:nonneg
                      % +Argument
    dcg_nth0_call//4 % +Options:list(nvpair)
                     % :Goal
                     % +Index:nonneg
                     % +Argument
  ]
).

/** <module> DCG meta

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12, 2014/02-2014/03
*/

:- use_remote_module(generics(list_ext)).
:- use_module(library(lists)).

:- meta_predicate(';'(2,2,?,?)).
:- meta_predicate(';'(2,2,2,?,?)).
:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_call(2,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_calls(+,//,?,?)).
:- meta_predicate(dcg_maplist(3,+,?,?)).
:- meta_predicate(dcg_maplist(4,+,+,?,?)).
:- meta_predicate(dcg_nth0_call(3,+,+,?,?)).
:- meta_predicate(dcg_nth0_call(+,3,+,+,?,?)).



';'(A, _B, C, D):-
  dcg_call(A, C, D).
';'(_A, B, C, D):-
  dcg_call(B, C, D).


';'(A, _B, _C, D, E):-
  dcg_call(A, D, E).
';'(_A, B, _C, D, E):-
  dcg_call(B, D, E).
';'(_A, _B, C, D, E):-
  dcg_call(C, D, E).


dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).


%! dcg_call(:Dcg)//
% Included for consistency with dcg_call//[1,2,3,4].
% @see Same effect as phrase/3.

dcg_call(Dcg, X, Y):-
  call(Dcg, X, Y).


dcg_call(Dcg, A1, X, Y):-
  call(Dcg, A1, X, Y).


dcg_call(Dcg, A1, A2, X, Y):-
  call(Dcg, A1, A2, X, Y).


dcg_call(Dcg, A1, A2, A3, X, Y):-
  call(Dcg, A1, A2, A3, X, Y).


dcg_call(Dcg, A1, A2, A3, A4, X, Y):-
  call(Dcg, A1, A2, A3, A4, X, Y).


dcg_calls(_:[], _Separator) --> [].
dcg_calls(Mod:DCG_Rules, Separator) -->
  {DCG_Rules = [H|T]},
  dcg_call(Mod:H),
  (
    {T == []}, !
  ;
    dcg_call(Separator)
  ),
  dcg_calls(Mod:T, Separator).


%! dcg_maplist(:Dcg, +Args:list)// .

dcg_maplist(_, []) --> [].
dcg_maplist(Dcg, [H|T]) -->
  dcg_apply(Dcg, H),
  dcg_maplist(Dcg, T).


%! dcg_maplist(:Dcg, +Args1:list, +Args2:list)// .

dcg_maplist(_, [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2]) -->
  dcg_call(Dcg, H1, H2),
  dcg_maplist(Dcg, T1, T2).


%! dcg_nth0_call(:Dcg, +Index:nonneg, +Element)// .
%! dcg_nth0_call(+Options:list(nvpair), :Dcg, +Index:nonneg, +Element)// .
% The following options are supported:
%   * =|minus(+UseMinus:boolean)|=
%     When `true` (default `false`), uses nth0_minus/4
%     instead of nth0/4. See module [list_ext].
%
% @see This meta-DCG is based on nth0_call/[3,4] in module [meta_ext].

dcg_nth0_call(Dcg, I, X) -->
  dcg_nth0_call([], Dcg, I, X).

dcg_nth0_call(O1, Dcg1, I, X) -->
  {
    strip_module(Dcg1, Mod, Dcg2),
    Dcg2 =.. [Pred|Args1],

    % Insert the extra argument.
    (
      option(minus(true), O1, false)
    ->
      nth0_minus(I, Args2, X, Args1)
    ;
      nth0(I, Args2, X, Args1)
    )
  },
  dcg_apply(Mod:Pred, Args2).

