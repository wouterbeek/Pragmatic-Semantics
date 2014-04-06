:- module(
  wordnet,
  [
    antonym/2, % ?Word:atom
               % ?Antonym:atom
    gloss/2, % +Word:atom
             % -Gloss:atom
    has_instance/2, % ?Class:atom
                    % ?Instance:atom
    holonym/2, % ?Whole:atom
               % ?Part:atom
    hypernym/2, % ?Word:atom
                % ?Hypernym:atom
    instance_of/2, % ?Instance:atom
                   % ?Class:atom
    meronym/2, % ?Part:atom
               % ?Whole:atom
    n_plus_7/2, % +Word:atom
                % -NewWord:atom
    n_plus_m/3, % +Word:atom
                % +M:natnum
                % -NewWord:atom
    word/1 % ?Word:atom
  ]
).

/** <module> Wordnet

Wordnet API.

@author Wouter Beek
@version 2012/10, 2014/03
*/

:- use_remote_module(os(file_ext)).

:- dynamic(ant/4).
:- dynamic(g/2).
:- dynamic(hyp/2).
:- dynamic(ins/2).
:- dynamic(mm/2).
:- dynamic(s/6).
:- dynamic(wordnet_loaded/0).



%! antonym(+Word:atom, -Antonym:atom) is nondet.
% Antonyms of words.
% A word can have multiple antonyms.

antonym(Word, Antonym):-
  word(SynsetID, WordNumber, Word),
  ant(SynsetID, WordNumber, AntonymSynsetID, AntonymNumber),
  word(AntonymSynsetID, AntonymNumber, Antonym).


%! gloss(+Word:atom, -Gloss) is nondet.
% Glosses of words.
% A word can have multiple glosses.

gloss(Word, Gloss):-
  load,
  word(SynsetID, Word),
  g(SynsetID, Gloss).


has_instance(Class, Instance):-
  word(SetSynsetID, Class),
  ins(SetSynsetID, InstanceSynsetID),
  word(InstanceSynsetID, Instance).


%! holonym(?While:atom, ?Part:atom) is nondet.
% Holonymy relations, i.e., inverted semantic part-of or member-of relations.
%
%   1. =X= is a holonym of =Y= if =Y=s are parts of =X=s.
%   2. =X= is a holonym of =Y= if =Y=s are members of =X=s.
%
% @see The oppposite holomymy/2.

holonym(Whole, Part):-
  meronym(Part, Whole).


%! hypernym(+Word:atom, -Hypernym:atom) is nondet.
% Hypernyms, i.e., word that share a type-of relationship.
% A word can have multiple hypernyms.

hypernym(Word, Hypernym):-
  load,
  word(SynsetID, Word),
  hyp(SynsetID, HypernymSynsetID),
  word(HypernymSynsetID, Hypernym).


instance_of(Instance, Class):-
  has_instance(Class, Instance).


load:-
  wordnet_loaded, !.
load:-
  absolute_file_name(data_wordnet(.), WordnetDirectory),
  path_walk_tree(WordnetDirectory, '.*.pl$', WordnetFiles),
  maplist(ensure_loaded, WordnetFiles),
  assert(wordnet_loaded).


%! meronym(?Part:atom, ?Whole:atom) is nondet.
% Meronym relations, i.e., semantic part-of or member-of relations.
%
%   1. =X= is a meronym of =Y= if =X=s are parts of =Y=(s).
%   2. =X= is a meronym of =Y= if =X=s are members of =Y=(s). 
%
% @see The oppposite holomymy/2.

meronym(Part, Whole):-
  word(PartSynsetID, Part),
  mm(PartSynsetID, WholeSynsetID),
  word(WholeSynsetID, Whole).


%! n_plus_7(+Word:atom, -NewWord:atom) is det.
% Implementation of the Oulipo method commonly called *|N + 7|*.
%
% @see n_plus_m/3.

n_plus_7(Word, NewWord):-
  n_plus_m(Word, 7, NewWord).


%! n_plus_m(+Word:atom, +M:nonneg, -NewWord:atom) is det.
% Returns the word that is stored in a codeline with the given
% natural number relative to the codeline the given word is
% stored at.

n_plus_m(Word, M, NewWord):-
  load,
  % Retrieve a reference to the clause for the given word.
  clause(wordnet:s(_, _, Word, _, _, _), _Body, Ref),
  % Retrieve the index of the clause in the code.
  nth_clause(_Pred, Index, Ref),
  % Increment the index.
  NewIndex is Index + M,
  % Retrieve a reference for the clause that has the new index in the code.
  nth_clause(wordnet:s(_,_,_,_,_,_), NewIndex, NewRef),
  % Retrieve the word that is described in this clause.
  clause(wordnet:s(_,_,NewWord,_,_,_), _NewBody, NewRef),
  !.

%% word(+Word:atom) is semidet.
%% word(-Word:atom) is nondet.
% Words.

word(Word):-
  word(_SynsetID, Word).

word(SynsetID, Word):-
  word(SynsetID, _WordNumber, Word).

word(SynsetID, WordNumber, Word):-
  load,
  s(SynsetID, WordNumber, Word, _SynsetType, _SenseNumber, _TagCount).

