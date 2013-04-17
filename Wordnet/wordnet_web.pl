:- module(
  wordnet_web,
  [
    antonym_web/2, % +Word:atom
                   % -AntonymMarkup:list
    gloss_web/2, % +Word:atom
                 % -GlossMarkup:list
    has_instance_web/2, % +Class:atom
                        % -InstanceMarkup:list
    holonym_web/2, % +Whole:atom
                   % -PartMarkup:list
    hypernym_web/2, % +Word:atom
                    % -HypernymMarkup:list
    instance_of_web/2, % +Instance:atom
                       % -ClassMarkup:list
    meronym_web/2, % +Part:atom
                   % -WholeMarkup:list
    n_plus_7_web/2, % +Word:atom
                    % -Markup:list
    n_plus_m_web/3, % +Word:atom
                    % +M:natnum
                    % -Markup:list
    statistics_web/1, % -Markup:list
    word_web/2 % +Word:atom
               % -Markup
  ]
).

/** <module> Wordnet Web

Wordnet Web module.

@author Wouter Beek
@version Oct2012
*/

:- use_module(wordnet(wordnet)).



antonym_web(Word, [element(p, [], [Antonym])]):-
  antonym(Word, Antonym).

%% gloss_web(+Word:atom, -Markup) is nondet.
% Returns a gloss of the given word in markup form.
% A word can have multiple glosses associated with it.

gloss_web(Word, [element(p, [], [Gloss])]):-
  gloss(Word, Gloss).

has_instance_web(Class, [element(p, [], [Instance])]):-
  has_instance(Class, Instance).

holonym_web(Whole, [element(p, [], [Part])]):-
  holonym(Whole, Part).

hypernym_web(Word, [element(p, [], [Hypernym])]):-
  hypernym(Word, Hypernym).

instance_of_web(Instance, [element(p, [], [Class])]):-
  instance_of(Instance, Class).

meronym_web(Part, [element(p, [], [Whole])]):-
  meronym(Part, Whole).

n_plus_7_web(Word, Markup):-
  n_plus_7(Word, NewWord),
  word_web(NewWord, Markup).

n_plus_m_web(Word, M, Markup):-
  n_plus_m(Word, M, NewWord),
  word_web(NewWord, Markup).

statistics_web(
  [
    element(
      table,
      [border=1, summary='This table shows Wordnet statistics.'],
      [
        element(caption, [], ['Wordnet statistics']),
        element(
          tr,
          [],
          [
            element(td, [], ['Antonym']),
            element(td, [], [AtomicNumberOfAntonyms])
          ]
        ),
        element(
          tr,
          [],
          [
            element(td, [], ['Glosses']),
            element(td, [], [AtomicNumberOfGlosses])
          ]
        ),
        element(
          tr,
          [],
          [
            element(td, [], ['Hypernyms']),
            element(td, [], [AtomicNumberOfHypernyms])
          ]
        ),
        element(
          tr,
          [],
          [
            element(td, [], ['Meronyms']),
            element(td, [], [AtomicNumberOfMeronyms])
          ]
        )
      ]
    )
  ]
):-
  count(antonym(_X1, _Y1), NumberOfAntonyms),
  atom_number(AtomicNumberOfAntonyms, NumberOfAntonyms),
  count(gloss(_X2, _Y2), NumberOfGlosses),
  atom_number(AtomicNumberOfGlosses, NumberOfGlosses),
  count(hypernym(_X3, _Y3), NumberOfHypernyms),
  atom_number(AtomicNumberOfHypernyms, NumberOfHypernyms),
  count(meronym(_X4, _Y4), NumberOfMeronyms),
  atom_number(AtomicNumberOfMeronyms, NumberOfMeronyms).

%% word_web(+Word:atom, -Markup) is det.
% Returns the markup representation for the given word's Wordnet entry.
% The markup contains the following information:
%   1. The word itself.
%   2. A numbered list of its glosses.

word_web(
  Word,
  [
    element(h1, [], [Word]),
    element(h2, [], ['Glosses']),
    Glosses,
    element(h2, [], ['Antonyms']),
    Antonyms,
    element(h2, [], ['Hypernyms']),
    Hypernyms
  ]
):-
  glosses_web_(Word, Glosses),
  antonyms_web_(Word, Antonyms),
  hypernyms_web_(Word, Hypernyms).

antonym_web_(Word, element(li, [], [Antonym])):-
  antonym(Word, Antonym).

antonyms_web_(Word, element(ol, [], Antonyms)):-
  findall(
    Antonym,
    antonym_web_(Word, Antonym),
    Antonyms
  ).

gloss_web_(Word, element(li, [], [Gloss])):-
  gloss(Word, Gloss).

glosses_web_(Word, element(ol, [], Glosses)):-
  findall(
    Gloss,
    gloss_web_(Word, Gloss),
    Glosses
  ).

hypernym_web_(Word, element(li, [], [Hypernym])):-
  hypernym(Word, Hypernym).

hypernyms_web_(Word, element(ol, [], Hypernyms)):-
  findall(
    Hypernym,
    hypernym_web_(Word, Hypernym),
    Hypernyms
  ).
