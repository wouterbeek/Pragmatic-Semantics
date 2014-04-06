:- module(
  wordnet_web,
  [
    antonym//1, % +Word:atom
    gloss//1, % +Word:atom
    has_instance//1, % +Class:atom
    holonym//1, % +Whole:atom
    hypernym//1, % +Word:atom
    instance_of//1, % +Instance:atom
    meronym//1, % +Part:atom
    n_plus_7//1, % +Word:atom
    n_plus_m//2, % +Word:atom
                 % +M:nonneg
    statistics//0,
    word//1 % +Word:atom
  ]
).

/** <module> Wordnet Web

Web-interface for Wordnet.

@author Wouter Beek
@version 2012/11, 2014/03
*/

:- use_module(datasets(wordnet)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).



%! antonyms(+Word:atom)// is det.
% Generates an HTML description for the antonyms of the given word.

antonyms(Word) -->
  {findall(A, antonym(Word, A), As)},
  html(
    \html_table(
      [header_row(true)],
      html(['Antonyms of ',Word,'.']),
      [['Antonym']|As]
    )
  ).


%! glosses(+Word:atom)// is det.
% Generates an HTML description for the glosses of the given word.

glosses(Word) -->
  {findall(G, gloss(Word, G), Gs)},
  html(
    \html_table(
      [header_row(true)],
      html(['Glosses of ',Word,'.']),
      [['Gloss']|Gs]
    )
  ).


%! has_instance(+Class:atom)// is det.

has_instance(Class) -->
  {findall(Instance, has_instance(Class, Instance), Instances)},
  html(
    \html_table(
      [header_row(true)],
      html(['Instances of ',Word,'.']),
      [['Instance']|Instances]
    )
  ).


%! hypernyms(+Word:atom)// is det.
% Generates an HTML description for the hypernyms of the given word.

hypernyms(Word) -->
  {findall(H, hypernym(Word, H), Hs)},
  html(
    \html_table(
      [header_row(true)],
      html(['Hypernyms of ',Word,'.']),
      [['Hypernym']|Hs]
    )
  ).


%! instance_of(+Instance:atom)// is det.
% Generates an HTML overview of the classes of the given instance.

instance_of(Instance) -->
  {findall(Class, instance_of(Instance, Class), Classes)},
  html(
    \html_table(
      [header_row(true)],
      html(['Classes of ',Word,'.']),
      [['Class']|Classes]
    )
  ).


%! meronyms(+Part:atom)// is det.
% Generates an HTML overview of the meronyms of the given word.

meronyms(Part) -->
  {findall(Whole, meronym(Part, Whole), Wholes)},
  html(
    \html_table(
      [header_row(true)],
      html(['Meronyms of ',Part,'.']),
      [['Meronym']|Wholes]
    )
  ).


n_plus_7(W1) -->
  {n_plus_7(W1, W2)},
  html([
    h2([Word,' plus 7']),
    \word(W2)
  ]).


%! n_plus_m_web(+Word:atom, +M:nonneg)// is det.
% Generates an HTML overview of the word
% which occurs _M_ positions later in the dictionary.

n_plus_m(W1, M) -->
  {n_plus_m(W1, M, W2)},
  html([
    h2([Word,'plus ',M]),
    \word(W2)
  ]).


%! statistics// is det.
% Generates an HTML table of Wordnet statistics.

statistics -->
  {
    aggregate_all(count, antonym(_X1, _Y1),  NumberOfAntonyms ),
    aggregate_all(count, gloss(_X2, _Y2),    NumberOfGlosses  ),
    aggregate_all(count, hypernym(_X3, _Y3), NumberOfHypernyms),
    aggregate_all(count, meronym(_X4, _Y4),  NumberOfMeronyms ),
  },
  html(
    \html_table(
      [header_row(true)],
      html('Overview of Wordnet statistics.'),
      [
        ['Type',     'Number of words'],
        ['Antonym',  NumberOfAntonyms ],
        ['Glosses',  NumberOfGlosses  ],
        ['Hypernyms',NumberOfHypernyms],
        ['Meronyms', NumberOfMeronyms ]
      ]
    )
  ).


%% word(+Word:atom)// is det.
% Generates an HTML description for the given word.

word(Word) -->
  html([
    h1('Word'),
    \antonyms(Word),
    \glosses(Word),
    \hypernyms(Word)
  ]).

