:- module(
  wordnet_web,
  [
    antonyms//1, % +Word:atom
    glosses//1, % +Word:atom
    has_instance//1, % +Class:atom
    hypernyms//1, % +Word:atom
    instance_of//1, % +Instance:atom
    meronyms//1, % +Part:atom
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
@tbd Implement holonym//1
@version 2012/11, 2014/03, 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).

:- use_module(datasets(wordnet)).

:- use_module(plHtml(html_table)).



%! antonyms(+Word:atom)// is det.
% Generates an HTML description for the antonyms of the given word.

antonyms(Word) -->
  {findall(A, antonym(Word, A), As)},
  html(
    \html_table(
      html(['Antonyms of ',Word,'.']),
      [['Antonym']|As],
      [header_row(true)]
    )
  ).


%! glosses(+Word:atom)// is det.
% Generates an HTML description for the glosses of the given word.

glosses(Word) -->
  {findall(G, gloss(Word, G), Gs)},
  html(
    \html_table(
      html(['Glosses of ',Word,'.']),
      [['Gloss']|Gs],
      [header_row(true)]
    )
  ).


%! has_instance(+Class:atom)// is det.

has_instance(Class) -->
  {findall(
    Instance,
    has_instance(Class, Instance),
    Instances
  )},
  html(
    \html_table(
      html(['Instances of ',Class,'.']),
      [['Instance']|Instances],
      [header_row(true)]
    )
  ).


%! hypernyms(+Word:atom)// is det.
% Generates an HTML description for the hypernyms of the given word.

hypernyms(Word) -->
  {findall(H, hypernym(Word, H), Hs)},
  html(
    \html_table(
      html(['Hypernyms of ',Word,'.']),
      [['Hypernym']|Hs],
      [header_row(true)]
    )
  ).


%! instance_of(+Instance:atom)// is det.
% Generates an HTML overview of the classes of the given instance.

instance_of(Instance) -->
  {findall(Class, instance_of(Instance, Class), Classes)},
  html(
    \html_table(
      html(['Classes of ',Instance,'.']),
      [['Class']|Classes],
      [header_row(true)]
    )
  ).


%! meronyms(+Part:atom)// is det.
% Generates an HTML overview of the meronyms of the given word.

meronyms(Part) -->
  {findall(Whole, meronym(Part, Whole), Wholes)},
  html(
    \html_table(
      html(['Meronyms of ',Part,'.']),
      [['Meronym']|Wholes],
      [header_row(true)]
    )
  ).


n_plus_7(Word1) -->
  {n_plus_7(Word1, Word2)},
  html([
    h2([Word1,' plus 7']),
    \word(Word2)
  ]).


%! n_plus_m_web(+Word:atom, +M:nonneg)// is det.
% Generates an HTML overview of the word
% which occurs _M_ positions later in the dictionary.

n_plus_m(Word1, M) -->
  {n_plus_m(Word1, M, Word2)},
  html([
    h2([Word1,'plus ',M]),
    \word(Word2)
  ]).


%! statistics// is det.
% Generates an HTML table of Wordnet statistics.

statistics -->
  {
    aggregate_all(count, antonym(_X1, _Y1),  NumberOfAntonyms ),
    aggregate_all(count, gloss(_X2, _Y2),    NumberOfGlosses  ),
    aggregate_all(count, hypernym(_X3, _Y3), NumberOfHypernyms),
    aggregate_all(count, meronym(_X4, _Y4),  NumberOfMeronyms )
  },
  html(
    \html_table(
      html('Overview of Wordnet statistics.'),
      [
        ['Type',     'Number of words'],
        ['Antonym',  NumberOfAntonyms ],
        ['Glosses',  NumberOfGlosses  ],
        ['Hypernyms',NumberOfHypernyms],
        ['Meronyms', NumberOfMeronyms ]
      ],
      [header_row(true)]
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

