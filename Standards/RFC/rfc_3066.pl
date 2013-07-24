:- module(
  rdf_3066,
  [
    language_tag/4 % +Options:list(nvpair)
                   % -Primary:atom
                   % -Secondary:list(atom)
                   % +C:difflist
  ]
).

/** <module> RFC 3066

Language tag parsing.

@author Wouter Beek
@version 2013/02, 2013/06-2013/07
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(iso(iso_639_1)).
:- use_module(iso(iso_639_2)).



language_tag(Primary, Secondary) -->
  language_tag(Primary),
  blank,
  language_tag(Secondary).

language_tag(Tag) -->
  dcg_word(Word),
  {atom_length(Word, Length)},
  dcg_switch(
    Length,
    % Length-2 codes must be ISO 639-1.
    % Length-3 codes must be ISO 639-2.
    [2-iso_639_1(Tag), 3-iso_639_2(Tag)]
  ).

