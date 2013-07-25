:- module(
  rfc5646,
  [
    rfc5646_language_tag//2 % -Tree:compound
                            % ?LanguageTag:atom
  ]
).

/** <module> RFC5646

Support for RFC 5646 on tags for identifying languages.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(lang('iso639-1')). % Used in meta-call.
:- use_module(lang('iso639-2')). % Used in meta-call.



rfc5646_language_tag(
  rfc5646_language_tag(primary(Primary),secondary(Secondary)),
  LanguageTag
) -->
  {nonvar(LanguageTag)}, !,
  {atomic_list_concat([Primary,Secondary], '-', LanguageTag)},
  rfc5646_language_tag(Primary),
  blank,
  rfc5646_language_tag(Secondary).
rfc5646_language_tag(
  rfc5646_language_tag(primary(Primary),secondary(Secondary)),
  LanguageTag
) -->
  rfc5646_language_tag(Primary),
  blank,
  rfc5646_language_tag(Secondary),
  {atomic_list_concat([Primary,Secondary], '-', LanguageTag)}.

rfc5646_language_tag(Tag) -->
  dcg_word(Word),
  {atom_length(Word, Length)},
  dcg_switch(
    Length,
    % Length-2 codes must be ISO 639-1.
    % Length-3 codes must be ISO 639-2.
    [2-'iso639-1'(Tag), 3-'iso639-2'(Tag)]
  ).

