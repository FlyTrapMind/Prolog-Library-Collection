:- module(
  html_dcg,
  [
    html_element//1, % +ElementName:atom
    html_element//2, % +ElementName:atom
                     % :Content:dcg
    html_entity//0,
    html_entity//1, % +EntityName:atom
    html_graph//0,
    html_string//0,
    html_style//1 % ?NVPairs:list(nvpair)
  ]
).

/** <module> HTML_DCG

DCG rules for HTML expressions.

@author Wouter Beek
@version 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- meta_predicate(html_element(+,//,?,?)).



html_entity -->
  "&",
  html_string,
  ";".

html_entity(EntityName) -->
  "&",
  atom(EntityName),
  ";".

html_element(ElementName) -->
  "<",
  atom(ElementName),
  "/>".

html_element(ElementName, Content) -->
  % Opening tab.
  "<",
  atom(ElementName),
  ">",
  
  % Content.
  dcg_call(Content),
  
  % Closing tab.
  "</",
  ElementName,
  ">".

%! html_graph//
% HTML reserves the following ASCII characters:
%   * Ampersand
%   * Apostrophe
%   * Greater-than
%   * Less-than
%   * Quotation mark

html_graph -->
  dcg_white.
html_graph -->
  alpha_numeric.
html_graph -->
  html_punctuation.

html_punctuation --> asterisk.
html_punctuation --> at_sign.
html_punctuation --> bracket.
html_punctuation --> caret.
html_punctuation --> colon.
html_punctuation --> comma.
html_punctuation --> dollar_sign.
html_punctuation --> dot.
html_punctuation --> equals_sign.
html_punctuation --> exclamation_mark.
html_punctuation --> grave_accent.
html_punctuation --> hyphen_minus.
html_punctuation --> number_sign.
html_punctuation --> percent_sign.
html_punctuation --> plus_sign.
html_punctuation --> question_mark.
html_punctuation --> semi_colon.
html_punctuation --> slash.
html_punctuation --> tilde.
html_punctuation --> underscore.
html_punctuation --> vertical_bar.

%! html_string//
% A _string_ is any collection of printable characters, including all spaces.

html_string -->
  html_graph,
  html_string.
html_string -->
  html_graph.

html_style([]) --> [].
html_style([NVPair|NVPairs]) -->
  {var(NVPair)}, !,
  html_style_word(Name1),
  colon,
  (space ; void),
  html_style_word(Value1),
  semi_colon,
  {
    atom_codes(Name2, Name1),
    atom_codes(Value2, Value1),
    NVPair =.. [Name2, Value2]
  },
  html_style(NVPairs).
html_style([NVPair|NVPairs]) -->
  {nonvar(NVPair)}, !,
  {
    NVPair =.. [Name1, Value1],
    atom_codes(Name1, Name2),
    atom_codes(Value1, Value2)
  },
  html_style_word(Name2),
  colon,
  (space ; void),
  html_style_word(Value2),
  semi_colon,
  html_style(NVPairs).

html_style_word([H|T]) -->
  (letter(H) ; underscore(H)),
  html_style_word(T).
html_style_word([]) --> [].

