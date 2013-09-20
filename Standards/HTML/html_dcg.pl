:- module(
  html_dcg,
  [
    html_element//1, % +ElementName:atom
    html_element//2, % +ElementName:atom
                     % :Content:dcg
    html_entity//0,
    html_entity//1, % +EntityName:atom
    html_graph//0,
    html_graph//1, % ?Code:code
    html_string//0,
    html_string//1, % +String:or([atom,list(code)])
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
:- use_module(dcg(dcg_multi)).

:- meta_predicate(html_element(+,//,?,?)).

:- discontiguous(html_punctuation//0).
:- discontiguous(html_punctuation//1).



html_entity -->
  "&",
  word(_),
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
  atom(ElementName),
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

html_graph(C) -->
  dcg_white(C).
html_graph(C) -->
  alpha_numeric(C).
html_graph(C) -->
  html_punctuation(C).

html_punctuation --> asterisk.
html_punctuation(C) --> asterisk(C).
html_punctuation --> at_sign.
html_punctuation(C) --> at_sign(C).
html_punctuation --> bracket.
html_punctuation(C) --> bracket(C).
html_punctuation --> caret.
html_punctuation(C) --> caret(C).
html_punctuation --> colon.
html_punctuation(D) --> colon(D).
html_punctuation --> comma.
html_punctuation(C) --> comma(C).
html_punctuation --> dollar_sign.
html_punctuation(C) --> dollar_sign(C).
html_punctuation --> dot.
html_punctuation(C) --> dot(C).
html_punctuation --> equals_sign.
html_punctuation(C) --> equals_sign(C).
html_punctuation --> exclamation_mark.
html_punctuation(C) --> exclamation_mark(C).
html_punctuation --> grave_accent.
html_punctuation(C) --> grave_accent(C).
html_punctuation --> hyphen_minus.
html_punctuation(C) --> hyphen_minus(C).
html_punctuation --> number_sign.
html_punctuation(C) --> number_sign(C).
html_punctuation --> percent_sign.
html_punctuation(C) --> percent_sign(C).
html_punctuation --> plus_sign.
html_punctuation(C) --> plus_sign(C).
html_punctuation --> question_mark.
html_punctuation(C) --> question_mark(C).
html_punctuation --> semi_colon.
html_punctuation(C) --> semi_colon(C).
html_punctuation --> slash.
html_punctuation(C) --> slash(C).
html_punctuation --> tilde.
html_punctuation(C) --> tilde(C).
html_punctuation --> underscore.
html_punctuation(C) --> underscore(C).
html_punctuation --> vertical_bar.
html_punctuation(C) --> vertical_bar(C).
% Now come the translations for escaped characters.
html_punctuation(34) --> "&quot;". % Double quotes (").
html_punctuation(60) --> "&gt;".   % Greater than (>).
html_punctuation(62) --> "&lt;".   % Smaller than (<).
html_punctuation(68) --> "&amp;".  % Ampersand (&).

%! html_string//
% A _string_ is any collection of printable characters, including all spaces.

html_string --> html_graph, html_string.
html_string --> html_graph.

html_string(Codes) -->
  {is_list(Codes)}, !,
  dcg_multi(html_graph, _Rep, Codes, []).
html_string(Text) -->
  {atom_codes(Text, Codes)},
  html_string(Codes).

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

