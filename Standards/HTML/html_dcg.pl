:- module(
  html_dcg,
  [
    html_dcg//1, % +Content:list(or([atom,compound,list(code)]))
    html_element//2, % +ElementName:atom
                     % +Attributes:list(nvpair)
    html_element//3, % +ElementName:atom
                     % +Attributes:list(nvpair)
                     % :Content:dcg
    html_entity//0,
    html_entity//1, % +EntityName:atom
    html_graph//0,
    html_graph//1, % ?Code:code
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
:- use_module(dcg(dcg_multi)).

:- meta_predicate(html_element(+,+,//,?,?)).

:- discontiguous(html_punctuation//0).
:- discontiguous(html_punctuation//1).



%! html_attribute//
% Used for *checking* GraphViz HTML-like labels.

html_attribute -->
  word(_),
  "=",
  double_quote,
  word(_),
  double_quote.

%! html_attribute(+Name:atom, +Value:atom)//
% Used for *generating* GraphViz HTML-like labels.

html_attribute(N, V) -->
  atom(N),
  "=",
  double_quote,
  atom(V),
  double_quote.

html_attributes --> [].
html_attributes -->
  " ",
  html_attribute,
  html_attributes.

html_attributes([]) --> [].
html_attributes([N=V|T]) -->
  " ",
  html_attribute(N, V),
  html_attributes(T).

html_attributes_(Attrs) -->
  {var(Attrs)}, !,
  html_attributes.
html_attributes_(Attrs) -->
  html_attributes(Attrs).

%! html_dcg(+Content:list(or([atom,compound,list(code)])))//

% Done.
html_dcg([]) --> !, [].
% Tag with no content.
html_dcg([tag(Name,Attrs)|T]) --> !,
  html_element(Name, Attrs),
  html_dcg(T).
% Tab with content.
html_dcg([tag(Name,Attrs,Contents)|T]) --> !,
  html_element(Name, Attrs, html_dcg(Contents)),
  html_dcg(T).
% Codes list.
html_dcg([H|T]) -->
  html_string(H), !,
  html_dcg(T).

html_entity -->
  "&",
  word(_),
  ";".

html_entity(EntityName) -->
  "&",
  atom(EntityName),
  ";".

html_element(ElementName, Attrs) -->
  "<",
  atom(ElementName),
  html_attributes_(Attrs),
  "/>".

html_element(ElementName, Attrs, Content) -->
  % Opening tab.
  "<",
  atom(ElementName),
  html_attributes_(Attrs),
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
html_punctuation(60) --> "&lt;".   % Smaller than (<).
html_punctuation(62) --> "&gt;".   % Greater than (>).
html_punctuation(68) --> "&amp;".  % Ampersand (&).

%! html_string//
% A _string_ is any collection of printable characters, including all spaces.

html_string -->
  html_graph,
  html_string.
html_string -->
  html_graph.

html_string([]) --> [].
html_string([H|T]) -->
  html_graph(H),
  html_string(T).

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

