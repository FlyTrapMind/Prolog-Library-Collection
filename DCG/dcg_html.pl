:- module(
  dcg_html,
  [
    html_convert//1, % -Converted:list(code)
    html_element//3, % ?Name:atom
                     % ?Attrs:list(nvpair)
                     % ?Content:dom
    html_style//1 % ?NVPairs:list(nvpair)
  ]
).

/** <module> DCG_HTML

Convert HTML strings.

@author Wouter Beek
@version 2013/06-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(html(html)).
:- use_module(library(apply)).



html_convert([]) --> [].
html_convert([H|T]) -->
  html_char(H),
  html_convert(T).

html_char('>') --> "&#62;".
html_char('<') --> "&#60;".
html_char(X) --> [X].

html_element(Name, MatchAttrs, Content) -->
  {var(MatchAttrs)},
  html_element(Name, [], Content).
html_element(Name, MatchAttrs, Content) -->
  {is_list(MatchAttrs)},
  [element(Name, Attrs, Content)],
  {maplist(html_attribute(Attrs), MatchAttrs)}.
html_element(Name, MatchAttr, Content) -->
  {\+ is_list(MatchAttr)},
  html_element(Name, [MatchAttr], Content).

html_style([]) --> [].
html_style([NVPair|NVPairs]) -->
  {var(NVPair)}, !,
  html_style_word(Name1),
  colon, (space ; ""),
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
  colon, (space ; ""),
  html_style_word(Value2),
  semi_colon,
  html_style(NVPairs).

html_style_word([H|T]) -->
  (letter(H) ; underscore(H)),
  html_style_word(T).
html_style_word([]) --> [].

