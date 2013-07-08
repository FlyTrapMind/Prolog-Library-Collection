:- module(
  dcg_html,
  [
    dcg_element//3, % ?Name:atom
                    % ?Attrs:list(nvpair)
                    % ?Content:dom
    dcg_style//1 % ?NVPairs:list(nvpair)
    html_convert//1 % -Converted:list(code)
  ]
).

/** <module> DCG_HTML

Convert HTML strings.

@author Wouter Beek
@version 2013/06-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).



dcg_element(Name, MatchAttrs, Content) -->
  {var(MatchAttrs)},
  dcg_element(Name, [], Content).
dcg_element(Name, MatchAttrs, Content) -->
  {is_list(MatchAttrs)},
  [element(Name, Attrs, Content)],
  {maplist(html_attribute(Attrs), MatchAttrs)}.
dcg_element(Name, MatchAttr, Content) -->
  {\+ is_list(MatchAttr)},
  dcg_element(Name, [MatchAttr], Content).

dcg_style([]) --> [].
dcg_style([NVPair|NVPairs]) -->
  {var(NVPair)}, !,
  dcg_style_word(Name1),
  colon, (space ; ""),
  dcg_style_word(Value1),
  semi_colon,
  {
    atom_codes(Name2, Name1),
    atom_codes(Value2, Value1),
    NVPair =.. [Name2, Value2]
  },
  dcg_style(NVPairs).
dcg_style([NVPair|NVPairs]) -->
  {nonvar(NVPair)}, !,
  {
    NVPair =.. [Name1, Value1],
    atom_codes(Name1, Name2),
    atom_codes(Value1, Value2)
  },
  dcg_style_word(Name2),
  colon, (space ; ""),
  dcg_style_word(Value2),
  semi_colon,
  dcg_style(NVPairs).

dcg_style_word([H|T]]) -->
  (letter(H) ; underscore(H)),
  dcg_style_word(T]).
dcg_style_word([]) --> [].

html_convert([]) --> [].
html_convert([H|T]) -->
  html_char(H),
  html_convert(T).

html_char('>') --> "&#62;".
html_char('<') --> "&#60;".
html_char(X) --> [X].

