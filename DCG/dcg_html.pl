:- module(
  dcg_html,
  [
    dcg_element//3, % ?Name:atom
                    % ?Attrs:list(nvpair)
                    % ?Content:dom
    html_convert//1 % -Converted:list(code)
  ]
).

/** <module> DCG_HTML

Convert HTML strings.

@author Wouter Beek
@version 2013/06
*/



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

html_convert([]) --> [].
html_convert([H|T]) -->
  html_char(H),
  html_convert(T).

html_char('>') --> "&#62;".
html_char('<') --> "&#60;".
html_char(X) --> [X].
