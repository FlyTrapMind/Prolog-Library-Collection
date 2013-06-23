:- module(
  dcg_html,
  [
    html_convert//1 % -Converted:list(code)
  ]
).

/** <module> DCG_HTML

Convert HTML strings.

@author Wouter Beek
@version 2013/06
*/



html_convert([]) --> [].
html_convert([H|T]) -->
  html_char(H),
  html_convert(T).

html_char('>') --> "&#62;".
html_char('<') --> "&#60;".
html_char(X) --> [X].
