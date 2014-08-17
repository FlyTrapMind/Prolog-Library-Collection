:- module(
  sparql_sep,
  [
    comma_separator//0,
    comma_separated//1, % :Dcg
    ws+//0,
    ws*//0
  ]
).

/** <module> SPARQL separator

Separators that often occur in the SPARQL grammar.

@author Wouter Beek
@version 2014/08
*/

:- use_module(sparql(sparql_char)).



%! comma_separator// is det.

comma_separator -->
  ws*, ",", ws+.



%! comma_separated(:Dcg)// is det.

comma_separated(Dcg) -->
  Dcg.
comma_separated(Dcg) -->
  Dcg, comma_separator,
  comma_separated(Dcg).



semicolon_separator -->
  ws*, ";", ws+.



ws+ --> 'WS'.
ws+ --> 'WS', ws+.



ws* --> [].
ws* --> 'WS', ws*.
