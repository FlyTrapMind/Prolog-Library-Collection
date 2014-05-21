:- module(
  sparql_literal,
  [
    'STRING_LITERAL1'//1, % ?Codes:list(code)
    'STRING_LITERAL2'//1, % ?Codes:list(code)
    'STRING_LITERAL_LONG1'//1, % ?Codes:list(code)
    'STRING_LITERAL_LONG2'//1 % ?Codes:list(code)
  ]
).

/** <module> SPARQL literal

DCGs for literal definition in SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_content)).



%! 'STRING_LITERAL1'(?Codes:list(code))// .
% ~~~{.ebnf}
% STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ~~~
%
% @compat SPARQL 1.1 Query [156].

'STRING_LITERAL1'(L) -->
  quoted(single_quote, 'STRING_LITERAL1_1*'(L)).

'STRING_LITERAL1_1*'([H|T]) -->
  'STRING_LITERAL1_1*_char'(H),
  'STRING_LITERAL1_1*'(T).
'STRING_LITERAL1_1*' --> [].

'STRING_LITERAL1_1*_char'(C) --> hex_code('27', C), !, {fail}.
'STRING_LITERAL1_1*_char'(C) --> hex_code('5C', C), !, {fail}.
'STRING_LITERAL1_1*_char'(C) --> hex_code('A', C), !, {fail}.
'STRING_LITERAL1_1*_char'(C) --> hex_code('D', C), !, {fail}.
'STRING_LITERAL1_1*_char'(C) --> 'ECHAR'(C).


%! 'STRING_LITERAL
[157]  	STRING_LITERAL2	  ::=  	'"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
