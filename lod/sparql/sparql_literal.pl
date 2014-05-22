:- module(
  sparql_literal,
  [
    'STRING_LITERAL1'//1, % ?Literal:atom
    'STRING_LITERAL2'//1, % ?Literal:atom
    'STRING_LITERAL_LONG1'//1, % ?Literal:atom
    'STRING_LITERAL_LONG2'//1 % ?Literal:atom
  ]
).

/** <module> SPARQL literal

DCGs for literal definition in SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(library(lists)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_meta)).
:- use_module(math(radix)).
:- use_module(sparql(sparql_char)).



%! 'STRING_LITERAL1'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ~~~
%
% @compat SPARQL 1.1 Query [156].

'STRING_LITERAL1'(Literal) -->
  quoted(single_quote, dcg_atom_codes('STRING_LITERAL1*', Literal)).

'STRING_LITERAL1*'([H|T]) -->
  'STRING_LITERAL1*_char'(H),
  'STRING_LITERAL1*'(T).
'STRING_LITERAL1*' --> [].

'STRING_LITERAL1*_char'(C) -->
  'ECHAR'(C),
  {\+ (
    dec_hex(C, Hex),
    member(Hex, ['27','5C','A','D'])
  )}.


%! 'STRING_LITERAL2'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
% ~~~
%
% @compat SPARQL 1.1 Query [157].

'STRING_LITERAL2'(Literal) -->
  quoted(double_quote, dcg_atom_codes('STRING_LITERAL2*', Literal)).

'STRING_LITERAL2*'([H|T]) -->
  'STRING_LITERAL2*_char'(H),
  'STRING_LITERAL2*'(T).
'STRING_LITERAL2*' --> [].

'STRING_LITERAL2*_char'(C) -->
  'ECHAR'(C),
  {\+ (
    dec_hex(C, Hex),
    member(Hex, ['22','5C','A','D'])
  )}.


%! 'STRING_LITERAL_LONG1'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL_LONG1 ::= "'''"
%                          ( ( "'" | "''" )? ( [^'\] | ECHAR ) )*
%                          "'''"
% ~~~
%
% @compat SPARQL 1.1 Update [158].

'STRING_LITERAL_LONG1'(Literal) -->
  'STRING_LITERAL_LONG'(single_quote, Literal) -->


%! 'STRING_LITERAL_LONG2'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL_LONG2 ::= '"""'
%                          ( ( '"' | '""' )? ( [^"\] | ECHAR ) )*
%                          '"""'
% ~~~
%
% @compat SPARQL 1.1 Update [159].

'STRING_LITERAL_LONG2'(Literal) -->
  'STRING_LITERAL_LONG'(double_quote, Literal) -->



% HELPER PREDICATES %

'STRING_LITERAL_LONG'(Quote, Literal) -->
  quoted(
    triple_quote(Quote),
    dcg_atom_codes('STRING_LITERAL_LONG*'(Quote), Literal)
  ).

'STRING_LITERAL_LONG*'(Quote, [H|T]) -->
  'STRING_LITERAL_LONG*_char'(Quote, H),
  'STRING_LITERAL_LONG*'(Quote, T).
'STRING_LITERAL_LONG*'(_, []) --> [].

'STRING_LITERAL_LONG*_char'(Quote, C) -->
  (Quote, Quote ; Quote ; ``),
  'ECHAR'(C),
  {\+ member(C, [39,92])}. % Apostrophe, backslash.

