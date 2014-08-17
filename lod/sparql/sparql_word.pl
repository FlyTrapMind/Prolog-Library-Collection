:- module(
  sparql_word,
  [
    'LANGTAG'//1, % ?LanguageTag:list(atom)
    'NIL'//0,
  ]
).

/** <module> SPARQL word

DCGs for word definitions in SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(library(lists)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(sparql(sparql_char)).



%! 'LANGTAG'(?LanguageTag:list(atom))// .
% ~~~{.ebnf}
% LANGTAG ::= '@' [a-zA-Z]+ ( '-' [a-zA-Z0-9]+ )*
% ~~~
%
% @compat SPARQL 1.0 [144].
% @compat SPARQL 1.1 Query [145].
% @compat Turtle 1.1 [144s].

'LANGTAG'([Tag|Subtags]) -->
  "@",
  dcg_atom_codes(ascii_letters, Tag),
  'LANGTAG1*'(Subtags).

ascii_letters([C]) -->
  ascii_letter(C).
ascii_letters([H|T]) -->
  ascii_letter(H),
  ascii_letters(T).

'LANGTAG1*'([Subtag|Subtags]) -->
  "-",
  ascii_alphanumerics(Subtag),
  'LANGTAG1*'(Subtags).
'LANGTAG1*'([]) --> [].

ascii_alphanumerics([C]) -->
  ascii_alphanumeric(C).
ascii_alphanumerics([H|T]) -->
  ascii_alphanumeric(H),
  ascii_alphanumerics(T).



%! 'NIL'// .
% ~~~{.ebnf}
% '(' WS* ')'
% ~~~
%
% @compat SPARQL 1.1 Query [161].

'NIL' -->
  bracketed(round, 'WS*').



%! 'VarOrTerm'// .
% ~~~{.ebnf}
% [106]    VarOrTerm ::= Var | GraphTerm
% ~~~

'VarOrTerm' --> 'Var'.
'VarOrTerm' --> 'GraphTerm'.



'WS*' --> 'WS', 'WS*'.
'WS*' --> [].

