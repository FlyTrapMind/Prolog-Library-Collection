:- module(
  sparql_word,
  [
    'BLANK_NODE_LABEL'//1, % ?Codes:list(code)
    'IRIREF'//1, % ?Codes:list(code)
    'LANGTAG'//1, % ?LanguageTag:list(atom)
    'PN_LOCAL'//1, % ?Codes:list(code)
    'PNAME_LN'//1, % ?Codes:list(code)
    'PNAME_NS'//1, % ?Codes:list(code)
    'VAR1'//1, % ?Name:atom
    'VAR2'//1, % ?Name:atom
    'VARNAMES'//1 % ?Codes:list(code)
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
:- use_module(dcg(dcg_multi)).
:- use_module(sparql(sparql_char)).



%! 'BLANK_NODE_LABEL'(?Codes:list(code))// .
% ~~~{.ebnf}
% BLANK_NODE_LABEL ::= '_:'
%                      ( PN_CHARS_U | [0-9] )
%                      ( ( PN_CHARS | '.' )* PN_CHARS )?
% ~~~
%
% @compat SPARQL 1.0 [141].
% @compat SPARQL 1.1 Query [142].
% @compat Turtle 1.1 [141s].

'BLANK_NODE_LABEL'([H|T2]) -->
  `_:`,
  % First character after colon.
  (
    'PN_CHARS_U'(H)
  ;
    decimal_digit(H)
  ),
  % Rest of characters.
  (
    ``, {T2 = []}
  ;
    'BLANK_NODE_LABEL_1*'(T1),
    'PN_CHARS'(Last),
    {append(T1, [Last], T2)}
  ).

'BLANK_NODE_LABEL_1*'([H|T]) -->
  ( 'PN_CHARS'(H) ; dot(H) ),
  'BLANK_NODE_LABEL_1*'(T).
'BLANK_NODE_LABEL_1*'([]) --> [].


%! 'IRIREF'(?Codes:list(code))// .
% ~~~{.ebnf}
% IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ~~~
%
% @compat SPARQL 1.1 Query [139].
% @tbd What about DELETE (decimal 127)?

'IRIREF'(Codes) -->
  bracketed(angular, 'IRIREF_char*'(Codes)).

'IRIREF_char*'([H|T]) -->
  'IRIREF_char'(H),
  'IRIREF_char*'(T).
'IRIREF_char*'([]) --> [].

'IRIREF_char'(C) --> control(C), !, {fail}.
'IRIREF_char'(C) --> angular_bracket(C), !, {fail}.
'IRIREF_char'(C) --> double_quote(C), !, {fail}.
'IRIREF_char'(C) --> curly_bracket(C), !, {fail}.
'IRIREF_char'(C) --> vertical_bar(C), !, {fail}.
'IRIREF_char'(C) --> carret(C), !, {fail}.
'IRIREF_char'(C) --> tilde(C), !, {fail}.
'IRIREF_char'(C) --> backslash(C), !, {fail}.
'IRIREF_char'(C) --> [C].


%! 'LANGTAG'(?LanguageTag:list(atom))// .
% ~~~{.ebnf}
% LANGTAG ::= '@' [a-zA-Z]+ ( '-' [a-zA-Z0-9]+ )*
% ~~~
%
% @compat SPARQL 1.0 [144].
% @compat SPARQL 1.1 Query [145].
% @compat Turtle 1.1 [144s].

'LANGTAG'([Tag|Subtags]) -->
  `@`,
  dcg_atom_codes(ascii_letters, Tag),
  'LANGTAG1*'(Subtags).

ascii_letters([C]) -->
  ascii_letter(C).
ascii_letters([H|T]) -->
  ascii_letter(H),
  ascii_letters(T).

'LANGTAG1*'([Subtag|Subtags]) -->
  `-`,
  ascii_alphanumerics(Subtag),
  'LANGTAG1*'(Subtags).
'LANGTAG1*'([]) --> [].

ascii_alphanumerics([C]) -->
  ascii_alphanumeric(C).
ascii_alphanumerics([H|T]) -->
  ascii_alphanumeric(H),
  ascii_alphanumerics(T).


%! 'PN_LOCAL'(?Codes:list(code))// .
% ~~~{.ebnf}
% PN_LOCAL ::= ( PN_CHARS_U |
%                ':' |
%                [0-9] |
%                PLX
%              )
%              ( ( PN_CHARS | '.' | ':' | PLX )*
%                ( PN_CHARS | ':' | PLX )
%              )?
% ~~~
%
% @compat Turtle 1.1 [168s].
% @compat SPARQL 1.1 Query [169].

'PN_LOCAL'([H|T]) -->
  'PN_LOCAL_1'(H),
  ('PN_LOCAL_2'(T) ; {T = []}).

'PN_LOCAL_1'(C) --> 'PN_CHARS_U'(C).
'PN_LOCAL_1'(C) --> colon(C).
'PN_LOCAL_1'(C) --> decimal_digit(C).
'PN_LOCAL_1'(C) --> 'PLX'(C).

'PN_LOCAL_2'(L2) -->
  'PN_LOCAL_2a*'(L1),
  'PN_LOCAL_2b'(Last),
  {append(L1, [Last], L2)}.

'PN_LOCAL_2a*'([H|T]) -->
  'PN_LOCAL_2a'(H),
  'PN_LOCAL_2a*'(T).
'PN_LOCAL_2a*'([]) --> [].

'PN_LOCAL_2a'(C) --> dot(C).
'PN_LOCAL_2a'(C) --> 'PN_LOCAL_2b'(C).

'PN_LOCAL_2b'(C) --> 'PN_CHARS'(C).
'PN_LOCAL_2b'(C) --> colon(C).
'PN_LOCAL_2b'(C) --> 'PLX'(C).


%! 'PN_PREFIX'(?Codes:list(code))// .
% ~~~{.ebnf}
% PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
% ~~~
%
% @compat SPARQL 1.0 [167].
% @compat SPARQL 1.1 Query [168].
% @compat Turtle 1.1 [167s].

'PN_PREFIX'([H|T]) -->
  'PN_CHARS_BASE'(H),
  (`` ; 'PN_PREFIX_2*'(T)).

'PN_PREFIX_2*'([H|T]) -->
  ('PN_CHARS'(H) ; dot(H)),
  'PN_PREFIX_2*'(T).
'PN_PREFIX_2*'([]) --> [].


%! 'PNAME_LN'(?Codes:list(code))// .
% ~~~{.ebnf}
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ~~~
%
% @compat SPARQL 1.0 [140].
% @compat SPARQL 1.1 Query [141].
% @compat Turtle 1.1 [140s].

'PNAME_LN'(L3) -->
  'PNAME_NS'(L1),
  'PN_LOCAL'(L2),
  {append(L1, L2, L3)}.


%! 'PNAME_NS'(?Codes:list(code))// .
% ~~~{.abn}
% PNAME_NS ::= PN_PREFIX? ':'
% ~~~
%
% @compat SPARQL 1.0 [139].
% @compat SPARQL 1.1 Query [140].
% @compat Turtle 1.1 [139s].

'PNAME_NS'(L2) -->
  ('PN_PREFIX'(L1) ; {L1 = []}),
  colon(Last),
  {append(L1, [Last], L2)}.


%! 'VAR1'(?Name:atom)// .
% ~~~{.ebnf}
% VAR1 ::= '?' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [143].

'VAR1'(Name) -->
  `?`,
  dcg_atom_codes('VARNAME', Name).


%! 'VAR2'(?Name:atom)// .
% ~~~{.ebnf}
% VAR2 ::= '$' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [144].

'VAR2'(Name) -->
  `$`,
  dcg_atom_codes('VARNAME', Name).


%! 'VARNAME'(?Codes:list(code))// .
% ~~~{.ebnf}
% VARNAME ::= ( PN_CHARS_U |
%               [0-9] )
%             ( PN_CHARS_U |
%               [0-9] |
%               #x00B7 |
%               [#x0300-#x036F] |
%               [#x203F-#x2040] )*
% ~~~
%
% @compat SPARQL 1.1 Query [166].

'VARNAME'([H|T]) -->
  'VARNAME_1'(H),
  'VARNAME_2*'(T).

'VARNAME_1'(C) -->
  'PN_CHARS_U'(C).
'VARNAME_1'(C) -->
  decimal_digit(C).

'VARNAME_2*'([H|T]) -->
  'VARNAME_2'(H),
  'VARNAME_2*'(T).

'VARNAME_2'(C) --> 'PN_CHARS_U'(C).
'VARNAME_2'(C) --> decimal_digit(C).
'VARNAME_2'(C) --> hex_code('B7', C).
'VARNAME_2'(C) --> between_hex('300', '36F', C).
'VARNAME_2'(C) --> between_hex('203F', '2040', C).

