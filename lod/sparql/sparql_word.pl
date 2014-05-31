:- module(
  sparql_word,
  [
    'ANON'//0,
    'BLANK_NODE_LABEL'//1, % ?BNodeLabel:atom
    'IRIREF'//1, % ?Iri:atom
    'LANGTAG'//1, % ?LanguageTag:list(atom)
    'NIL'//0,
    'PN_LOCAL'//1, % ?LocalPart:atom
    'PN_PREFIX'//1, % ?Prefix:atom
    'PNAME_LN'//1, % ?FullName:pair(atom)
    'PNAME_NS'//1, % ?Prefix:atom
    'VAR1'//1, % ?VarName:atom
    'VAR2'//1, % ?VarName:atom
    'VARNAME'//1 % ?VarName:atom
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



%! 'ANON'// .
% ~~~{.ebnf}
% ANON ::= '[' WS* ']'
% ~~~
%
% @compat SPARQL 1.0 [162].
% @compat SPARQL 1.1 Query [163].
% @compat Turtle 1.1 [162s].

'ANON' -->
  bracketed(square, 'WS*').


%! 'BLANK_NODE_LABEL'(?BNodeLabel:atom)// .
% ~~~{.ebnf}
% BLANK_NODE_LABEL ::= '_:'
%                      ( PN_CHARS_U | [0-9] )
%                      ( ( PN_CHARS | '.' )* PN_CHARS )?
% ~~~
%
% @compat SPARQL 1.0 [141].
% @compat SPARQL 1.1 Query [142].
% @compat Turtle 1.1 [141s].

'BLANK_NODE_LABEL'(BNodeLabel) -->
  dcg_atom_codes('BLANK_NODE_LABEL_codes', BNodeLabel) -->

'BLANK_NODE_LABEL_codes'([H|T2]) -->
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


%! 'IRIREF'(?Iri:atom)// .
% ~~~{.ebnf}
% IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ~~~
%
% @compat SPARQL 1.1 Query [139].
% @tbd What about DELETE (decimal 127)?

'IRIREF'(Iri) -->
  dcg_atom_codes(bracketed(angular, 'IRIREF_char*'(Codes)), Iri).

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


%! 'NIL'// .
% ~~~{.ebnf}
% '(' WS* ')'
% ~~~
%
% @compat SPARQL 1.1 Query [161].

'NIL' -->
  bracketed(round, 'WS*').


%! 'PN_LOCAL'(?LocalPart:atom)// .
% ~~~{.ebnf}
% PN_LOCAL ::= ( PN_CHARS_U | ':' | [0-9] | PLX )
%              ( ( PN_CHARS | '.' | ':' | PLX )*
%                ( PN_CHARS | ':' | PLX ) )?
% ~~~
%
% @compat SPARQL 1.0 [168].
% @compat SPARQL 1.1 Query [169].
% @compat Turtle 1.1 [168s].

'PN_LOCAL'(LocalPart) -->
  dcg_atom_codes('PN_LOCAL_codes', LocalPart).

'PN_LOCAL_codes'([H|T]) -->
  'PN_LOCAL_1'(H),
  ('PN_LOCAL_2'(T) ; {T = []}).

'PN_LOCAL_1'(C) --> 'PN_CHARS_U'(C).
'PN_LOCAL_1'(C) --> colon(C).
'PN_LOCAL_1'(C) --> decimal_digit(C).
'PN_LOCAL_1'(C) --> 'PLX'(C).

'PN_LOCAL_2'([H|T]) -->
  'PN_LOCAL_2a'(H),
  'PN_LOCAL_2'(T).
'PN_LOCAL_2'([Last]) -->
  'PN_LOCAL_2b'(Last).

'PN_LOCAL_2a'(C) --> dot(C).
'PN_LOCAL_2a'(C) --> 'PN_LOCAL_2b'(C).

'PN_LOCAL_2b'(C) --> 'PN_CHARS'(C).
'PN_LOCAL_2b'(C) --> colon(C).
'PN_LOCAL_2b'(C) --> 'PLX'(C).


%! 'PN_PREFIX'(?Prefix:atom)// .
% ~~~{.ebnf}
% PN_PREFIX ::= PN_CHARS_BASE ( ( PN_CHARS | '.' )* PN_CHARS )?
% ~~~
%
% @compat SPARQL 1.0 [167].
% @compat SPARQL 1.1 Query [168].
% @compat Turtle 1.1 [167s].

'PN_PREFIX'(Prefix) -->
  dcg_atom_codes('PN_PREFIX_codes', Prefix).

'PN_PREFIX_codes'([H|T]) -->
  'PN_CHARS_BASE'(H),
  (`` ; 'PN_PREFIX_rest'(T)).

'PN_PREFIX_rest'([H|T]) -->
  ('PN_CHARS'(H) ; dot(H)),
  'PN_PREFIX_rest'(T).
'PN_PREFIX_rest'([Last]) -->
  'PN_CHARS'(Last).


%! 'PNAME_LN'(?FullName:pair(atom))// .
% ~~~{.ebnf}
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ~~~
%
% @compat SPARQL 1.0 [140].
% @compat SPARQL 1.1 Query [141].
% @compat Turtle 1.1 [140s].

'PNAME_LN'(Prefix-Local) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local).


%! 'PNAME_NS'(?Prefix:atom)// .
% ~~~{.abn}
% PNAME_NS ::= PN_PREFIX? ':'
% ~~~
%
% @compat SPARQL 1.0 [139].
% @compat SPARQL 1.1 Query [140].
% @compat Turtle 1.1 [139s].

'PNAME_NS'(Prefix) -->
  ('PN_PREFIX'(Prefix) ; {Prefix = ''}),
  `:`.


%! 'VAR1'(?VarName:atom)// .
% ~~~{.ebnf}
% VAR1 ::= '?' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [143].

'VAR1'(VarName) -->
  `?`,
  'VARNAME'(VarName).


%! 'VAR2'(?VarName:atom)// .
% ~~~{.ebnf}
% VAR2 ::= '$' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [144].

'VAR2'(VarName) -->
  `$`,
  'VARNAME'(VarName).


%! 'VARNAME'(?VarName:atom)// .
% ~~~{.ebnf}
% VARNAME ::= ( PN_CHARS_U | [0-9] )
%             ( PN_CHARS_U | [0-9] | #x00B7 |
%               [#x0300-#x036F] | [#x203F-#x2040] )*
% ~~~
%
% @compat SPARQL 1.1 Query [166].

'VARNAME'(VarName) -->
  dcg_atom_codes('VARNAME_codes', VarName).

'VARNAME_codes'([H|T]) -->
  'VARNAME_first'(H),
  'VARNAME_rest*'(T).

'VARNAME_first'(C) -->
  'PN_CHARS_U'(C).
'VARNAME_first'(C) -->
  decimal_digit(C).

'VARNAME_rest*'([H|T]) -->
  'VARNAME_rest'(H),
  'VARNAME_rest*'(T).

'VARNAME_rest'(C) --> 'PN_CHARS_U'(C).
'VARNAME_rest'(C) --> decimal_digit(C).
'VARNAME_rest'(C) --> hex_code('B7', C).
'VARNAME_rest'(C) --> between_hex('300', '36F', C).
'VARNAME_rest'(C) --> between_hex('203F', '2040', C).


'WS*' --> 'WS', 'WS*'.
'WS*' --> [].

