:- module(
  turtle_terminals,
  [
    'ANON'//0,
    'BLANK_NODE_LABEL'//0,
    'DECIMAL'//0,
    'DOUBLE'//0,
    'INTEGER'//0,
    'IRIREF'//0,
    'LANGTAG'//0,
    'PN_LOCAL'//0,
    'PNAME_LN'//0,
    'PNAME_NS'//0,
    'STRING_LITERAL_QUOTE'//0,
    'STRING_LITERAL_SINGLE_QUOTE'//0,
    'STRING_LITERAL_LONG_SINGLE_QUOTE'//0,
    'STRING_LITERAL_LONG_QUOTE'//0
  ]
).

/** <module> Turtle terminals

@author Wouter Beek
@version 2014/04
*/



%! 'ANON'// .
% ~~~{.ebnf}
% [162s]   ANON ::= '[' WS* ']'
% ~~~

'ANON' --> `[`, 'WS*', `]`.

'WS*' --> 'WS', 'WS*'.
'WS*' --> [].


%! 'BLANK_NODE_LABEL'// .
% ~~~{.ebnf}
% [141s]   BLANK_NODE_LABEL ::= '_:'
%                               (PN_CHARS_U | [0-9])
%                               ((PN_CHARS | '.')* PN_CHARS)?
% ~~~

'BLANK_NODE_LABEL' -->
  `_:`,
  ('PN_CHARS_U' ; '[0-9]'),
  (`` ; 'BLANK_NODE_LABEL_3*', 'PN_CHARS').

'BLANK_NODE_LABEL_3*' --> ('PN_CHARS'; `.`), 'BLANK_NODE_LABEL_3*'.
'BLANK_NODE_LABEL_3*' --> [].


%! 'DECIMAL'// .
% ~~~{.ebnf}
% [20]   DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ~~~

'DECIMAL' --> (`` ; `+` ; `-`), '[0-9]*', `.`, '[0-9]+'.


%! 'DOUBLE'// .
% ~~~{.ebnf}
% [21]   DOUBLE ::= [+-]?
%        ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
% ~~~

'DOUBLE' --> (`` ; `+` ; `-`), 'DOUBLE_char'.

'DOUBLE_char' --> 'DOUBLE_char1'.
'DOUBLE_char' --> 'DOUBLE_char2'.
'DOUBLE_char' --> 'DOUBLE_char3'.

'DOUBLE_char1' --> '[0-9]+', 'DOUBLE_char2'.
'DOUBLE_char2' --> `.`, 'DOUBLE_char3'.
'DOUBLE_char3' --> '[0-9]+', 'EXPONENT'.


%! 'ECHAR'// .
% ~~~{.ebnf}
% [159s]   ECHAR ::= '\' [tbnrf"'\]
% ~~~

'ECHAR' --> `\\`, 'ECHAR_char'.

'ECHAR_char' --> `t`.
'ECHAR_char' --> `b`.
'ECHAR_char' --> `n`.
'ECHAR_char' --> `r`.
'ECHAR_char' --> `f`.
'ECHAR_char' --> `"`.
'ECHAR_char' --> `'`.
'ECHAR_char' --> `\\`.


%! 'EXPONENT'// .
% ~~~{.ebnf}
% [154s]   EXPONENT ::= [eE] [+-]? [0-9]+
% ~~~

'EXPONENT' --> (`e` ; `E`), (`` ; `+` ; `-`), '[0-9]+'.


%! 'HEX'// .
% ~~~{.ebnf}
% [171s]   HEX ::= [0-9] | [A-F] | [a-f]
% ~~~

'HEX' --> '[0-9]'.
'HEX' --> between_dec(65, 70).
'HEX' --> between_dec(97, 102).


%! 'INTEGER'// .
% ~~~{.ebnf}
% [19]   INTEGER ::= [+-]? [0-9]+
% ~~~

'INTEGER' --> (`` ; `+` ; `-`), '[0-9]+'.


%! 'IRIREF'// .
% ~~~{.ebnf}
% [18]   IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
%                   /* #x00=NULL #01, '1F=control codes #x20=space */
% ~~~

'IRIREF' --> `<`, 'IRIREF_char*', `>`.

'IRIREF_char*' --> 'IRIREF_char', 'IRIREF_char*'.
'IRIREF_char*' --> [].

'IRIREF_char' --> [C], {C =< 32}, !, {fail}.
'IRIREF_char' --> `<`, !, {fail}.
'IRIREF_char' --> `>`, !, {fail}.
'IRIREF_char' --> `"`, !, {fail}.
'IRIREF_char' --> `{`, !, {fail}.
'IRIREF_char' --> `}`, !, {fail}.
'IRIREF_char' --> `|`, !, {fail}.
'IRIREF_char' --> `^`, !, {fail}.
'IRIREF_char' --> `\``, !, {fail}.
'IRIREF_char' --> `\\`, !, {fail}.
'IRIREF_char' --> 'UCHAR'.
'IRIREF_char' --> [_].


%! 'LANGTAG'// .
% ~~~{.ebnf}
% [144s]   LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
% ~~~

'LANGTAG' --> `@`, '[a-zA-Z]+', 'LANGTAG_3*'.

'LANGTAG_3*' --> `-`, '[a-zA-Z0-9]+', 'LANGTAG_3*'.
'LANGTAG_3*' --> [].


%! 'PERCENT'// .
% ~~~{.ebnf}
% [170s]   PERCENT ::= '%' HEX HEX
% ~~~

'PERCENT' --> `%`, 'HEX', 'HEX'.


%! 'PLX'// .
% ~~~{.ebnf}
% [169s]   PLX ::= PERCENT | PN_LOCAL_ESC
% ~~~

'PLX' --> 'PERCENT'.
'PLX' --> 'PN_LOCAL_ESC'.


%! 'PN_CHARS'// .
% ~~~{.ebnf}
% [166s]   PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 |
%                       [#x0300-#x036F] | [#x203F-#x2040]
% ~~~

'PN_CHARS' --> 'PN_CHARS_U'.
'PN_CHARS' --> `-`.
'PN_CHARS' --> '[0-9]'.
'PN_CHARS' --> hex('00B7').
'PN_CHARS' --> between_hex('0300', '036F').
'PN_CHARS' --> between_hex('203F', '2040').


%! 'PN_CHARS_BASE'// .
% ~~~{.ebnf}
% [163s]   PN_CHARS_BASE ::= [A-Z] | [a-z] |
%                            [#x00C0-#x00D6] | [#x00D8-#x00F6] |
%                            [#x00F8-#x02FF] | [#x0370-#x037D] |
%                            [#x037F-#x1FFF] | [#x200C-#x200D] |
%                            [#x2070-#x218F] | [#x2C00-#x2FEF] |
%                            [#x3001-#xD7FF] | [#xF900-#xFDCF] |
%                            [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
% ~~~

'PN_CHARS_BASE' --> '[a-zA-Z]'.
'PN_CHARS_BASE' --> between_hex('00C0', '00D6').
'PN_CHARS_BASE' --> between_hex('00D8', '00F6').
'PN_CHARS_BASE' --> between_hex('00F8', '02FF').
'PN_CHARS_BASE' --> between_hex('0370', '037D').
'PN_CHARS_BASE' --> between_hex('037F', '1FFF').
'PN_CHARS_BASE' --> between_hex('200C', '200D').
'PN_CHARS_BASE' --> between_hex('2070', '218F').
'PN_CHARS_BASE' --> between_hex('2C00', '2FEF').
'PN_CHARS_BASE' --> between_hex('3001', 'D7FF').
'PN_CHARS_BASE' --> between_hex('F900', 'FDCF').
'PN_CHARS_BASE' --> between_hex('FDF0', 'FFFD').
'PN_CHARS_BASE' --> between_hex('10000', 'EFFFF').


%! 'PN_CHARS_U'// .
% ~~~{.ebnf}
% [164s]   PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ~~~

'PN_CHARS_U' --> 'PN_CHARS_BASE'.
'PN_CHARS_U' --> `_`.


%! 'PN_LOCAL'// .
% ~~~{.ebnf}
% [168s]   PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX)
%                       ((PN_CHARS | '.' | ':' | PLX)*
%                        (PN_CHARS | ':' | PLX)
%                       )?
% ~~~

'PN_LOCAL' --> 'PN_LOCAL_1', (`` ; 'PN_LOCAL_2').

'PN_LOCAL_1' --> 'PN_CHARS_U'.
'PN_LOCAL_1' --> `:`.
'PN_LOCAL_1' --> '[0-9]'.
'PN_LOCAL_1' --> 'PLX'.

'PN_LOCAL_2' --> 'PN_LOCAL_2a*', 'PN_LOCAL_2b'.

'PN_LOCAL_2a*' --> [].
'PN_LOCAL_2a*' --> 'PN_LOCAL_2a', 'PN_LOCAL_2a*'.

'PN_LOCAL_2a' --> `.`.
'PN_LOCAL_2a' --> 'PN_LOCAL_2b'.

'PN_LOCAL_2b' --> 'PN_CHARS'.
'PN_LOCAL_2b' --> `:`.
'PN_LOCAL_2b' --> 'PLX'.


%! 'PN_LOCAL_ESC'// .
% ~~~{.ebnf}
% [172s]   PN_LOCAL_ESC ::= '\'
%                           ('_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                            "'" | '(' | ')' | '*' | '+' | ',' | ';' |
%                            '=' | '/' | '?' | '#' | '@' | '%'
%                           )
% ~~~

'PN_LOCAL_ESC' --> `\\`, 'PN_LOCAL_ESC_char'.

'PN_LOCAL_ESC_char' --> `_`.
'PN_LOCAL_ESC_char' --> `~`.
'PN_LOCAL_ESC_char' --> `.`.
'PN_LOCAL_ESC_char' --> `-`.
'PN_LOCAL_ESC_char' --> `!`.
'PN_LOCAL_ESC_char' --> `$`.
'PN_LOCAL_ESC_char' --> `&`.
'PN_LOCAL_ESC_char' --> `'`.
'PN_LOCAL_ESC_char' --> `(`.
'PN_LOCAL_ESC_char' --> `)`.
'PN_LOCAL_ESC_char' --> `*`.
'PN_LOCAL_ESC_char' --> `+`.
'PN_LOCAL_ESC_char' --> `,`.
'PN_LOCAL_ESC_char' --> `;`.
'PN_LOCAL_ESC_char' --> `=`.
'PN_LOCAL_ESC_char' --> `/`.
'PN_LOCAL_ESC_char' --> `?`.
'PN_LOCAL_ESC_char' --> `#`.
'PN_LOCAL_ESC_char' --> `@`.
'PN_LOCAL_ESC_char' --> `%`.


%! 'PN_PREFIX'// .
% ~~~{.ebnf}
% [167s]   PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
% ~~~

'PN_PREFIX' --> 'PN_CHARS_BASE', (`` ; 'PN_PREFIX_2*').

'PN_PREFIX_2*' --> ('PN_CHARS' ; `.`), 'PN_PREFIX_2*'.
'PN_PREFIX_2*' --> [].


%! 'PNAME_LN'// .
% ~~~{.ebnf}
% [140s]   PNAME_LN ::= PNAME_NS PN_LOCAL
% ~~~

'PNAME_LN' -->
  'PNAME_NS',
  'PN_LOCAL'.


%! 'PNAME_NS'// .
% ~~~{.abn}
% [139s]   PNAME_NS ::= PN_PREFIX? ':'
% ~~~

'PNAME_NS' --> (`` ; 'PN_PREFIX'), `:`.


%! 'STRING_LITERAL_QUOTE'// .
% ~~~{.ebnf}
% [22]   STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                                 /* #x22=" #x5C=\ #xA=new line
%                                    #xD=carriage return */
% ~~~

'STRING_LITERAL_QUOTE' --> `"`, 'STRING_LITERAL_QUOTE_char*', `"`.

'STRING_LITERAL_QUOTE_char*' --> 'STRING_LITERAL_QUOTE_char', 'STRING_LITERAL_QUOTE_char*'.
'STRING_LITERAL_QUOTE_char*' --> [].

'STRING_LITERAL_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_QUOTE_char' --> hex('22'), !, {fail}.
'STRING_LITERAL_QUOTE_char' --> hex('5C'), !, {fail}.
'STRING_LITERAL_QUOTE_char' --> hex('A'), !, {fail}.
'STRING_LITERAL_QUOTE_char' --> hex('D'), !, {fail}.
'STRING_LITERAL_QUOTE_char' --> [_].


%! 'STRING_LITERAL_SINGLE_QUOTE'// .
% ~~~{.ebnf}
% [23]   STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                        ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                        "'"
%                                        /* #x27=' #x5C=\ #xA=new line
%                                           #xD=carriage return */
% ~~~

'STRING_LITERAL_SINGLE_QUOTE' --> `'`, 'STRING_LITERAL_SINGLE_QUOTE_char*', `'`.

'STRING_LITERAL_SINGLE_QUOTE_char*' --> 'STRING_LITERAL_SINGLE_QUOTE_char', 'STRING_LITERAL_SINGLE_QUOTE_char*'.
'STRING_LITERAL_SINGLE_QUOTE_char*' --> [].

'STRING_LITERAL_SINGLE_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_SINGLE_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('27'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('5C'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('A'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('D'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> [_].


%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'// .
% ~~~{.ebnf}
% [24]   STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''"
%                                             (("'" | "''")?
%                                              ([^'\] | ECHAR | UCHAR)
%                                             )*
%                                             "'''"
% ~~~

'STRING_LITERAL_LONG_SINGLE_QUOTE' -->
  `'''`,
  (`` ; `'` ; `''`),
  'STRING_LITERAL_LONG_SINGLE_QUOTE_char*',
  `'''`.

'STRING_LITERAL_LONG_SINGLE_QUOTE_char*' --> 'STRING_LITERAL_LONG_SINGLE_QUOTE_char', 'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*' --> [].

'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> `'`, !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> `\\`, !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> [_].


%! 'STRING_LITERAL_LONG_QUOTE'// .
% ~~~{.ebnf}
% [25]   STRING_LITERAL_LONG_QUOTE ::= '"""'
%                                      (('"' | '""')? ([^"\] | ECHAR | UCHAR))*
%                                      '"""'
% ~~~

'STRING_LITERAL_LONG_QUOTE' -->
  `"""`,
  (`` ; `"` ; `""`),
  'STRING_LITERAL_LONG_QUOTE_char*',
  `"""`.

'STRING_LITERAL_LONG_QUOTE_char*' --> 'STRING_LITERAL_LONG_QUOTE_char', 'STRING_LITERAL_LONG_QUOTE_char*'.
'STRING_LITERAL_LONG_QUOTE_char*' --> [].

'STRING_LITERAL_LONG_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_LONG_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_LONG_QUOTE_char' --> `"`, !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char' --> `\\`, !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char' --> [_].


%! 'UCHAR'// .
% ~~~{.ebnf}
% [26]   UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ~~~

'UCHAR' --> `\\u`, 'HEX', 'HEX', 'HEX', 'HEX'.
'UCHAR' --> `\\U`, 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX'.


%! 'WS'// .
% ~~~{.ebnf}
% [161s]   WS ::= #x20 | #x9 | #xD | #xA
%                 /* #x20=space #x9=character tabulation
%                    #xD=carriage return #xA=new line */
% ~~~

'WS' --> hex('20').
'WS' --> hex('9').
'WS' --> hex('d').
'WS' --> hex('a').



% HELPERS %

:- use_module(library(apply)).

'[0-9]' --> between_dec(48, 57).

'[0-9]+' --> '[0-9]', '[0-9]*'.

'[0-9]*' --> '[0-9]', '[0-9]*'.
'[0-9]*' --> [].

'[A-Z]' --> between_dec(65, 90).

'[a-z]' --> between_dec(97, 122).

'[a-zA-Z]' --> '[A-Z]'.
'[a-zA-Z]' --> '[a-z]'.

'[a-zA-Z]+' --> '[a-zA-Z]', '[a-zA-Z]*'.

'[a-zA-Z]*' --> '[a-zA-Z]', '[a-zA-Z]*'.
'[a-zA-Z]*' --> [].

'[a-zA-Z0-9]' --> '[a-zA-Z]'.
'[a-zA-Z0-9]' --> '[0-9]'.

'[a-zA-Z0-9]+' --> '[a-zA-Z0-9]', '[a-zA-Z0-9]*'.

'[a-zA-Z0-9]*' --> '[a-zA-Z0-9]', '[a-zA-Z0-9]*'.
'[a-zA-Z0-9]*' --> [].


%! between_dec(+BeginDec:between(0,9), +EndDec:between(0,9))// .

between_dec(BeginDec, EndDec) -->
  [C],
  {between(BeginDec, EndDec, C)}.


%! between_hex(+BeginHex:atom, +EndHex:atom)// .

between_hex(BeginHex, EndHex) -->
  {maplist(hex_value, [BeginHex,EndHex], [BeginDec,EndDec])},
  between_dec(BeginDec, EndDec).


hex(Hex) -->
  {hex_value(Hex, Dec)},
  [Dec].


%! hex_digits(+HexValue:atom, -DecValue:integer) is det.

hex_value(HexValue, DecValue):-
  atom_chars(HexValue, HexDigits),
  hex_digits(HexDigits, 0, DecValue).

hex_digits([], N, N).
hex_digits([H|T], N1, N):-
  char_type(H, xdigit(N0)),
  N2 is N1 * 16 + N0,
  hex_digits(T, N2, N).

