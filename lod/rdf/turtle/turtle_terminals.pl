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
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_content)).



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
% HEX ::= [0-9] | [A-F] | [a-f]
% ~~~
%
% @compat Turtle 1.1 [171s].
% @compat SPARQL Query 1.1 [172].

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
%
% @compat Turtle 1.1 [18].
% @compate SPARQL 1.1 Query [139].

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
% PERCENT ::= '%' HEX HEX
% ~~~
%
% @compat Turtle 1.1 [170s].
% @compat SPARQL Query 1.1 [170].

'PERCENT' --> `%`, 'HEX', 'HEX'.


%! 'PLX'// .
% ~~~{.ebnf}
% PLX ::= PERCENT | PN_LOCAL_ESC
% ~~~
%
% @compat Turtle 1.1 [169s].
% @compat SPARQL 1.1 Query [170].

'PLX' --> 'PERCENT'.
'PLX' --> 'PN_LOCAL_ESC'.


%! 'PN_CHARS'// .
% ~~~{.ebnf}
% PN_CHARS ::= PN_CHARS_U |
%              '-' |
%              [0-9] |
%              #xB7 |
%              [#x300-#x36F] |
%              [#x203F-#x2040]
% ~~~
%
% @compat Turtle 1.1 [166s].
% @compat SPARQL 1.1 Query [167].

'PN_CHARS' --> 'PN_CHARS_U'.
'PN_CHARS' --> `-`.
'PN_CHARS' --> '[0-9]'.
'PN_CHARS' --> hex('00B7').
'PN_CHARS' --> between_hex('0300', '036F').
'PN_CHARS' --> between_hex('203F', '2040').


%! 'PN_CHARS_BASE'// .
% ~~~{.ebnf}
% PN_CHARS_BASE ::= [A-Z] |
%                   [a-z] |
%                   [#xC0-#xD6] |
%                   [#xD8-#xF6] |
%                   [#xF8-#x2FF] |
%                   [#x370-#x37D] |
%                   [#x37F-#x1FFF] |
%                   [#x200C-#x200D] |
%                   [#x2070-#x218F] |
%                   [#x2C00-#x2FEF] |
%                   [#x3001-#xD7FF] |
%                   [#xF900-#xFDCF] |
%                   [#xFDF0-#xFFFD] |
%                   [#x10000-#xEFFFF]
% ~~~
%
% @compat Turtle 1.1 [163s].
% @compat SPARQL 1.1 Query [164].

% [A-Z] and [a-z]
'PN_CHARS_BASE'(C) --> ascii_letter(C).
% #xC0-#xD6
'PN_CHARS_BASE'(C) --> between_hex('C0', 'D6', C).
% #xD8-#xF6
'PN_CHARS_BASE'(C) --> between_hex('D8', 'F6', C).
% #xF8-#x2FF
'PN_CHARS_BASE'(C) --> between_hex('F8', '2FF', C).
% #x370-#x37D
'PN_CHARS_BASE'(C) --> between_hex('370', '37D', C).
% #x37F-#x1FFF
'PN_CHARS_BASE'(C) --> between_hex('37F', '1FFF', C).
% #x200C-#x200D
'PN_CHARS_BASE'(C) --> zero_width_non_joiner(C).
'PN_CHARS_BASE'(C) --> zero_width_joiner(C).
% #x2070-#x218F
'PN_CHARS_BASE'(C) --> between_hex('2070', '218F', C).
% #x2C00-#x2FEF
'PN_CHARS_BASE'(C) --> between_hex('2C00', '2FEF', C).
% #x3001-#xD7FF
'PN_CHARS_BASE'(C) --> between_hex('3001', 'D7FF', C).
% #xF900-#xFDCF
'PN_CHARS_BASE'(C) --> between_hex('F900', 'FDCF', C).
% #xFDF0-#xFFFD
'PN_CHARS_BASE'(C) --> between_hex('FDF0, 'FFFD', C).
% #x10000-#xEFFFF
'PN_CHARS_BASE'(C) --> between_hex('10000', 'EFFFF', C).


%! 'PN_CHARS_U'// .
% ~~~{.ebnf}
% [164s]   PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ~~~

'PN_CHARS_U' --> 'PN_CHARS_BASE'.
'PN_CHARS_U' --> `_`.


%! 'PN_LOCAL'// .
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
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | '*' | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ~~~
%
% @compat Turtle 1.1 [172s].
% @compat SPARQL 1.1 Query [173].

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
% PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
% ~~~
%
% @compat Turtle 1.1 [167s].
% @compat SPARQL 1.1 Query [168].

'PN_PREFIX' --> 'PN_CHARS_BASE', (`` ; 'PN_PREFIX_2*').

'PN_PREFIX_2*' --> ('PN_CHARS' ; `.`), 'PN_PREFIX_2*'.
'PN_PREFIX_2*' --> [].


%! 'PNAME_LN'// .
% ~~~{.ebnf}
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ~~~
%
% @compat Turtle 1.1 [140s].
% @compat SPARQL 1.1 Query [141].

'PNAME_LN' -->
  'PNAME_NS',
  'PN_LOCAL'.


%! 'PNAME_NS'// .
% ~~~{.abn}
% PNAME_NS ::= PN_PREFIX? ':'
% ~~~
%
% @compat Turtle 1.1 [139s].
% @compat SPARQL 1.1 Query [140].

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


hex(Hex) -->
  {hex_value(Hex, Dec)},
  [Dec].

