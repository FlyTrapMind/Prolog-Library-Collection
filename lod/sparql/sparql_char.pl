:- module(
  sparql_char,
  [
    'ECHAR'//1, % ?Code:code
    'HEX'//1, % ?Weight:between(0,15)
    'PLX'//1, % ?Code:code
    'PERCENT'//1, % ?Weight:between(0,256)
    'PN_LOCAL_ESC'//1, % ?Code:code
    'PN_CHARS'//1, % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//1, % ?Code:code
    'WS'//0
  ]
).

/** <module> SPARQL character

DCGs for characters defined by the SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_unicode)).



%! 'ECHAR'(?Code:code)// .
% ~~~{.ebnf}
% ECHAR ::= '\' [tbnrf"'\]
% ~~~
%
% @compat SPARQL 1.0 [159].
% @compat SPARQL 1.1 Query [160].
% @compat Turtle 1.1 [159s].

'ECHAR'(C) --> `\\`, 'ECHAR_char'(C).

'ECHAR_char'(C) --> `t`, {horizontal_tab(C, _, _)}.
'ECHAR_char'(C) --> `b`, {bell(C, _, _)}.
'ECHAR_char'(C) --> `n`, {line_feed(C, _, _)}.
'ECHAR_char'(C) --> `r`, {carriage_return(C, _, _)}.
'ECHAR_char'(C) --> `f`, {form_feed(C, _, _)}.
'ECHAR_char'(C) --> double_quote(C).
'ECHAR_char'(C) --> apostrophe(C).
'ECHAR_char'(C) --> backslash(C).


%! 'HEX'(?Weight:between(0,15))// .
% ~~~{.ebnf}
% HEX ::= [0-9] | [A-F] | [a-f]
% ~~~
%
% @compat SPARQL 1.0 [171].
% @compat SPARQL 1.1 Query [172].
% @compat Turtle 1.1 [171s].

'HEX'(Weight) -->
  hexadecimal_digit(_, Weight).


%! 'PERCENT'(?Weight:between(0,256))// .
% ~~~{.ebnf}
% PERCENT ::= '%' HEX HEX
% ~~~
%
% @compat SPARQL 1.0 [170].
% @compat SPARQL Query 1.1 [171].
% @compat Turtle 1.1 [170s].

'PERCENT'(Weight) -->
  {between(0, 256, Weight)},
  {Weight1 is Weight // 16},
  {Weight2 is Weight rem 16},
  'PERCENT'(Weight1, Weight2).
'PERCENT'(Weight) -->
  {var(Weight)},
  'PERCENT'(Weight1, Weight2),
  {Weight is Weight1 * 16 + Weight2}.

'PERCENT'(Weight1, Weight2) -->
  `%`,
  'HEX'(Weight1),
  'HEX'(Weight2).


%! 'PLX'(?Code:code)// .
% ~~~{.ebnf}
% PLX ::= PERCENT | PN_LOCAL_ESC
% ~~~
%
% @compat SPARQL 1.0 [169].
% @compat SPARQL 1.1 Query [170].
% @compat Turtle 1.1 [169s].

'PLX'(C) --> 'PERCENT'(C).
'PLX'(C) --> 'PN_LOCAL_ESC'(C).


%! 'PN_CHARS'(?Code:code)// .
% ~~~{.ebnf}
% PN_CHARS ::= PN_CHARS_U |
%              '-' |
%              [0-9] |
%              #xB7 |
%              [#x300-#x36F] |
%              [#x203F-#x2040]
% ~~~
%
% @compat SPARQL 1.0 [166].
% @compat SPARQL 1.1 Query [167].
% @compat Turtle 1.1 [166s].

'PN_CHARS'(C) --> 'PN_CHARS_U'(C).
'PN_CHARS'(C) --> hyphen_minus(C).
'PN_CHARS'(C) --> decimal_digit(C).
'PN_CHARS'(C) --> hex_code('00B7', C).
'PN_CHARS'(C) --> between_hex('0300', '036F', C).
'PN_CHARS'(C) --> between_hex('203F', '2040', C).


%! 'PN_CHARS_BASE'(?Code:code)// .
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
% @compat SPARQL 1.0 [163].
% @compat SPARQL 1.1 Query [164].
% @compat Turtle 1.1 [163s].

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
'PN_CHARS_BASE'(C) --> between_hex('FDF0', 'FFFD', C).
% #x10000-#xEFFFF
'PN_CHARS_BASE'(C) --> between_hex('10000', 'EFFFF', C).


%! 'PN_CHARS_U'(?Code:code)// .
% ~~~{.ebnf}
% PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ~~~
%
% @compat SPARQL 1.0 [164].
% @compat SPARQL 1.1 Query [165].
% @compat Turtle 1.1 [164s].

'PN_CHARS_U'(C) --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(C) --> underscore(C).


%! 'PN_LOCAL_ESC'(?Code:code)// .
% ~~~{.ebnf}
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | '*' | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ~~~
%
% @compat SPARQL 1.0 [172].
% @compat SPARQL 1.1 Query [173].
% @compat Turtle 1.1 [172s].

'PN_LOCAL_ESC'(C) -->
  `\\`,
  'PN_LOCAL_ESC_char'(C).

'PN_LOCAL_ESC_char'(C) --> underscore(C).
'PN_LOCAL_ESC_char'(C) --> tilde(C).
'PN_LOCAL_ESC_char'(C) --> dot(C).
'PN_LOCAL_ESC_char'(C) --> hyphen_minus(C).
'PN_LOCAL_ESC_char'(C) --> exclamation_mark(C).
'PN_LOCAL_ESC_char'(C) --> dollar_sign(C).
'PN_LOCAL_ESC_char'(C) --> ampersand(C).
'PN_LOCAL_ESC_char'(C) --> apostrophe(C).
'PN_LOCAL_ESC_char'(C) --> opening_round_bracket(C).
'PN_LOCAL_ESC_char'(C) --> closing_round_bracket(C).
'PN_LOCAL_ESC_char'(C) --> asterisk(C).
'PN_LOCAL_ESC_char'(C) --> plus_sign(C).
'PN_LOCAL_ESC_char'(C) --> comma(C).
'PN_LOCAL_ESC_char'(C) --> semi_colon(C).
'PN_LOCAL_ESC_char'(C) --> equals_sign(C).
'PN_LOCAL_ESC_char'(C) --> slash(C).
'PN_LOCAL_ESC_char'(C) --> question_mark(C).
'PN_LOCAL_ESC_char'(C) --> number_sign(C).
'PN_LOCAL_ESC_char'(C) --> at_sign(C).
'PN_LOCAL_ESC_char'(C) --> percent_sign(C).


%! 'WS'// .
% ~~~{.ebnf}
% WS ::= #x20 | #x9 | #xD | #xA
%        /* #x20=space #x9=character tabulation
%           #xD=carriage return #xA=new line */
% ~~~
%
% @compat SPARQL 1.0 [161].
% @compat SPARQL 1.1 Query [162].
% @compat Turtle 1.1 [161s].

'WS' --> space.
'WS' --> horizontal_tab.
'WS' --> carriage_return.
'WS' --> line_feed.

