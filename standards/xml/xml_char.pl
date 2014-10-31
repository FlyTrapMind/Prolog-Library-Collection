:- module(
  xml_char,
  [
    'Char'//1, % ?Code:code
    'Char11'//1, % ?Code:code
    'NameChar'//1, % ?Code:code
    'NameStartChar'//1, % ?Code:code
    'RestrictedChar'//1, % ?Code:code
    'S'//0
  ]
).

/** <module> XML: Character

DCGs for character definitions in XML recommendations.

@author Wouter Beek
@version 2014/05, 2014/08, 2014/10
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii), [
     carriage_return//0,
     carriage_return//1,
     colon//1,
     decimal_digit//2,
     dot//1,
     horizontal_tab//0,
     horizontal_tab//1,
     hyphen_minus//1,
     letter//1,
     line_feed//0,
     line_feed//1,
     space//0,
     underscore//1
   ]).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_unicode), [
     character_tie//1,
     middle_dot//1,
     undertie//1,
     zero_width_joiner//1,
     zero_width_non_joiner//1
   ]).



%! 'Char'(?Code:code)// .
% An **XML Character** is an atomic unit of text specified by ISO/IEC 10646.
%
% ~~~{.ebnf}
% Char ::=   #x9              // Horizontal tab
%          | #xA              // Line feed
%          | #xD              // Carriage return
%          | [#x20-#xD7FF]    // Space, punctuation, numbers, letters
%          | [#xE000-#xFFFD]
%          | [#x10000-#x10FFFF]
% ~~~
%
% Avoid comapatibility characters [Unicode, section 2.3].
% Avoid the following characters (control characters,
% permanently undefined Unicode characters):
%
% ~~~{.txt}
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ~~~
%
% @compat XML 1.0.5 [2].

% Horizontal tab =|#x9|=
'Char'(Code) --> horizontal_tab(Code).
% Line feed =|#xA|=
'Char'(Code) --> line_feed(Code).
% Carriage return =|#xD|=
'Char'(Code) --> carriage_return(Code).
% Space, punctuation, numbers, letters
% =|#x20-#xD7FF|=
'Char'(Code) --> between_code(hex('20'), hex('D7FF'), dec(Code)).
% =|#xE000-#xFFFD|=
'Char'(Code) --> between_code(hex('E000'), hex('FFFD'), dec(Code)).
% =|#x10000-#x10FFFF|=
'Char'(Code) --> between_code(hex('10000'), hex('10FFFF'), dec(Code)).



%! 'Char11'(?Code:code)// .
% ~~~{.ebnf}
% Char ::=   [#x1-#xD7FF]
%          | [#xE000-#xFFFD]
%          | [#x10000-#x10FFFF]
%             /* any Unicode character, excluding the surrogate blocks,
%                FFFE, and FFFF. */
% ~~~
%
% @compat XML 1.1.2 [2].

% #x1-#xD7FF
'Char11'(Code) --> between_code(hex('1'), hex('D7FF'), dec(Code)).
% #xE000-#xFFFD
'Char11'(Code) --> between_code(hex('E000'), hex('FFFD'), dec(Code)).
% #x10000-#x10FFFF
'Char11'(Code) --> between_code(hex('10000'), hex('10FFFF'), dec(Code)).



%! 'NameChar'(?Code:code)// .
% ~~~{.ebnf}
% NameChar ::=   NameStartChar
%              | "-"
%              | "."
%              | [0-9]
%              | #xB7
%              | [#x0300-#x036F]
%              | [#x203F-#x2040]
% ~~~
%
% @compat XML 1.0.5 [4a].
% @compat XML 1.1.2 [4a].

'NameChar'(Code) --> 'NameStartChar'(Code).
'NameChar'(Code) --> hyphen_minus(Code).
'NameChar'(Code) --> dot(Code).
'NameChar'(Code) --> decimal_digit(_, Code).
% #x00B7
'NameChar'(Code) --> middle_dot(Code).
% #x0300-#x036F
'NameChar'(Code) --> between_code(hex('0300'), hex('036F'), dec(Code)).
% #x203F
'NameChar'(Code) --> undertie(Code).
% #x2040
'NameChar'(Code) --> character_tie(Code).



%! 'NameStartChar'(?Code:code)// .
% ~~~{.ebnf}
% NameStartChar ::=   ":"
%                   | [A-Z]
%                   | "_"
%                   | [a-z]
%                   | [#xC0-#xD6]
%                   | [#xD8-#xF6]
%                   | [#xF8-#x2FF]
%                   | [#x370-#x37D]
%                   | [#x37F-#x1FFF]
%                   | [#x200C-#x200D]
%                   | [#x2070-#x218F]
%                   | [#x2C00-#x2FEF]
%                   | [#x3001-#xD7FF]
%                   | [#xF900-#xFDCF]
%                   | [#xFDF0-#xFFFD]
%                   | [#x10000-#xEFFFF]
% ~~~
%
% @compat XML 1.0.5 [4].
% @compat XML 1.1.2 [4].

% [A-Z] and [a-z]
'NameStartChar'(Code) --> letter(Code).
% ":"
'NameStartChar'(Code) --> colon(Code).
% "_"
'NameStartChar'(Code) --> underscore(Code).
% #xC0-#xD6
'NameStartChar'(Code) --> between_code(hex('C0'), hex('D6'), dec(Code)).
% #xD8-#xF6
'NameStartChar'(Code) --> between_code(hex('D8'), hex('F6'), dec(Code)).
% #xF8-#x2FF
'NameStartChar'(Code) --> between_code(hex('F8'), hex('2FF'), dec(Code)).
% #x370-#x37D
'NameStartChar'(Code) --> between_code(hex('370'), hex('37D'), dec(Code)).
% #x37F-#x1FFF
'NameStartChar'(Code) --> between_code(hex('37F'), hex('1FFF'), dec(Code)).
% #x200C-#x200D
'NameStartChar'(Code) --> zero_width_non_joiner(Code).
'NameStartChar'(Code) --> zero_width_joiner(Code).
% #x2070-#x218F
'NameStartChar'(Code) --> between_code(hex('2070'), hex('218F'), dec(Code)).
% #x2C00-#x2FEF
'NameStartChar'(Code) --> between_code(hex('2C00'), hex('2FEF'), dec(Code)).
% #x3001-#xD7FF
'NameStartChar'(Code) --> between_code(hex('3001'), hex('D7FF'), dec(Code)).
% #xF900-#xFDCF
'NameStartChar'(Code) --> between_code(hex('F900'), hex('FDCF'), dec(Code)).
% #xFDF0-#xFFFD
'NameStartChar'(Code) --> between_code(hex('FDF0'), hex('FFFD'), dec(Code)).
% #x10000-#xEFFFF
'NameStartChar'(Code) --> between_code(hex('10000'), hex('EFFFF'), dec(Code)).



%! 'RestrictedChar'(?Code:code)// .
% ~~~{.ebnf}
% RestrictedChar ::=
%     \\ Start of heading, start of text, end of text, end of transmission,
%     \\ enquiry, positive acknowledgement, bell, backspace.
%       [#x1-#x8]
%
%     \\ Vertical tab, form feed.
%     | [#xB-#xC]
%
%     \\ Shift out, shift in, data link escape, device control (1, 2, 3, 4),
%     \\ negative acknowledgement, synchronous idle,
%     \\ end of transmission block, cancel, end of medium, substitute,
%     \\ escape, file separator, group separator, record separator,
%     \\ unit separator.
%     | [#xE-#x1F]
%
%     | [#x7F-#x84]
%     | [#x86-#x9F]
% ~~~
%
% @compat XML 1.1.2 [2a].

'RestrictedChar'(Code) --> between_code(hex('1'), hex('8'), dec(Code)).
'RestrictedChar'(Code) --> between_code(hex('B'), hex('Code'), dec(Code)).
'RestrictedChar'(Code) --> between_code(hex('E'), hex('1F'), dec(Code)).
'RestrictedChar'(Code) --> between_code(hex('7F'), hex('84'), dec(Code)).
'RestrictedChar'(Code) --> between_code(hex('86'), hex('9F'), dec(Code)).



%! 'S'// .
% White space.
%
% ~~~{.ebnf}
% S ::= ( #x20 | #x9 | #xD | #xA )+   // Any consecutive number of spaces,
%                                     // carriage returns, line feeds, and
%                                     // horizontal tabs.
% ~~~
%
% The presence of carriage_return// in the above production is maintained
% purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either removed
% or replaced by line_feed// (i.e., `#xA`) characters before any other
% processing is done.
%
% @compat XML 1.0.5 [3].
% @compat XML 1.1.2 [3].

'S' --> '+'('S_char', []).

'S_char' --> carriage_return.
'S_char' --> horizontal_tab.
'S_char' --> line_feed.
'S_char' --> space.
