:- module(
  rfc2234,
  [
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'BIT'//0,
    'BIT'//1, % ?Code:code
    'BIT'//2, % ?Code:code
              % ?DecimalDigit:between(0,1)
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?Code:code
                % ?DecimalDigit:between(0,9)
    'DQUOTE'//0,
    'HEXDIG'//0,
    'HEXDIG'//1, % ?Code:code
    'HEXDIG'//2, % ?Code:code
                 % ?DecimalNumber:between(0,15)
    'HTAB'//0,
    'HTAB'//1, % ?Code:code
    'LF'//0,
    'LWSP'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'VCHAR'//0,
    'VCHAR'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).

/** <module> RFC 2234

@author Wouter Beek
@see [RFC 2234](http://tools.ietf.org/html/rfc2234)
@version 2014/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).



%! 'ALPHA'// .
%! 'ALPHA'(?Code:code)// .
% Alphabetic character, i.e. belonging to
% the range of ASCII letters, =|A-Z / a-z|=.
%
% Hexadecimal character range: =|%x41-5A|= through =|%x61-7A|=.
%
% ~~~{.abnf}
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ~~~

'ALPHA' -->
  'ALPHA'(_).

'ALPHA'(C) -->
  ascii_letter(C).


%! 'BIT'// .
%! 'BIT'(?Code:code)// .
%! 'BIT'(?Code:code, ?DecimalDigit:between(0,1))// .
% A binary digit, i.e. `0` or `1`.
%
% ~~~{.abnf}
% BIT = "0" / "1"
% ~~~

'BIT' -->
  'BIT'(_).

'BIT'(C) -->
  'BIT'(C, _).

'BIT'(C, D) -->
  binary_digit(C, D).


%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% ~~~{.abnf}
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ~~~

'CHAR' -->
  'CHAR'(_).

'CHAR'(C) -->
  [C],
  {between(1, 127, C)}.


%! 'CR'// .
% The carriage return.
%
% ~~~{.abnf}
% CR = %x0D   ; carriage return
% ~~~

'CR' -->
  carriage_return.


%! 'CRLF'// .
% Internet standard newline.
%
% ~~~{.abnf}
% CRLF = CR LF   ; Internet standard newline
% ~~~

'CRLF' -->
  'CR',
  'LF'.


%! 'CTL'// .
%! 'CTL'(?Code:code)// .
% Control character.
%
% ~~~{.abnf}
% CTL = %x00-1F / %x7F   ; controls
% ~~~

'CTL' -->
  'CTL'(_).

'CTL'(C) -->
  control(C).


%! 'DIGIT'// .
%! 'DIGIT'(?Code:code)// .
%! 'DIGIT'(?Code:code, ?DecimalDigit:between(0,9))// .
% Decimal digit.
%
% ~~~{.abnf}
% DIGIT = %x30-39   ; 0-9
% ~~~

'DIGIT' -->
  'DIGIT'(_).

'DIGIT'(C) -->
  'DIGIT'(C, _).

'DIGIT'(C, D) -->
  decimal_digit(C, D).


%! 'DQUOTE'// .
% US-ASCII double-quote mark.
%
% ~~~
% DQUOTE = %x22   ; " (Double Quote)
% ~~~

'DQUOTE' -->
  double_quote.


%! 'HEXDIG'// .
%! 'HEXDIG'(?Code:code)// .
%! 'HEXDIG'(?Code:code, ?DecimalNumber:between(0.15))// .
% Hexadecimal digit.
%
% ~~~{.abnf}
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ~~~

'HEXDIG' -->
  'HEXDIG'(_).

'HEXDIG'(C) -->
  'HEXDIG'(C, _).

'HEXDIG'(C, D) --> 'DIGIT'(C, D).
'HEXDIG'(C, 10) --> a_lowercase(C).
'HEXDIG'(C, 11) --> b_lowercase(C).
'HEXDIG'(C, 12) --> c_lowercase(C).
'HEXDIG'(C, 13) --> d_lowercase(C).
'HEXDIG'(C, 14) --> e_lowercase(C).
'HEXDIG'(C, 15) --> f_lowercase(C).


%! 'HTAB'// .
%! 'HTAB'(?Code:code)// .
% The horizontal tab.
%
% ~~~{.abnf}
% HTAB = %x09   ; horizontal tab
% ~~~

'HTAB' -->
  'HTAB'(_).

'HTAB'(C) -->
  horizontal_tab(C).


%! 'LF'// .
% The linefeed.
%
% ~~~{.abnf}
% LF = %x0A   ; linefeed
% ~~~

'LF' -->
  line_feed.


%! 'LWSP'// .
% Linear white space.
%
% ~~~{.abnf}
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ~~~

'LWSP' -->
  'WSP',
  'LWSP'.
'LWSP' -->
  'CRLF',
  'WSP'.
'LWSP' -->
  [].


%! 'OCTET'// .
%! 'OCTET'(?Code:code)// .
% An octect, i.e. 8 bits of data.
%
% ~~~{.abnf}
% OCTET = %x00-FF   ; 8 bits of data
% ~~~

'OCTET' -->
  'OCTET'(_).

'OCTET'(C) -->
  [C],
  {between(0, 255, C)}.


%! 'SP'// .
%! 'SP'(?Code:code)// .
% The space.
%
% ~~~{.abnf}
% SP = %x20
% ~~~

'SP' -->
  'SP'(_).

'SP'(C) -->
  space(C).


%! 'VCHAR'// .
%! 'VCHAR'(?Code:code)// .
% Visible characters.
%
% ~~~{.abnf}
% VCHAR = %x21-7E   ; visible (printing) characters
% ~~~

'VCHAR' -->
  'VCHAR'(_).

'VCHAR'(C) -->
  ascii_graphic(C).


%! 'WSP'// .
% Whitesapace, defined as sequences of space and horizontal tab.
%
% ~~~{.abnf}
% WSP = SP / HTAB   ; white space
% ~~~

'WSP' -->
  'WSP'(_).

'WSP'(C) -->
  'SP'(C).
'WSP'(C) -->
  'HTAB'(C).

