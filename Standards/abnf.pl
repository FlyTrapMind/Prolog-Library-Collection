:- module(
  abnf,
  [
    'ALPHA'//0,
    'ALPHA'//1,
    'BIT'//0,
    'BIT'//2, % ?DecimalDigit:between(0,1)
              % ?Code:code
    'CHAR'//0,
    'CHAR'//1,
    'CR'//0,
    'CR'//1,
    'CRLF'//0,
    'CRLF'//1, % ?Codes:list(code)
    'CTL'//0,
    'CTL'//1,
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?DecimalDigit:between(0,9)
                % ?Code:code
    'DQUOTE'//0,
    'DQUOTE'//1,
    'HEXDIG'//0,
    'HEXDIG'//2, % ?DecimalDigit:between(0,15)
                 % ?Code:code
    'HTAB'//0,
    'HTAB'//1,
    'LF'//0,
    'LF'//1,
    'LWSP'//0,
    'LWSP'//1,
    'OCTET'//0,
    'OCTET'//1,
    'SP'//0,
    'SP'//1,
    'VCHAR'//0,
    'VCHAR'//1,
    'WSP'//0,
    'WSP'//1
  ]
).

/** <module> ABNF

Support for Augmented Backus-Naur Format (ABNF).

@author Wouter Beek
@see Based on the obsoleted RFC 4234 standard, found at
     http://tools.ietf.org/html/rfc4234
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).



%! 'ALPHA'//
% @see 'ALPHA'//1

'ALPHA' -->
  letter.

%! 'ALPHA'(?Code:code)//
% The ASCII letters, i.e. =|A-Z / a-z|=.
%
% Hexadecimal character range: `%x41-5A` through `%x61-7A`.
%
% ~~~{.abnf}
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ~~~

'ALPHA'(C) -->
  letter(C).

%! 'BIT'//
% @see 'BIT'//1

'BIT' -->
  binary_digit.

%! 'BIT'(?DecimalDigit:between(0,1), ?Code:code)//
% A binary digit, i.e. `0` or `1`.
%
% ~~~{.abnf}
% BIT = "0" / "1"
% ~~~

'BIT'(D, C) -->
  binary_digit(D, C).

%! 'CHAR'//
% @see 'CHAR'//1

'CHAR' -->
  [C],
  {between(1, 127, C)}.

%! 'CHAR'(?Code:code)//
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% ~~~{.abnf}
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ~~~

'CHAR'(C) -->
  [C],
  {between(1, 127, C)}.

%! 'CR'//
% @see 'CR'//1

'CR' -->
  carriage_return.

%! 'CR'(?Code:code)//
% The carriage return.
%
% ~~~{.abnf}
% CR = %x0D   ; carriage return
% ~~~

'CR'(C) -->
  carriage_return(C).

%! 'CRLF'//
% @see 'CRLF'//1

'CRLF' -->
  'CRLF'(_Cs).

%! 'CRLF'(?Codes:list(code))//
% Internet standard newline.
%
% ~~~{.abnf}
% CRLF = CR LF   ; Internet standard newline
% ~~~

'CRLF'([C1,C2]) -->
  'CR'(C1),
  'LF'(C2).

%! 'CTL'//
% @see 'CTL'//1

'CTL' -->
  control.

%! 'CTL'(?Code:code)//
% Control characters.
%
% ~~~{.abnf}
% CTL = %x00-1F / %x7F   ; controls
% ~~~

'CTL'(C) -->
  control(C).

%! 'DIGIT'//
% @see 'DIGIT'//2

'DIGIT' -->
  decimal_digit.

%! 'DIGIT'(?Code:code)//
% @see 'DIGIT'//2

'DIGIT'(C) -->
  decimal_digit(C).

%! 'DIGIT'(?DecimalDigit:integer, ?Code:code)//
% Decimal digits.
%
% ~~~{.abnf}
% DIGIT = %x30-39   ; 0-9
% ~~~

'DIGIT'(D, C) -->
  decimal_digit(D, C).

%! 'DQUOTE'//
% @see 'DQUOTE'//1

'DQUOTE' -->
  double_quote.

%! 'DQUOTE'(?Code:code)//
%
% ~~~{.abnf}
% DQUOTE = %x22   ; " (Double Quote)
% ~~~

'DQUOTE'(C) -->
  double_quote(C).

%! 'HEXDIG'//
% @see 'HEXDIG'//1

'HEXDIG' -->
  hexadecimal_digit.

%! 'HEXDIG'(?DecimalDigit:between(0,15), ?Code:code)//
% Hexadecimal digits.
%
% ~~~{.abnf}
% HEXDIG =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ~~~

'HEXDIG'(D, C) -->
  hexadecimal_digit(D, C).

%! 'HTAB'//
% @see 'HTAB'//1

'HTAB' -->
  horizontal_tab.

%! 'HTAB'(?Code:code)//
%
% ~~~{.abnf}
% HTAB = %x09   ; horizontal tab
% ~~~

'HTAB'(C) -->
  horizontal_tab(C).

%! 'LF'//
% @see 'LF'//1

'LF' -->
  line_feed.

%! 'LF'(?Code:code)//
%
% ~~~{.abnf}
% LF = %x0A   ; linefeed
% ~~~

'LF'(C) -->
  line_feed(C).

%! 'LWSP'//
% @see 'LWSP'//

'LWSP' -->
  'LWSP'(_C).

%! 'LWSP'(?Code:code)//
%
% ~~~{.abnf}
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ~~~

'LWSP'([H|T]) -->
  'WSP'(H),
  'LWSP'(T).
'LWSP'([H|T]) -->
  'CRLF'(H),
  'WSP'(T).
'LWSP'([]) -->
  [].

%! 'OCTET'//
% @see 'OCTET'//1

'OCTET' -->
  'OCTET'(_C).

%! 'OCTET'(?Code:code)//
%
% ~~~{.abnf}
% OCTET = %x00-FF   ; 8 bits of data
% ~~~

'OCTET'(C) -->
  [C],
  {between(0, 255, C)}.

%! 'SP'//
% @see 'SP'//1

'SP' -->
  space.

%! 'SP'(?Code:code)//
%
% ~~~{.abnf}
% SP = %x20
% ~~~

'SP'(C) -->
  space(C).

%! 'VCHAR'//
% @see 'VCHAR'//1

'VCHAR' -->
  dcg_graph.

%! 'VCHAR'(?Code:code)//
%
% ~~~{.abnf}
% VCHAR = %x21-7E   ; visible (printing) characters
% ~~~

'VCHAR'(C) -->
  dcg_graph(C).

%! 'WSP'//
% @see 'WSP'//1

'WSP' -->
  'WSP'(_C).

%! 'WSP'(?Code:code)//
%
% ~~~{.abnf}
% WSP = SP / HTAB   ; white space
% ~~~

'WSP'(C) -->
  'SP'(C).
'WSP'(C) -->
  'HTAB'(C).

