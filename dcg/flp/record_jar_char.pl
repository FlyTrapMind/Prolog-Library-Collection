:- module(
  record_jar_char,
  [
    'ASCCHAR'//1, % ?Code:code
    character//1, % ?Code:code
    'ESCAPE'//1, % ?Code:code
    'field-name-character'//1, % ?Code:code
    'UNICHAR'//1 % ?Code:code
  ]
).

/** <module> Reocrd Jar characters

DCGs for characters that occur in the Record Jar representation format.

@author Wouter Beek
@version 2013/07, 2014/05
*/

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(flp(rfc4234_basic)).
:- use_module(math(radix)).



%! 'ASCCHAR'(?Code:code)// .
% ASCII characters except %x26 (&) and %x5C (\).
%
% ~~~{.abnf}
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ~~~

'ASCCHAR'(C) -->
  between_hex('21', '25', C).
'ASCCHAR'(C) -->
  between_hex('27', '5B', C).
'ASCCHAR'(C) -->
  between_hex('5D', '7E', C).


%! character(?Code:code)// .
% ~~~{.abnf}
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ~~~
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

character(C) -->
  'ASCCHAR'(C).
character(C) -->
  'WSP'(C).
character(C) -->
  'ESCAPE'(C).
character(C) -->
  'UNICHAR'(C).


%! 'ESCAPE'(?Code:code)// .
% ~~~{.abnf}
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ~~~

'ESCAPE'(C) -->
  backslash,
  ( backslash(C)
  ; ampersat(C)
  ; r_lowercase(C)
  ; n_lowercase(C)
  ; t_lowercase(C)
  ).
'ESCAPE'(DecimalNumber) -->
  `&#x`,
  'm*n'(2, 6, 'HEXDIG', DecimalDigits),
  {digits_to_decimal(DecimalDigits, 16, DecimalNumber)}.


%! 'field-name-character'(?Code:code)// .
% This rule does not occur in the specified grammar,
% but I have added it since there are additional requirements mentioned
% in the documentation.

'field-name-character'(C) -->
  character(C),
  % Explicitly exclude space// and colon//.
  {\+ memberchk(C, [32, 58])}.


%! 'UNICHAR'(?Code:code)// .
% ~~~{.abnf}
% UNICHAR = %x80-10FFFF
% ~~~

'UNICHAR'(C) -->
  between_hex('80', '10FFFF', C).

