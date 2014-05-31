:- module(
  record_jar_char,
  [
    'ASCCHAR'//2, % -Tree:compound
                  % ?Code:code
    character//2, % -Tree:compound
                  % ?Code:code
    'ESCAPE'//2, % -Tree:compound
                 % ?Code:code
    'field-name-character'//2, % -Tree:compound
                               % ?Code:code
    'UNICHAR'//2 % -Tree:compound
                 % ?Code:code
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



%! 'ASCCHAR'(-Tree:compound, ?Code:code)// .
% ASCII characters except %x26 (&) and %x5C (\).
%
% ~~~{.abnf}
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ~~~

'ASCCHAR'('ASCCHAR'(C), C) -->
  between_hex('21', '25', C).
'ASCCHAR'('ASCCHAR'(C), C) -->
  between_hex('27', '5B', C).
'ASCCHAR'('ASCCHAR'(C), C) -->
  between_hex('5D', '7E', C).


%! character(-Tree:compound, ?Code:code)// .
% ~~~{.abnf}
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ~~~
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

character(character(T), C) -->
  'ASCCHAR'(T, C).
character(character('WSP'(C)), C) -->
  'WSP'(C).
character(character(T1), C) -->
  'ESCAPE'(T1, C).
character(character(T1), C) -->
  'UNICHAR'(T1, C).


%! 'ESCAPE'(-Tree:compound, ?Code:code)// .
% ~~~{.abnf}
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ~~~

'ESCAPE'('ESCAPE'('\\',C), C) -->
  backslash,
  ( backslash(C)
  ; ampersat(C)
  ; r_lowercase(C)
  ; n_lowercase(C)
  ; t_lowercase(C)
  ).
'ESCAPE'('ESCAPE'('&#x',Codes), DecimalNumber) -->
  `&#x`,
  'm*n'(2, 6, 'HEXDIG', Codes, DecimalDigits),
  {digits_to_decimal(DecimalDigits, 16, DecimalNumber)}.


%! 'field-name-character'(-Tree:compound, ?Code:code)// .
% This rule does not occur in the specified grammar,
% but I have added it since there are additional requirements mentioned
% in the documentation.
%
% To stay close to the grammar, we display applications of this rule
% as `charcter` nodes in the parse tree.

'field-name-character'(T, C) -->
  character(T, C),
  % Explicitly exclude space// and colon//.
  {\+ memberchk(C, [32, 58])}.


%! 'UNICHAR'(-Tree:compound, ?Code:code)// .
% ~~~{.abnf}
% UNICHAR = %x80-10FFFF
% ~~~

'UNICHAR'('UNICHAR'(C), C) -->
  between_hex('80', '10FFFF', C).

