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
@version 2013/07, 2014/05, 2014/10-2014/11
*/

:- use_module(math(radix)).

:- use_module(plDcg(abnf_core_rules)).
:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_code)).



%! 'ASCCHAR'(-Tree:compound, ?Code:code)// .
% ASCII characters except %x26 (&) and %x5C (\).
%
% ```abnf
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ```

'ASCCHAR'('ASCCHAR'(Code), Code) -->
  between_code_radix(hex('21'), hex('25'), Code).
'ASCCHAR'('ASCCHAR'(Code), Code) -->
  between_code_radix(hex('27'), hex('5B'), Code).
'ASCCHAR'('ASCCHAR'(Code), Code) -->
  between_code_radix(hex('5D'), hex('7E'), Code).



%! character(-Tree:compound, ?Code:code)// .
% ```abnf
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ```
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

character(character(T), Code) -->
  'ASCCHAR'(T, Code).
character(character('WSP'(Code)), Code) -->
  'WSP'(Code).
character(character(T1), Code) -->
  'ESCAPE'(T1, Code).
character(character(T1), Code) -->
  'UNICHAR'(T1, Code).



%! 'ESCAPE'(-Tree:compound, ?Code:code)// .
% ```abnf
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ```

'ESCAPE'('ESCAPE'('\\',Code), Code) -->
  "\\",
  (   backslash(Code)
  ;   ampersat(Code)
  ;   r_lowercase(Code)
  ;   n_lowercase(Code)
  ;   t_lowercase(Code)
  ).
'ESCAPE'('ESCAPE'('&#x',Number), Number) -->
  "&#x",
  'm*n'(2, 6, 'HEXDIG', Weights, []),
  {weights_radix(Weights, dec(Number))}.



%! 'field-name-character'(-Tree:compound, ?Code:code)// .
% This rule does not occur in the specified grammar,
% but I have added it since there are additional requirements mentioned
% in the documentation.
%
% To stay close to the grammar, we display applications of this rule
% as `charcter` nodes in the parse tree.

'field-name-character'(T, Code) -->
  character(T, Code),
  % Explicitly exclude space// and colon//.
  {\+ memberchk(Code, [32, 58])}.



%! 'UNICHAR'(-Tree:compound, ?Code:code)// .
% ```abnf
% UNICHAR = %x80-10FFFF
% ```

'UNICHAR'('UNICHAR'(Code), Code) -->
  between_code_radix(hex('80'), hex('10FFFF'), Code).
