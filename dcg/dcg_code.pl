:- module(
  dcg_code,
  [
    between_code//2, % +Low:code
                     % +High:code
    between_code//3, % +Low:code
                     % +High:code
                     % ?Code:code
    between_code_radix//2, % +RadixLow:compound
                           % +RadixHigh:compound
    between_code_radix//3, % +RadixLow:compound
                           % +RadixHigh:compound
                           % -Code:code
    code//1, % ?Code:code
    code_ci//1, % ?Code:code
    code_lower//1, % ?Code:code
    code_radix//1, % ?RadixCode:compound
    code_radix//2, % ?RadixCode:compound
                   % -Code:code
    code_upper//1 % ?Code:code
  ]
).

/** <module> DCG codes

Grammar support for entering character codes.

@author Wouter Beek
@version 2014/10-2014/11
*/

:- use_module(library(apply)).

:- use_module(generics(code_ext)).
:- use_module(math(radix)).

:- use_module(plDcg(dcg_ascii)).



%! between_code(+Low:code, +High:code)// .
%! between_code(+Low:code, +High:code, ?Code:code)// .

between_code(Low, High) -->
  between_code(Low, High, _).

between_code(Low, High, Code) -->
  code(Code),
  {between(Low, High, Code)}.



%! between_code_radix(+Low:compound, +High:compound)// .
%! between_code_radix(+Low:compound, +High:compound, -Code:code)// .
% Parses or generates a code between the given numbers.

between_code_radix(Low, High) -->
  between_code_radix(Low, High, _).

between_code_radix(dec(Low), dec(High), Code) --> !,
  between_code(Low, High, Code).
between_code_radix(hex(Low1), hex(High1), Code) -->
  {maplist(radix, [Low1,High1], [Low2,High2])},
  between_code_radix(dec(Low2), dec(High2), Code).



%! code(+Code:code)// is det.
%! code(-Code:code)// is det.

code(Code) -->
  [Code].



%! code_ci(+Code:nonneg)// is multi.
%! code_ci(-Code:nonneg)// is nondet.
% Writes case-insensitive variants of the given code.
%
% ### Examples
%
% Generate the upper- and lowercase variants of a given letter.
%
% ```prolog
% ?- phrase(code_ci(oct(142)), [X]), string_codes(S, [X]).
% X = 66,
% S = "B" ;
% X = 98,
% S = "b".
% ```
%
% Parse a letter in its lower- and uppercase variants.
%
% ```prolog
% ?- string_codes("b", Codes), phrase(code_ci(hex(X)), Codes).
% Codes = [98],
% X = '42' ;
% Codes = [98],
% X = '62'.
% ```
%
% Generate all case-variants of a given string.
%
% ```prolog
% ?- phrase('*'(code_ci, "http", []), Codes).
% Codes = "http" ;
% Codes = "httP" ;
% Codes = "htTp" ;
% Codes = "htTP" ;
% Codes = "hTtp" ;
% Codes = "hTtP" ;
% Codes = "hTTp" ;
% Codes = "hTTP" ;
% Codes = "Http" ;
% Codes = "HttP" ;
% Codes = "HtTp" ;
% Codes = "HtTP" ;
% Codes = "HTtp" ;
% Codes = "HTtP" ;
% Codes = "HTTp" ;
% Codes = "HTTP" ;
% false.
% ```

code_ci(Code) -->
  {var(Code)}, !,
  code(Code0),
  {code_ci(Code0, Code)}.
code_ci(Code) -->
  {code_ci(Code, Code0)},
  code(Code0).



%! code_lower(+Code:nonneg)// is det.
%! code_lower(-Code:nonneg)// is nondet.

code_lower(Code) -->
  {var(Code)}, !,
  letter_lowercase(Code0),
  {code_ci(Code0, Code)}.
code_lower(Code) -->
  {to_lower(Code, Code0)},
  letter_lowercase(Code0).



%! code_radix(+RadixCode:compound)// .
%! code_radix(+RadixCode:compound, -Code:code)// .
% Emits a single code and allows the code to be represented
% in one of the following bases:
%   - bin(+nonneg)
%   - dec(+nonneg)
%   - hex(+atom)
%   - oct(+nonneg)

code_radix(RadixCode) -->
  code_radix(RadixCode, _).

code_radix(RadixCode, Code) -->
  {var(RadixCode)}, !,
  code(Code),
  {radix(RadixCode, dec(Code))}.
code_radix(RadixCode, Code) -->
  {radix(RadixCode, dec(Code))},
  [Code].



%! code_upper(+Code:nonneg)// is det.
%! code_upper(-Code:nonneg)// is nondet.

code_upper(Code) -->
  {var(Code)}, !,
  letter_uppercase(Code0),
  {code_ci(Code0, Code)}.
code_upper(Code) -->
  {to_upper(Code, Code0)},
  letter_uppercase(Code0).
