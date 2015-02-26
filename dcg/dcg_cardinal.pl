:- module(
  dcg_cardinal,
  [
    between//2, % +Low:integer
                % +High:integer
    between//3, % +Low:integer
                % +High:integer
                % ?Value:integer
    between_digit//2, % +Low:hex
                      % +High:hex
    between_digit//3, % +Low:hex
                      % +High:hex
                      % -Value:hex
    between_digit//4, % +Low:hex
                      % +High:hex
                      % -Value:hex
                      % -Code:code
    between_radix//2, % +Low:compound
                      % +High:compound
    between_radix//3, % +Low:compound
                      % +High:compound
                      % ?Value:compound
    between_radix//4, % +Low:compound
                      % +High:compound
                      % ?Value:compound
                      % ?Code:code
    binary_number//1, % ?Value:compound
    decimal_number//1, % ?Value:compound
    digit//1, % ?Value:hex
    exponent//1, % ?Value:compound
    exponent_sign//0,
    hexadecimal_number//1, % ?Value:compound
    octal_number//1, % ?Value:compound
    sign//1, % ?Sign:number
    sign_negative//1, % ?Sign:number
    signed_number//1 % ?Number:number
  ]
).
:- reexport(
  library(dcg/basics),
  [
    float//1,
    integer//1,
    number//1
  ]
).

/** <module> Processing cardinal numbers in DCGs

Library `dcg/basics` comes with the following rules for cardinals:
  - `float//1`
  - `integer//1`
  - `number//1`

Sometimes we want to process integers between a given lower and upper bound:

  - between//[2,3]
    Processes integers between the given lower and higher bounds.
  - between_digit//[2,3]
    Processes digits between the given lower and higher bounds.
    `hex` is defined as `or([between(0,9),oneof([a,b,c,d,e,f])])`.
  - between_radix//[2,3]
    The values are either integers (in decimal base) or compound terms
    (`bin/1`, `oct/1`, `dec/1`, `hex/1`)
    representing numbers in different bases
    (binary, octal, hexadecimal).

### Example:

```prolog
?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
X = 11,
Codes = [57] ;
X = 12,
Codes = [49, 48] ;
X = 13,
Codes = [49, 49] ;
X = 14,
Codes = [49, 50] ;
X = 15,
Codes = [49, 51] ;
X = 16,
Codes = [49, 52] ;
X = 17,
Codes = [49, 53].
```

Besides integers (base 10) we sometimes want to write digits and numbers in
other bases (2, 8, 16).
The following DCG rules process numbers in different bases,
where the value can be given/returned in a specified base
(`bin/1`, `oct/1`, `dec/1`, `hex/1`):

  - `binary_number(?Value:compound)`
  - `decimal_number(?Value:compound)`
  - `octal_number(?Value:compound)`
  - `hexadecimal_number(?Value:compound)`

Other rules in this module:

  - `decimal_fraction(?Fraction:between(0.0,1.0))`
  - `exponent(?Value:compound)`
  - `exponent_sign`
  - `sign(?Sign:number)`
  - `signed_number(?Number:number)`

--

@author Wouter Beek
@version 2013/06-2013/09, 2014/05, 2014/10-2014/11
*/

:- use_module(library(error)).

:- use_module(generics(typecheck)).
:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).




%! between(+Low:integer, +High:integer)// .
%! between(+Low:integer, +High:integer, ?Value:integer)// .
% Process integers between the given lower and higher bounds.

between(Low, High) -->
  between(Low, High, _).

between(Low, High, Value) -->
  integer(Value),
  {between(Low, High, Value)}.



%! between_digit(+LowWeight:hex, +HighWeight:hex)// .
%! between_digit(+LowWeight:hex, +HighWeight:hex, -Value:hex)// .
%! between_digit(+LowWeight:hex, +HighWeight:hex, -Value:hex, -Code:code)// .
% Process digits between the given lower and higher bounds.
%
% This supports digits of hexadecimal radix.

between_digit(Low, Hight) -->
  between_digit(Low, Hight, _).

between_digit(Low, High, Value) -->
  between_digit(Low, High, Value, _).

between_digit(Low, _, _, _) -->
  {\+ hexadecimal_digit(Low)}, !,
  {type_error(hex, Low)}.
between_digit(_, High, _, _) -->
  {\+ hexadecimal_digit(High)}, !,
  {type_error(hex, High)}.
between_digit(Low, High, Value, Code) -->
  between_radix(hex(Low), hex(High), hex(Value), Code).



%! between_radix(+Low:compound, +High:compound)// .
%! between_radix(+Low:compound, +High:compound, ?Value:compound)// .
%! between_radix(+Low:compound, +High:compound, ?Value:compound, ?Code:code)// .
% Process integers that are specified in various bases
% (binary, octal, decimal, hexadecimal)
% and that are between the lower and higher bounds.
%
% ### Example
%
% ```prolog
% ?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
% X = 11,
% Codes = [57] ;
% X = 12,
% Codes = [49, 48] ;
% X = 13,
% Codes = [49, 49] ;
% X = 14,
% Codes = [49, 50] ;
% X = 15,
% Codes = [49, 51] ;
% X = 16,
% Codes = [49, 52] ;
% X = 17,
% Codes = [49, 53].
% ```

between_radix(Low, High) -->
  between_radix(Low, High, _).

between_radix(Low, High, Value) -->
  between_radix(Low, High, Value, _).

between_radix(Low, High, Value, ValueDec) -->
  {
    radix(Low, dec(LowDec)),
    radix(High, dec(HighDec)),
    between(LowDec, HighDec, ValueDec)
  },
  integer(ValueDec),
  {radix(dec(ValueDec), Value)}.



%! binary_number(?Value:compound)//

binary_number(Value) -->
  {ground(Value)}, !,
  {weights_radix(Weights, Value)},
  '*'(binary_digit, Weights, []).
binary_number(Value) -->
  '*'(binary_digit, Weights, []),
  {weights_radix(Weights, Value)}.



%! decimal_fraction(-Fraction:between(0.0,1.0))// .
% Read a decimal fraction.

decimal_fraction(Fraction) -->
  '*'(decimal_digit, Weights, []),
  {weights_fraction(Weights, Fraction)}.



%! decimal_number(?Value:compound)// .

decimal_number(Value) -->
  between_radix(Value, Value).



%! digit(+Digit:hex)// .

digit(Digit) -->
  between_digit(Digit, Digit).



%! exponent(?Value:compound)// .

exponent(Value) -->
  exponent_sign,
  decimal_number(Value).


%! exponent_sign// .

exponent_sign --> e_lowercase.



%! hexadecimal_number(?Value:compound)//

hexadecimal_number(Value) -->
  {ground(Value)}, !,
  {weights_radix(Weights, Value)},
  '*'(hexadecimal_digit, Weights, []).
hexadecimal_number(Value) -->
  '*'(hexadecimal_digit, Weights, []),
  {weights_radix(Weights, Value)}.



%! octal_number(?Value:compound)// .

octal_number(Value) -->
  {ground(Value)}, !,
  {weights_radix(Weights, Value)},
  '*'(octal_digit, Weights, []).
octal_number(Value) -->
  '*'(octal_digit, Weights, []),
  {weights_radix(Weights, Value)}.



%! sign(+Sign:number)// .
%! sign(-Sign:oneof([-1,1]))// .
% Parses and generates both signs.
%
% Mapping: {〈{i ∈ N | i ≧ 0}, 1〉,〈{i ∈ N | i < 0}, -1〉}
% Canonical inverse: {〈1,1〉,〈-1,-1〉}

sign(Sign) -->
  sign(Sign, true).



%! sign_negative(+Sign:number)// .
%! sign_negative(-Sign:oneof([-1,1]))// .
% Parser both signs but only generates a negative sign.

sign_negative(Sign) -->
  sign(Sign, false).



%! signed_number(+Number:number)// .
%! signed_number(-Number:number)// .

signed_number(Number) -->
  {var(Number)}, !,
  sign(Sign),
  integer(Integer0),
  {Number is Sign * Integer0}.
signed_number(Number) -->
  % Due to the flexible input format of sign//1 we can use the number itself
  % rather than its sign (i.e., `-1` or `1`).
  sign(Number),
  {Integer0 is abs(Number)},
  integer(Integer0).



% HELPERS

%! sign(+Sign:number, +GeneratePositive:boolean)// .
%! sign(-Sign:oneof([-1,1]), +GeneratePositive:boolean)// .

sign(Sg, _) -->
  {var(Sg)}, !,
  parse_sign(Sg).
sign(N, GeneratePositive) -->
  generate_sign(N, GeneratePositive).

parse_sign(-1) --> "-".
parse_sign(1) --> "+".

generate_sign(N, _) -->
  {negative_integer(N)}, !,
  "-".
generate_sign(N, false) -->
  {nonneg(N)}, !,
  "+".
