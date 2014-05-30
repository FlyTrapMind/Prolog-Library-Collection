:- module(
  dcg_cardinal,
  [
    between_decimal_digit//2, % +LowDecimal:between(0,9)
                              % +HighDecimal:between(0,9)
    between_decimal_digit//4, % +LowDecimal:between(0,9)
                              % +HighDecimal:between(0,9)
                              % ?Code:nonneg
                              % ?DecimalDigit:between(0,9)
    binary_digit//2, % ?Code:code
                     % ?DecimalDigit:between(0,1)
    binary_number//1, % ?DecimalNumber:integer
    decimal_digit//2, % ?Code:code
                      % ?DecimalDigit:between(0,9)
    decimal_number//1, % ?DecimalNumber:integer
    decimal_fraction//1, % ?Decimal:between(0.0,1.0)
    exponent//0,
    exponent_sign//0,
    hexadecimal_digit//2, % ?Code:code
                          % ?DecimalDigit:between(0,15)
    hexadecimal_number//1, % -DecinalNumber:integer
    int_codes//1, % ?Codes:list(code)
    'integer?'//1, % ?Integer:integer
    octal_digit//2, % ?Code:code
                    % ?DecimalDigit:between(0,7)
    octal_number//1, % -DecinalNumber:integer
    sign//1, % ?Sign:integer
    'sign?'//1 % ?Sign:oneof([-1,1])
  ]
).
:- reexport(
  library(dcg/basics),
  [
    digit//1,
    digits//1,
    float//1,
    integer//1,
    number//1,
    xdigit//1,
    xdigits//1,
    xinteger//1
  ]
).

/** <module> DCG_CARDINAL

DCGs for cardinal numbers.

@author Wouter Beek
@version 2013/06-2013/09, 2014/05
*/

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(typecheck)).
:- use_module(math(math_ext)).

:- meta_predicate(digits_to_decimal_number(//,+,?,?,?)).
:- meta_predicate(digits_to_decimal_number(//,+,+,?,?,?)).



%! between_decimal_digit(
%!   +LowDecimal:between(0,9),
%!   +HighDecimal:between(0,9)
%! )// .

between_decimal_digit(LowDec, HighDec) -->
  between_decimal_digit(LowDec, HighDec, _, _).

%! between_decimal_digit(
%!   +LowDecimal:between(0,9),
%!   +HighDecimal:between(0,9),
%!   ?Code:nonneg,
%!   ?DecimalDigit:between(0,9)
%! )// .

between_decimal_digit(LowDec, HighDec, Code, Digit) -->
  {between(LowDec, HighDec, Digit)},
  decimal_digit(Code, Digit).


%! binary_digit(?Code:code, ?DecimalDigit:between(0,1))//

binary_digit(C, 0) --> zero(C).
binary_digit(C, 1) --> one(C).

%! binary_number(-DecimalNumber:integer)//

binary_number(N) -->
  digits_to_decimal_number(binary_digit, 2, N).


%! decimal_digit(?Code:code, ?DecimalDigit:between(0,9))//

decimal_digit(C, N) -->
  {var(C), var(N)},
  decimal_digit_nondet(C, N).
decimal_digit(C, N) -->
  decimal_digit_nondet(C, N), !.

decimal_digit_nondet(C, N) --> octal_digit(C, N).
decimal_digit_nondet(C, 8) --> eight(C).
decimal_digit_nondet(C, 9) --> nine(C).


%! decimal_fraction(-Decimal:between(0.0,1.0))// .

decimal_fraction(Decimal) -->
  decimal_fraction(0, 1, Decimal).

decimal_fraction(Sum1, Index1, Decimal) -->
  digit(Digit), !,
  {Sum2 is Sum1 + Digit * 10 ** (-1 * Index1)},
  {Index2 is Index1 + 1},
  decimal_fraction(Sum2, Index2, Decimal).
decimal_fraction(Decimal, _, Decimal) --> [].


%! decimal_number(-DecimalNumber:integer)//

decimal_number(N) -->
  digits_to_decimal_number(decimal_digit, 10, N).

%! digits_to_decimal_number(
%!   :DCGBody,
%!   +Radix:integer,
%!   ?DecimalNumber:integer
%! )//
% Processes digits of arbitrary radix and returns the decimal equivalent.
%
% @arg DCGBody processes a single digit if the given radix.
% @arg Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @arg An integer representing the processed number, converted to
%      the decimal number system.

digits_to_decimal_number(_Digit, Radix, M, H, T):-
  number(M), !,
  atomic_list_concat(['~', Radix, r], Format),
  format(codes(H, T), Format, [M]).
digits_to_decimal_number(Digit, Radix, M) -->
  % We start with processing the first digit.
  dcg_call(Digit, N),
  % We keep track of the decimal equivalent of the digits that we have
  % seen so far, in order to do the radix multiplication with.
  digits_to_decimal_number(Digit, Radix, N, M).

% Look for the next number...
digits_to_decimal_number(Digit, Radix, M1, M) -->
  % Process the next digit.
  dcg_call(Digit, N),
  % Perform radix multiplication.
  {M2 is M1 * Radix + N},
  digits_to_decimal_number(Digit, Radix, M2, M).
% End of code segment, the decimal number we have built so far is the result.
digits_to_decimal_number(_Digit, _Radix, M, M) --> [].


exponent -->
  exponent_sign,
  '+'(decimal_digit).


exponent_sign --> e.


%! hexadecimal_digit(?Code:code, ?DecimalNumber:between(0,15))//

hexadecimal_digit(C, N) --> decimal_digit(C, N).
hexadecimal_digit(C, 10) --> a(C).
hexadecimal_digit(C, 11) --> b(C).
hexadecimal_digit(C, 12) --> c(C).
hexadecimal_digit(C, 13) --> d(C).
hexadecimal_digit(C, 14) --> e(C).
hexadecimal_digit(C, 15) --> f(C).

%! hexadecimal_number(-DecimalDigit:integer)//

hexadecimal_number(N) -->
  digits_to_decimal_number(hexadecimal_digit, 16, N).

%! int_codes(?Codes:list(code))//
% A positive number of digits, possibly followed by a sign.

int_codes([C,D0|D]) -->
  sign(C), !,
  digit(D0),
  digits(D).
int_codes([D0|D]) -->
  digit(D0),
  digits(D).


%! 'integer?'(?Integer:integer)// .
% Sometimes, we want to allow an integer to occur
% optionally.
% It is often easy to re

% Read integer I, including 0.
'integer?'(I) -->
  {var(I)},
  integer(I), !.
% Read 0 for the empty list.
'integer?'(I) -->
  {var(I)},
  [], !,
  {I = 0}.
% Write 0: [].
'integer?'(I) -->
  {I == 0}, !,
  [].
% Write I > 0: I.
'integer?'(I) -->
  integer(I).


%! octal_digit(?Code:code, ?DecimalDigit:between(0,7))//

octal_digit(C, N) --> binary_digit(C, N).
octal_digit(C, 2) --> two(C).
octal_digit(C, 3) --> three(C).
octal_digit(C, 4) --> four(C).
octal_digit(C, 5) --> five(C).
octal_digit(C, 6) --> six(C).
octal_digit(C, 7) --> seven(C).

%! octal_number(-DecimalDigit:integer)//

octal_number(N) -->
  digits_to_decimal_number(octal_digit, 8, N).

%! sign(+Sign:integer)// .
%! sign(-Sign:oneof([-1,1]))// .
% Mapping: {〈{i ∈ N | i ≧ 0}, 1〉,〈{i ∈ N | i < 0}, -1〉}
% Canonical inverse: {〈1,1〉,〈-1,-1〉}

sign(I) -->
  {negative_integer(I)}, !,
  `-`.
sign(I) -->
  {nonneg(I)}, !,
  `+`.
sign(-1) -->
  `-`.
sign(1) -->
  `+`.


%! 'sign?'(+Sign:integer)// .
%! 'sign?'(-Sign:oneof([-1,1]))// .
% Like sign//1, but the plus sign is not written,
% and the plus sign is returned if no sign can be read.

% Do not write a sign for positive integers.
'sign?'(Sign) -->
  {positive_integer(Sign)}, !,
  [].
% Write the minus sign based on a negative integer,
% or read a sign, either negative or positive.
'sign?'(Sign) -->
  sign(Sign).
% Read no sign, return +1.
'sign?'(Sign) -->
  {var(Sign)},
  [],
  {Sign = 1}.

