:- module(
  dcg_cardinal,
  [
    binary_digit//0,
    binary_digit//1, % ?DecimalDigit:between(0,1)
    binary_digit//2, % ?DecimalDigit:between(0,1)
                     % ?Code:code
    binary_number//1, % ?DecimalNumber:integer
    decimal_digit//0,
    decimal_digit//1, % ?DecimalDigit:between(0,9)
    decimal_digit//2, % ?DecimalDigit:between(0,9)
                      % ?Code:code
    decimal_number//1, % ?DecimalNumber:integer
    exponent//0,
    exponent_sign//0,
    exponent_sign//1, % ?Code:code
    hexadecimal_digit//0,
    hexadecimal_digit//1, % ?DecimalDigit:between(0,15)
    hexadecimal_digit//2, % ?DecimalDigit:between(0,15)
                          % ?Code:code
    hexadecimal_number//1, % -DecinalNumber:integer
    octal_digit//0,
    octal_digit//1, % ?DecimalDigit:between(0,7)
    octal_digit//2, % ?DecimalDigit:between(0,7)
                    % ?Code:code
    octal_number//1, % -DecinalNumber:integer
    sign//0,
    sign//1, % ?Sign:oneof([-1,1])
    signed_number//1, % ?SignedNumber:float
    unsigned_number//1 % ?UnsignedNumber:float
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
@version 2013/06-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(math(math_ext)).
:- use_module(math(radix)).

:- meta_predicate(digits_to_decimal_number(//,+,?,?,?)).
:- meta_predicate(digits_to_decimal_number(//,+,+,?,?,?)).



%! binary_digit//

binary_digit --> zero.
binary_digit --> one.

%! binary_digit(?DecimalDigit:between(0,1))//

binary_digit(0) --> zero.
binary_digit(1) --> one.

%! binary_digit(?DecimalDigit:between(0,1), ?Code:code)//

binary_digit(0, C) --> zero(C).
binary_digit(1, C) --> one(C).

%! binary_number(-DecimalNumber:integer)//

binary_number(N) -->
  digits_to_decimal_number(binary_digit, 2, N).

%! decimal_digit//

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.

%! decimal_digit(?DecimalDigit:between(0,9))//

decimal_digit(N) --> octal_digit(N).
decimal_digit(8) --> eight.
decimal_digit(9) --> nine.

%! decimal_digit(?DecimalDigit:between(0,9), ?Code:code)//

decimal_digit(N, C) --> octal_digit(N, C).
decimal_digit(8, C) --> eight(C).
decimal_digit(9, C) --> nine(C).

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
  % We keep track of the decimal equivalent if the digits that we have
  % seen so far, in order to do the radix multiplication with.
  digits_to_decimal_number(Digit, Radix, N, M).

% End of code segment, the decimal number we have built so far is the result.
digits_to_decimal_number(_Digit, _Radix, M, M) --> [].
digits_to_decimal_number(Digit, Radix, M1, M) -->
  % Process the next digit.
  dcg_call(Digit, N),
  % Perform radix multiplication.
  {M2 is M1 * Radix + N},
  digits_to_decimal_number(Digit, Radix, M2, M).

exponent -->
  exponent_sign,
  dcg_plus(decimal_digit).

exponent_sign --> e.

exponent_sign(C) --> e(C).

%! hexadecimal_digit//

hexadecimal_digit --> decimal_digit.
hexadecimal_digit --> a.
hexadecimal_digit --> b.
hexadecimal_digit --> c.
hexadecimal_digit --> d.
hexadecimal_digit --> e.
hexadecimal_digit --> f.

%! hexadecimal_digit(?DecimalNumber:between(0,15))//

hexadecimal_digit(N) --> decimal_digit(N).
hexadecimal_digit(10) --> a.
hexadecimal_digit(11) --> b.
hexadecimal_digit(12) --> c.
hexadecimal_digit(13) --> d.
hexadecimal_digit(14) --> e.
hexadecimal_digit(15) --> f.

%! hexadecimal_digit(?DecimalNumber:between(0,15), ?Code:code)//

hexadecimal_digit(N, C) --> decimal_digit(N, C).
hexadecimal_digit(10, C) --> a(C).
hexadecimal_digit(11, C) --> b(C).
hexadecimal_digit(12, C) --> c(C).
hexadecimal_digit(13, C) --> d(C).
hexadecimal_digit(14, C) --> e(C).
hexadecimal_digit(15, C) --> f(C).

%! hexadecimal_number(-DecimalDigit:integer)//

hexadecimal_number(N) -->
  digits_to_decimal_number(hexadecimal_digit, 16, N).

%! octal_digit//

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.

%! octal_digit(?DecimalDigit:between(0,7))//

octal_digit(N) --> binary_digit(N).
octal_digit(2) --> two.
octal_digit(3) --> three.
octal_digit(4) --> four.
octal_digit(5) --> five.
octal_digit(6) --> six.
octal_digit(7) --> seven.

%! octal_digit(?DecimalDigit:between(0,7), ?Code:code)//

octal_digit(N, C) --> binary_digit(N, C).
octal_digit(2, C) --> two(C).
octal_digit(3, C) --> three(C).
octal_digit(4, C) --> four(C).
octal_digit(5, C) --> five(C).
octal_digit(6, C) --> six(C).
octal_digit(7, C) --> seven(C).

%! octal_number(-DecimalDigit:integer)//

octal_number(N) -->
  digits_to_decimal_number(octal_digit, 8, N).

sign --> minus_sign.
sign --> plus_sign.

sign(-1) --> minus_sign.
sign(1) --> plus_sign.

signed_number(N, H, T):-
  number(N), !,
  format(codes(H, T), '~w', [N]).
signed_number(N) -->
  unsigned_number(N).
signed_number(N) -->
  sign(Sg),
  unsigned_number(N1),
  {N is Sg * N1}.

unsigned_number(N, H, T):-
  number(N), !,
  format(codes(H, T), '~w', [N]).
unsigned_number(N) -->
  decimal_number(N).
unsigned_number(N) -->
  ("", {N1 = 0} ; decimal_number(N1)),
  dot,
  ("", {N2 = 0} ; decimal_number(N2)),
  {integers_to_float(N1, N2, N)}.
