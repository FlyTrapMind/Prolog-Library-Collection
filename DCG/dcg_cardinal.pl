:- module(
  dcg_cardinal,
  [
    binary_digit//0,
    binary_digit//2, % ?DecimalDigit:between(0,1)
                     % ?Code:code
    decimal_digit//0,
    decimal_digit//2, % ?DecimalDigit:between(0,9)
                      % ?Code:code
    decimal_number//2, % ?DecimalNumber:integer
                       % ?Codes:list(code)
    exponent//0,
    exponent_sign//0,
    exponent_sign//1, % ?Code:code
    hexadecimal_digit//0,
    hexadecimal_digit//2, % ?DecimalDigit:between(0,15)
                          % ?Code:code
    octal_digit//0,
    octal_digit//2, % ?DecimalDigit:between(0,7)
                    % ?Code:code
    sign//0,
    sign//2, % ?Sign:oneof([-1,1])
             % ?Code:code
    signed_number//2, % -SignedNumberal:float
                      % ?Codes:list(code)
    unsigned_number//2 % -UnsignedNumberal:float
                       % ?Codes:list(code)
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



binary_digit --> zero.
binary_digit --> one.

binary_digit(0, C) --> zero(C).
binary_digit(1, C) --> one(C).

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.

decimal_digit(N, C) --> octal_digit(N, C).
decimal_digit(8, C) --> eight(C).
decimal_digit(9, C) --> nine(C).

decimal_number(N, [C]) -->
  decimal_digit(N, C).
decimal_number(N, [C|Cs]) -->
  decimal_digit(_N1, C),
  decimal_number(_N2, Cs),
  {number_codes(N, [C|Cs])}.

exponent -->
  exponent_sign,
  dcg_plus(decimal_digit).

exponent_sign --> e.

exponent_sign(C) --> e(C).

hexadecimal_digit --> decimal_digit.
hexadecimal_digit --> a.
hexadecimal_digit --> b.
hexadecimal_digit --> c.
hexadecimal_digit --> d.
hexadecimal_digit --> e.
hexadecimal_digit --> f.

hexadecimal_digit(N, C) --> decimal_digit(N, C).
hexadecimal_digit(10, C) --> a(C).
hexadecimal_digit(11, C) --> b(C).
hexadecimal_digit(12, C) --> c(C).
hexadecimal_digit(13, C) --> d(C).
hexadecimal_digit(14, C) --> e(C).
hexadecimal_digit(15, C) --> f(C).

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.

octal_digit(N, C) --> binary_digit(N, C).
octal_digit(2, C) --> two(C).
octal_digit(3, C) --> three(C).
octal_digit(4, C) --> four(C).
octal_digit(5, C) --> five(C).
octal_digit(6, C) --> six(C).
octal_digit(7, C) --> seven(C).

sign --> minus_sign.
sign --> plus_sign.

sign(-1, C) --> minus_sign(C).
sign(1, C) --> plus_sign(C).

signed_number(SgN, Cs) -->
  unsigned_number(SgN, Cs).
signed_number(SgN, [C|Cs]) -->
  sign(Sg, C),
  unsigned_number(N, Cs),
  {SgN is Sg * N}.

% @tbd number_codes/2 does not support code lists that
%      start with dot//.
%unsigned_number(N, [C|Cs]) -->
%  dot(C),
%  decimal_number(_N, Cs),
%  {number_codes(N, [C|Cs])}.
unsigned_number(N, Cs) -->
  decimal_number(N, Cs).
unsigned_number(N, [X|Cs2]) -->
  decimal_number(_N, [X|Cs1]),
  dot(C),
  {
    append(Cs1, [C], Cs2),
    number_codes(N, [X|Cs2])
  }.
unsigned_number(N, [X|Cs]) -->
  decimal_number(_N1, [X|Cs1]),
  dot(C),
  decimal_number(_N2, Cs2),
  {
    append([Cs1, [C], Cs2], Cs),
    number_codes(N, [X|Cs])
  }.

