:- module(
  dcg_cardinal,
  [
    binary_digit//0,
    binary_digit//1, % ?DecimalDigit:between(0,1)
    decimal_digit//0,
    decimal_digit//1, % ?DecimalDigit:between(0,9)
    decimal_number//1, % -DecimalNumber:integer
    exponent//0,
    exponent_sign//0,
    exponent_sign//1,
    hexadecimal_digit//0,
    hexadecimal_digit//1, % ?DecimalDigit:between(0,15)
    octal_digit//0,
    octal_digit//1, % ?DecimalDigit:between(0,7)
    sign//0,
    sign//1 % ?Sign:oneof([-1,1])
    %signed_numeral//1, % -SignedNumberal:float
    %unsigned_numeral//1 % -UnsignedNumberal:float
  ]
).

/** <module> DCG_CARDINAL

DCGs for cardinal numbers.

@author Wouter Beek
@version 2013/06-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).

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



binary_digit --> zero.
binary_digit --> one.

binary_digit(0) --> zero.
binary_digit(1) --> one.

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.

decimal_digit(X) --> octal_digit(X).
decimal_digit(8) --> eight.
decimal_digit(9) --> nine.

decimal_number(N) -->
  decimal_number(0, N).

decimal_number(H1, N) -->
  decimal_digit(M),
  {H2 is H1 * 10 + M},
  decimal_number(H2, N).
decimal_number(N, N) --> [].

exponent -->
  exponent_sign,
  dcg_plus(decimal_digit).

exponent_sign --> e.

exponent_sign(X) --> e(X).

hexadecimal_digit --> decimal_digit.
hexadecimal_digit --> a.
hexadecimal_digit --> b.
hexadecimal_digit --> c.
hexadecimal_digit --> d.
hexadecimal_digit --> e.
hexadecimal_digit --> f.

hexadecimal_digit(X) --> decimal_digit(X).
hexadecimal_digit(10) --> a.
hexadecimal_digit(11) --> b.
hexadecimal_digit(12) --> c.
hexadecimal_digit(13) --> d.
hexadecimal_digit(14) --> e.
hexadecimal_digit(15) --> f.

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.

octal_digit(X) --> binary_digit(X).
octal_digit(2) --> two.
octal_digit(3) --> three.
octal_digit(4) --> four.
octal_digit(5) --> five.
octal_digit(6) --> six.
octal_digit(7) --> seven.

sign --> minus_sign.
sign --> plus_sign.

sign(-1) --> minus_sign.
sign(1) --> plus_sign.

%signed_numeral(SgN) -->
%  unsigned_numeral(SgN).
%signed_numeral(SgN) -->
%  sign(Sg),
%  unsigned_numeral(N),
%  {SgN is Sg * N}.

%unsigned_numberal(N) -->
%  dot,
%  decimal_number(N).
%unsigned_numberal(N) -->
%  decimal_number(N),
%  (dot ; "").
%unsigned_numeral(N) -->
%  decimal_number(N1),
%  dot,
%  decimal_numeral(N2),
%  {}.
