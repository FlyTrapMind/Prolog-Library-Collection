:- module(
  dcg_cardinal,
  [
    binary_digit//0,
    binary_digit//1,
    decimal_digit//0,
    decimal_digit//1,
    exponent//0,
    exponent_sign//0,
    exponent_sign//1,
    hexadecimal_digit//0,
    hexadecimal_digit//1,
    octal_digit//0,
    octal_digit//1
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

binary_digit(X) --> zero(X).
binary_digit(X) --> one(X).

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.

decimal_digit(X) --> octal_digit(X).
decimal_digit(X) --> eight(X).
decimal_digit(X) --> nine(X).

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
hexadecimal_digit(X) --> a(X).
hexadecimal_digit(X) --> b(X).
hexadecimal_digit(X) --> c(X).
hexadecimal_digit(X) --> d(X).
hexadecimal_digit(X) --> e(X).
hexadecimal_digit(X) --> f(X).

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.

octal_digit(X) --> binary_digit(X).
octal_digit(X) --> two(X).
octal_digit(X) --> three(X).
octal_digit(X) --> four(X).
octal_digit(X) --> five(X).
octal_digit(X) --> six(X).
octal_digit(X) --> seven(X).
