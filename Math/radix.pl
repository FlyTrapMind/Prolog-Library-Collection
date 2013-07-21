:- module(
  radix,
  [
    decimal_to_digits/2, % +DecimalNumber:integer
                         % -DecimalDigits:list(between(0,9))
    digits_to_decimal/2, % +DecimalDigits:list(between(0,9))
                         % -DecimalNumber:integer
    digits_to_decimal/3, % +DecimalDigits:list(between(0,15))
                         % +Radix:integer
                         % -DecimalNumber:integer
    number_to_decimal/3, % +RadixNumber:atomic
                         % +Radix:between(2,16)
                         % -DecimalNumber:integer
    number_to_digits/2 % +DecimalNumber:integer
                       % +DecimalDigits:list(between(0,9))
  ]
).

/** <module> RADIX

Predicate for transforming numbers with a different radix.

@author Wouter Beek
@tbd Study the radix topic further and reimplement these predicates
     in a more generic way.
@version 2013/07
*/

:- use_module(library(apply)).



decimal_to_digits(DecimalNumber, DecimalDigits):-
  atom_chars(DecimalNumber, Chars),
  maplist(atom_number, Chars, DecimalDigits).

digits_to_decimal(DecimalDigits, DecimalNumber):-
  digits_to_decimal(DecimalDigits, 10, DecimalNumber).

%! digits_to_decimal(
%!   +DecimalDigits:list(integer),
%!   +Radix:integer,
%!   -DecimalNumber:integer
%! ) is det.
% Process the decimal digits from left to right, using the radix to multiply
% the result at each step; returning the decimal number.
%
% @arg DecimalDigits A list of decimal digits.
%      Conversion from -- for instance -- hexadecimal digits has
%      already occured before this predicate is invoked.
% @arg Radix An integer indicating the radix of the decimal digits.
% @arg DecimalNumber An integer that is the given decimal digits
%      under the given radix.

digits_to_decimal(DecimalDigits, Radix, DecimalNumber):-
  digits_to_decimal(DecimalDigits, Radix, 0, DecimalNumber).

digits_to_decimal([], _Radix, DecimalNumber, DecimalNumber).
digits_to_decimal([H|T], Radix, M1, DecimalNumber):-
  M2 is M1 * Radix + H,
  digits_to_decimal(T, Radix, M2, DecimalNumber).

%! number_to_decimal(
%!   +RadixNumber:atomic,
%!   +Radix:between(2,16),
%!   -DecimalNumber:integer
%! ) is det.

number_to_decimal(RadixNumber, Radix, DecimalNumber):-
  atom_chars(RadixNumber, RadixDigits),
  maplist(number_to_decimal, RadixDigits, DecimalDigits),
  digits_to_decimal(DecimalDigits, Radix, DecimalNumber).

number_to_decimal(RadixDigit, DecimalNumber):-
  char_type(RadixDigit, xdigit(DecimalNumber)).

number_to_digits(DecimalNumber, DecimalDigits):-
  atom_chars(DecimalNumber, Chars),
  maplist(atom_number, Chars, DecimalDigits).

