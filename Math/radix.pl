:- module(
  radix,
  [
    radix_to_decimal/3 % +RadixNumber:atom
                       % +Radix:between(2,16)
                       % -DecimalNumber:integer
  ]
).

/** <module> RADIX

Predicate for transforming numbers with a different radix.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(apply)).



%! radix(
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

radix(DecimalDigits, Radix, DecimalNumber):-
  radix(DecimalDigits, Radix, 0, DecimalNumber).

radix([], _Radix, DecimalNumber, DecimalNumber).
radix([H|T], Radix, M1, DecimalNumber):-
  M2 is M1 * Radix + H,
  radix(T, Radix, M2, DecimalNumber).

%! radix_to_decimal(
%!   +RadixNumber:atom,
%!   +Radix:between(2,16),
%!   -DecimalNumber:integer
%! ) is det.

radix_to_decimal(RadixNumber, Radix, DecimalNumber):-
  atom_chars(RadixNumber, RadixDigits),
  maplist(radix_to_decimal, RadixDigits, DecimalDigits),
  radix(DecimalDigits, Radix, DecimalNumber).

radix_to_decimal(RadixDigit, DecimalNumber):-
  char_type(RadixDigit, xdigit(DecimalNumber)).
