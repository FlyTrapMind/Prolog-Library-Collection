:- module(
  radix,
  [
    between_hex/3, % +LowHex:atom
                   % +HighHex:atom
                   % ?Number:integer
    decimal_to_digits/2, % +DecimalNumber:integer
                         % -DecimalDigits:list(between(0,9))
    decimal_to_digits/3, % +DecimalNumber:integer
                         % +Radix:oneof([2,8,10])
                         % -DecimalDigits:list(between(0,9))
    digits_to_decimal/2, % +DecimalDigits:list(between(0,9))
                         % -DecimalNumber:integer
    digits_to_decimal/3, % +DecimalDigits:list(between(0,15))
                         % +Radix:integer
                         % -DecimalNumber:integer
    hex_value/2, % +HexadecimalValue:atom
                 % -DecimalValue:nonneg
    number_to_decimal/3 % +RadixNumber:atomic
                        % +Radix:between(2,16)
                        % -DecimalNumber:integer
  ]
).

/** <module> RADIX

Predicate for transforming numbers with a different radix.

@author Wouter Beek
@tbd Study the radix topic further and reimplement these predicates
     in a more generic way.
@version 2013/07-2013/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).



between_hex(LowHex, HighHex, Number):-
  number_to_decimal(LowHex, 16, Low),
  number_to_decimal(HighHex, 16, High),
  between(Low, High, Number).

decimal_to_digits(DecimalNumber, DecimalDigits):-
  atom_chars(DecimalNumber, Chars),
  maplist(atom_number, Chars, DecimalDigits).

decimal_to_digits(D, R, Sol):-
  decimal_to_digits(D, R, [], Sol).

decimal_to_digits(D, R, A, A):-
  D < R, !.
decimal_to_digits(D, R, A, Sol):-
  H is D mod R,
  NewD is D // R,
  decimal_to_digits(NewD, R, [H|A], Sol).

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

digits_to_decimal(Ds, Radix, D):-
  aggregate_all(
    sum(ToDPart),
    (
      nth0(Position, Ds, FromD),
      ToDPart is FromD * Radix ** Position
    ),
    D
  ).


%! hex_value(+HexadecimalValue:atom, -DecimalValue:nonneg) is det.
% @tbd Allow negative values.

hex_value(HexValue, DecValue):-
  atom_chars(HexValue, HexDigits),
  hex_digits(HexDigits, 0, DecValue).


%! dec_hex(+Dec:nonneg, +Hex:atom) is semidet.
%! dec_hex(+Dec:nonneg, -Hex:atom) is det.
%! dec_hex(-Dec:nonneg, +Hex:atom) is det.

dec_hex(Dec, Hex2):-
  integer(Dec1), !,
  dec_to_hex(Dec, Hex1),
  reverse(Hex1, Hex2).
dec_hex(Dec, Hex1):-
  hex_to_dec(Hex, Dec).


%! dec_to_hex(+Dec:nonneg, -Hex:atom) is det.

dec_to_hex(Dec, Hex2):-
  dec_to_hex_(Dec, Hex1),
  reverse(Hex1, Hex2).

dec_to_hex_(Dec1, [H|T]):-
  Rem is Dec1 mod 16,
  code_type(H, xdigit(Rem)),
  Dec2 is (Dec1 - Rem) / 16,
  dec_to_hex_(Dec2, T).


%! hex_to_dec(+Hex:atom, -Dec:nonneg) is det.

hex_to_dec(Hex1, Dec):-
  atom(Hex1), !,
  atom_codes(Hex1, Hex2),
  hex_to_dec(Hex2, Dec).
hex_to_dec(Hex, Dec):-
  is_list(Hex),
  hex_to_dec(Hex, 0, Dec).

hex_to_dec([], Dec, Dec).
hex_to_dec([Hex|T], Sum1, Dec):-
  code_type(Hex, xdigit(HDec))
  Sum2 is Sum1 * 16 + HDec,
  hex_to_dec(T, Sum2, Dec).


hex_digits([], N, N).
hex_digits([H|T], N1, N):-
  char_type(H, xdigit(N0)),
  N2 is N1 * 16 + N0,
  hex_digits(T, N2, N).


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

