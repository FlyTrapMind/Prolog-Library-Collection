:- module(
  radix,
  [
    between_hex/3, % +LowHex:atom
                   % +HighHex:atom
                   % ?Number:integer
    dec_to_hex/2, % +DecimalNumber:nonneg,
                  % -HexadecimalNumber:atom
    digits_decimal/2, % +DecimalDigits:list(nonneg)
                      % -DecimalNumber:nonneg
    digits_decimal/3, % +DecimalDigits:list(nonneg)
                      % +Radix:positive_integer
                      % -DecimalNumber:nonneg
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
@version 2013/07-2013/08, 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).



between_hex(LowHex, HighHex, Number):-
  number_to_decimal(LowHex, 16, Low),
  number_to_decimal(HighHex, 16, High),
  between(Low, High, Number).


%! dec_hex(+Dec:nonneg, +Hex:atom) is semidet.
%! dec_hex(+Dec:nonneg, -Hex:atom) is det.
%! dec_hex(-Dec:nonneg, +Hex:atom) is det.

dec_hex(Dec, Hex2):-
  integer(Dec), !,
  dec_to_hex(Dec, Hex1),
  reverse(Hex1, Hex2).
dec_hex(Dec, Hex):-
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


%! digits_decimal(+Digits:list(nonneg), -Decimal:nonneg) is det.
%! digits_decimal(-Digits:list(nonneg), +Decimal:nonneg) is det.

digits_decimal(Digits, Decimal):-
  digits_decimal(Digits, 10, Decimal).


%! digits_decimal(+Digits:list(nonneg), +Radix:nonneg, -Decimal:nonneg) is det.
%! digits_decimal(-Digits:list(nonneg), +Radix:nonneg, +Decimal:nonneg) is det.

digits_decimal(Digits, Radix, Decimal):-
  (   nonvar(Decimal)
  ->  decimal_to_digits(Decimal, Radix, Digits0),
      reverse(Digits0, Digits)
  ;   digits_to_decimal(Digits, Radix, _, Decimal)
  ).

decimal_to_digits(Digit, Radix, [Digit]):-
  Digit =< Radix, !.
decimal_to_digits(Decimal, Radix, [Digit|Digits]):-
  Digit is Decimal mod Radix,
  Decimal0 is Decimal // Radix,
  decimal_to_digits(Decimal0, Radix, Digits).

digits_to_decimal([Digit], _, 0, Digit):- !.
digits_to_decimal([Digit|Digits], Radix, Position, Decimal):-
  digits_to_decimal(Digits, Radix, Position0, Decimal0),
  succ(Position0, Position),
  Decimal is Decimal0 + Digit * Radix ** Position.


%! hex_value(-HexadecimalValue:atom, +DecimalValue:nonneg) is det.
%! hex_value(+HexadecimalValue:atom, -DecimalValue:nonneg) is det.
% @tbd Allow negative values.

hex_value(HexValue, DecValue):-
  atom_chars(HexValue, HexDigits),
  hex_digits(HexDigits, 0, DecValue).


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
  code_type(Hex, xdigit(HDec)),
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
  digits_decimal(DecimalDigits, Radix, DecimalNumber).

number_to_decimal(RadixDigit, DecimalNumber):-
  char_type(RadixDigit, xdigit(DecimalNumber)).

