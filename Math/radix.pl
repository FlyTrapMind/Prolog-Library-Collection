:- module(
  radix,
  [
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
    digits_to_digits/4, % +FromDigigts:list(between(0,15))
                        % +From:oneof([2,8,10,16])
                        % +To:oneof([2,8,10,16])
                        % -ToDigits:list(between(0,15))
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
% @param DecimalDigits A list of decimal digits.
%      Conversion from -- for instance -- hexadecimal digits has
%      already occured before this predicate is invoked.
% @param Radix An integer indicating the radix of the decimal digits.
% @param DecimalNumber An integer that is the given decimal digits
%      under the given radix.

digits_to_decimal(DecimalDigits, Radix, DecimalNumber):-
  digits_to_decimal(DecimalDigits, Radix, 0, DecimalNumber).

digits_to_decimal([], _Radix, DecimalNumber, DecimalNumber).
digits_to_decimal([H|T], Radix, M1, DecimalNumber):-
  M2 is M1 * Radix + H,
  digits_to_decimal(T, Radix, M2, DecimalNumber).

%! digits_to_digit(
%!   +FromDigits:list(between(0,15)),
%!   +From:oneof([2,8,10,16]),
%!   +To:oneof([2,8,10,16]),
%!   -ToDigit:between(0,15)
%! ) is det.

digits_to_digit(FromDs, From, To, ToD):-
  aggregate(
    sum(ToDPart),
    (
      nth0(Position, FromDs, FromD),
      ToDPart is FromD * From ** Position
    ),
    ToD
  ).

%! digits_to_digits(
%!   +FromDigits:list(between(0,15)),
%!   +From:oneof([2,8,10,16]),
%!   +To:oneof([2,8,10,16]),
%!   -ToDigits:list(between(0,15))
%! ) is det.

digits_to_digits([], _From, _To, []):- !.
digits_to_digits(L1, From, To, [L2H|L2]):-
  From > To, !,
  NumberOfDigits is To // From,
  length(L1_, NumberOfDigits),
  append(L1_, NewL1, L1),
  digits_to_digit(L1_, From, To, L2H),
  digits_to_digits(NewL1, From, To, L2).

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

