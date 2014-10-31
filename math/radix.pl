:- module(
  radix,
  [
    between_radix/3, % +Low:compound
                     % +High:compound
                     % ?Number:compound
    digit_weight/2, % ?Digit:hex
                    % ?Weight:between(0,15)
    digits_decimal/2, % ?Digits:list(hex)
                      % ?Decimal:nonneg
    digits_radix/2, % ?Digits:list(hex)
                    % ?Number:compound
    hexadecimal_digit/1, % ?Digit:hex
    hexadecimal_digit_weight/2, % ?Digit:hex
                                % ?Weight:between(0,15)
    radix/2, % +From:compound
             % ?To:compound
    weights_radix/2 % ?Weights:list(between(0,15))
                    % ?Number:compound
  ]
).

/** <module> Mathematics: Radix conversion

Predicate for transforming numbers between
positional notations of different radix.

@author Wouter Beek
@version 2013/07-2013/08, 2014/09-2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).

:- use_module(generics(char_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).

:- multifile(error:has_type/2).
error:has_type(hex, Value):-
  error:has_type(or([between(0,9),oneof([a,b,c,d,e,f])]), Value).
error:has_type(rad_name, Value):-
  error:has_type(oneof([bin,oct,dec,hex]), Value).
error:has_type(rad_number, Value):-
  error:has_type(oneof([2,8,10,16]), Value).





%! between_radix(+Low:compound, +High:compound, +Number:compound) is semidet.
%! between_radix(+Low:compound, +High:compound, -Number:compound) is nondet.

between_radix(Low, High, Number):-
  radix(Low, dec(LowDec)),
  radix(High, dec(HighDec)),
  % If Number is ground it is more efficient to first convert it to decimal
  % and then check whether it is between (rather than generating values
  % that are between).
  % This also makes the `(+,+,+)`-case semi-deterministic.
  (	  ground(Number)
  ->  radix(Number, dec(NumberDec)),
      between(LowDec, HighDec, NumberDec)
  ;   between(LowDec, HighDec, NumberDec),
      radix(Number, dec(NumberDec))
  ).



%! digit_weight(+Digit:hex, -Weight:between(0,15)) is det.
%! digit_weight(-Digit:hex, +Weight:between(0,15)) is det.

digit_weight(Weight, Weight):-
  between(0, 9, Weight).
digit_weight(a, 10).
digit_weight(b, 11).
digit_weight(c, 12).
digit_weight(d, 13).
digit_weight(e, 14).
digit_weight(f, 15).



%! digits_decimal(+Digits:list(hex), -Decimal:nonneg) is det.
%! digits_decimal(-Digits:list(hex), +Decimal:nonneg) is det.

digits_decimal(Digits, Decimal):-
  digits_radix(Digits, dec(Decimal)).



%! digits_radix(+Digits:list(hex), -Number:compound) is det.
%! digits_radix(-Digits:list(hex), +Number:compound) is det.

digits_radix(Digits, Number):-
  nonvar(Digits), !,
  maplist(hexadecimal_digit_weight, Digits, Weights),
  weights_radix(Weights, Number).
digits_radix(Digits, Number):-
  nonvar(Number), !,
  weights_radix(Weights, Number),
  maplist(hexadecimal_digit_weight, Digits, Weights).



%! hexadecimal_digit(+Digit:hex) is semidet.
%! hexadecimal_digit(-Digit:hex) is multi.

hexadecimal_digit(Digit):-
  hexadecimal_digit_weight(Digit, _).



%! hexadecimal_digit_weight(+Digit:hex, +Weight:between(0,15)) is semidet.
%! hexadecimal_digit_weight(+Digit:hex, -Weight:between(0,15)) is det.
%! hexadecimal_digit_weight(-Digit:hex, +Weight:between(0,15)) is det.

hexadecimal_digit_weight(Weight, Weight):-
  between(0, 9, Weight), !.
hexadecimal_digit_weight(a, 10).
hexadecimal_digit_weight(b, 11).
hexadecimal_digit_weight(c, 12).
hexadecimal_digit_weight(d, 13).
hexadecimal_digit_weight(e, 14).
hexadecimal_digit_weight(f, 15).



%! radix(+From:compound, +To:compound) is det.
%! radix(+From:compound, -To:compound) is det.
% Radix conversion between often used bases.
%
% From and To make use of the followig radix notations:
%   - bin(+nonneg)
%   - dec(+nonneg)
%   - hex(+atom)
%   - oct(+nonneg)
%
% If From is an atom it is assumed to be `hex(+atom)`.
% If From is a non-negative integer it is assumed to be `dec(+nonneg)`.
%
% @throws instantiation_error If both From and To are uninstantiated.

% Swap arguments or
% instantiation error.
radix(From, To):-
  \+ ground(From), !,
  % One argument has to be ground.
  (   \+ ground(To)
  ->  instantiation_error(To)
  ;   radix(To, From)
  ).
% Non-radix notation for input: hexadecimal.
radix(From, To):-
  atom(From), !,
  radix(hex(From), To).
% Non-radix notation for input: decimal.
radix(From, To):-
  nonneg(From), !,
  radix(dec(From), To).
% Radix notation for input: binary, octal, decimal, hexadecimal.
radix(From, To):-
  From =.. [FromRadix,FromValue], !,
  to_decimal(FromRadix, FromValue, Decimal),
  % If there is no radix for the output value then we assume decimal base.
  (   var(To)
  ->  ToRadix = dec,
			To = dec(ToValue)
  ;   To =.. [ToRadix,ToValue]
	),
  from_decimal(Decimal, ToRadix, ToValue).



%! weights_radix(+Weights:between(0,15), -Number:compound) is det.
%! weights_radix(-Weights:between(0,15), +Number:compound) is det.

weights_radix(Weights, Number):-
  nonvar(Weights),
  Number =.. [Radix,Value], !,
  maplist(char_weight, Chars, Weights),
  (   Radix == hex
  ->  atom_chars(Value, Chars)
  ;   number_chars(Value, Chars)
  ).
weights_radix(Weights, Number):-
  nonvar(Number), !,
  atom_chars(Number, Chars),
  maplist(char_weight, Chars, Weights).





% HELPERS

%! char_weight(+Char:char, -Weight:between(0,15)) is det.
%! char_weight(-Char:char, +Weight:between(0,15)) is det.

char_weight(Char, Weight):-
  char_type(Char, xdigit(Weight)).



%! from_decimal(
%!   +Decimal:nonneg,
%!   +Radix:rad_name,
%!   -Value:or([atom,nonneg])
%! ) is det.

from_decimal(Decimal, dec, Decimal):- !.
from_decimal(Decimal, Radix, Value):-
  radix_value(Radix, RadixValue),
  from_decimal(Decimal, RadixValue, [], Chars),
  radix_chars_to_atomic(Radix, Chars, Value).

from_decimal(0, _, Chars, Chars):- !.
from_decimal(Decimal, Radix, Chars, Sol):-
  Value is Decimal mod Radix,
  radix_value(Radix, Char, Value),
  Remainder is Decimal div Radix,
  from_decimal(Remainder, Radix, [Char|Chars], Sol).



%! radix_chars_to_atomic(
%!   +Radix:rad_name,
%!   +Chars:list(char),
%!   -Value:atomic
%! ) is det.

radix_chars_to_atomic(hex, Chars, Atom):- !,
  atom_chars(Atom, Chars).
radix_chars_to_atomic(Radix, Chars, Value):-
  memberchk(Radix, [bin,oct,dec]),
  number_chars(Value, Chars).



%! radix_value(+Radix:rad_name, -Value:rad_number) is det.

radix_value(bin,  2).
radix_value(dec, 10).
radix_value(hex, 16).
radix_value(oct,  8).



%! radix_value(+Radix:rad_number, +Char:char, +Value:between(0,15)) is semidet.
%! radix_value(+Radix:rad_number, +Char:char, -Value:between(0,15)) is det.
%! radix_value(+Radix:rad_number, -Char:char, +Value:between(0,15)) is det.

radix_value(16, Char, Value):- !,
  char_type(Char, xdigit(Value)).
radix_value(Radix, Char, Value):-
  char_type(Char, digit(Value)),
  Value < Radix.



%! to_decimal(
%!   +Radix:rad_number,
%!   +Value:or([atom,list(char),list(code),nonneg,string]),
%!   -Decimal:nonneg
%! ) is det.

to_decimal(dec, Decimal, Decimal):-
	 nonneg(Decimal), !.
to_decimal(Radix, Value, Decimal):-
  radix_value(Radix, RadixValue),
  to_chars(Value, Chars),
  aggregate_all(
    sum(PositionalValue),
    (
      nth0_minus(Position, Chars, Char),
      radix_value(RadixValue, Char, Weight),
      PositionalValue is Weight * RadixValue ** Position
    ),
    Decimal
  ).
