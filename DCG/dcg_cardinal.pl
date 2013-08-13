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
    int_codes//1, % ?Codes:list(code)
    nonzero_decimal_digit//1, % ?DecimalDigit:between(1,9)
    nonzero_decimal_digit//2, % ?DecimalDigit:between(1,9)
                              % ?Code:code
    nonzero_octal_digit//1, % ?DecimalDigit:between(1,7)
    nonzero_octal_digit//2, % ?DecimalDigit:between(1,7)
                            % ?Code:code
    octal_digit//0,
    octal_digit//1, % ?DecimalDigit:between(0,7)
    octal_digit//2, % ?DecimalDigit:between(0,7)
                    % ?Code:code
    octal_number//1, % -DecinalNumber:integer
    sign//1, % ?Sign:code
    sign//2, % -Tree:compound
             % ?Sign:oneof([-1,1])
    signed_number//1, % ?SignedNumber:float
    unsigned_number//1, % ?UnsignedNumber:float
    unsigned_number//3 % ?Number:float
                       % ?IntegerComponent:integer
                       % ?FractionalComponent:integer
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
@version 2013/06-2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(math(math_ext)).

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
% @param DCGBody processes a single digit if the given radix.
% @param Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @param An integer representing the processed number, converted to
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

% Look for the next number...
digits_to_decimal_number(Digit, Radix, M1, M) -->
  % Process the next digit.
  dcg_call(Digit, N),
  % Perform radix multiplication.
  {M2 is M1 * Radix + N},
  digits_to_decimal_number(Digit, Radix, M2, M).
% End of code segment, the decimal number we have built so far is the result.
digits_to_decimal_number(_Digit, _Radix, M, M) --> [].

exponent -->
  exponent_sign,
  dcg_multi(decimal_digit, 1-_).

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

%! int_codes(?Codes:list(code))//
% A positive number of digits, possibly followed by a sign.

int_codes([C,D0|D]) -->
  sign(C), !,
  digit(D0),
  digits(D).
int_codes([D0|D]) -->
  digit(D0),
  digits(D).

nonzero_decimal_digit(D) --> nonzero_octal_digit(D).
nonzero_decimal_digit(8) --> eight.
nonzero_decimal_digit(9) --> nine.

nonzero_decimal_digit(D, C) --> nonzero_octal_digit(D, C).
nonzero_decimal_digit(8, C) --> eight(C).
nonzero_decimal_digit(9, C) --> nine(C).

nonzero_octal_digit(1) --> one.
nonzero_octal_digit(2) --> two.
nonzero_octal_digit(3) --> three.
nonzero_octal_digit(4) --> four.
nonzero_octal_digit(5) --> five.
nonzero_octal_digit(6) --> six.
nonzero_octal_digit(7) --> seven.

nonzero_octal_digit(1, C) --> one(C).
nonzero_octal_digit(2, C) --> two(C).
nonzero_octal_digit(3, C) --> three(C).
nonzero_octal_digit(4, C) --> four(C).
nonzero_octal_digit(5, C) --> five(C).
nonzero_octal_digit(6, C) --> six(C).
nonzero_octal_digit(7, C) --> seven(C).

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

sign(0'-) --> "-". %'
sign(0'+) --> "+". %'

sign(sign(-1), -1) --> "-".
sign(sign(1), 1) --> "+".

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
  unsigned_number(N, _Integer, _Fraction).

%! unsigned_number(
%!   ?Number:float,
%!   ?IntegerComponent:integer,
%!   ?FractionalComponent:integer
%! )//

unsigned_number(N, N_I, N_F) -->
  ({N_I = 0} ; decimal_number(N_I)),
  dot,
  ({N_F = 0} ; decimal_number(N_F)),
  {number_parts(N, N_I, N_F)}.

