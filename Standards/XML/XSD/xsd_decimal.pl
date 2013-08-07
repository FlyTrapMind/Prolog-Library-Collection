:- module(
  xsd_decimal,
  [
    decimalCanonicalMap/2, % +Decimal
                           % -LEX:list(code)
    decimalLexicalMap/2 % ?LEX:list(code)
                        % ?Decimal
  ]
).

/** <module> XSD_DECIMAL

*Decimal* represents a subset of the real numbers, which can be represented
by decimal numerals.

#### Prolog compound term

~~~{.pl}
decimal(I:integer,N:integer)
~~~

#### Value space

The value space of decimal is the set of numbers that
can be obtained by dividing an integer by a non-negative power of ten,
i.e., expressible as $\frac{i}{10n} where $i$ and $n$ are integers and
$n \geq 0$.

Precision is not reflected in this value space; the number 2.0
is not distinct from the number 2.00. The order relation on decimal is the
order relation on real numbers, restricted to this subset.

#### Lexical space

Decimal has a lexical representation consisting of a non-empty finite-length
sequence of decimal digits (#x30–#x39) separated by a period as a decimal
indicator. An optional leading sign is allowed. If the sign is omitted,
"+" is assumed. Leading and trailing zeroes are optional. If the fractional
part is zero, the period and following zero(es) can be omitted.

Examples:
  * =|-1.23|=
  * =|12678967.543233|=
  * =|+100000.00|=
  * =|210|=

~~~{.ebnf}
decimalLexicalRep ::= decimalPtNumeral | noDecimalPtNumeral
decimalPtNumeral ::= ('+' | '-')? unsignedDecimalPtNumeral
digit ::= [0-9]
fracFrag ::= digit+
noDecimalPtNumeral ::= ('+' | '-')? unsignedNoDecimalPtNumeral
unsignedDecimalPtNumeral ::=
    (unsignedNoDecimalPtNumeral '.' fracFrag?) | ('.' fracFrag)
unsignedNoDecimalPtNumeral ::= digit+
~~~

~~~{.re}
(\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)
~~~

#### Canonical representation

For integers, the decimal point and fractional part are prohibited.
For other values, the preceding optional "+" sign is prohibited.
The decimal point is required.
In all cases, leading and trailing zeroes are prohibited subject to the
following: there must be at least one digit to the right and to the left
of the decimal point which may be a zero.

--

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_ext)).
:- use_module(library(lists)).
:- use_module(math(radix)).



%! decimalCanonicalMap(+Decimal, -LEX:list(code))
% Maps a decimal to its canonical representation, a decimalLexicalRep//1.
%
% If the given decimal is an integer, then return noDecimalPtCanonicalMap//1.
% Otherwise, return decimalPtCanonicalMap//1.
%
% @param Decimal A decimal value.
% @param LEX A literal matching decimalLexicalRep//1.

decimalCanonicalMap(Decimal, LEX):-
  integer(Decimal), !,
  phrase(noDecimalPtCanonicalMap(Decimal), LEX).
decimalCanonicalMap(Decimal, LEX):-
  phrase(decimalPtCanonicalMap(Decimal), LEX).

%! decimalLexicalRep(D)//
% Processes a decimal value that is internally represented as
% a SWI-Prolog float.
%
% @see For a full DCG alternative, that does not use number_codes/2
%      nor format/3, see decimalLexicalRep//2.

decimalLexicalRep(D) -->
  {var(D)}, !,
  (sign(H1) -> {L1 = [H1|T1]} ; {L1 = T1}),
  dcg_multi(decimal_digit, between(1,_), T1),
  (dot(H2), dcg_multi(decimal_digit, _, T2) -> {L2 = [H2|T2]} ; {L2 = T2}),
  {append(L1, L2, L), number_codes(D, L)}.
decimalLexicalRep(D, H, T):-
  number(D), !,
  format(codes(H,T), '~w', [D]).

%! decimalLexicalRep(-Tree:compound, ?Decimal:compound)//
% Processes a decimal value using the compound term notation for a decimal.
% Also returns the parse tree.

decimalLexicalRep(T0, decimal(I,N)) -->
  {var(I), var(N)}, !,
  (sign(T1, Sign) ; {Sign = 1}),
  dcg_multi(decimal_digit, between(1,_), I1s),
  (
    dot, {T3 = '.'},
    dcg_multi(decimal_digit, _, I2s)
  ;
    {I2s  = []}
  ),
  {
    append(I1s, I2s, Is),
    digits_to_decimal(Is, I_),
    I is Sign * I_,
    length(I2s, N),
    parse_tree(decimalLexicalRep, [T1,I1s,T3,I2s], T0)
  }.
decimalLexicalRep(T0, decimal(I,N)) -->
  {
    integer(I), integer(N), !,
    length(I2s, N),
    (I < 0 -> Sign = -1 ; Sign = 1),
    I_ is Sign * I,
    decimal_to_digits(I_, Is),
    append(I1s, I2s, Is)
  },
  (sign(T1, Sign) ; {Sign = 1}),
  dcg_multi(decimal_digit, between(1,_), I1s),
  (
    dot,
    dcg_multi(decimal_digit, _, I2s)
  ->
    {T3 = '.'}
  ;
    {I2s  = []}
  ),
  {parse_tree(decimalLexicalRep, [T1,I1s,T3,I2s], T0)}.

%! digit(+Integer:between(0,9), Digit:char) is det.
% Maps each integer between 0 and 9 to the corresponding digit.
%
% @param Integer An integer between 0 and 9 inclusive.
% @param Digit Matches digit//.
%
% @see Builtin char_type/1.

digit(I, D):-
  char_type(D, digit(I)).

%! digitRemainderSeq(+Integer:nonneg, Sequence:list(nonneg)) is det.
% Maps each nonnegative integer to a sequence of integers.
%
% Definition of sequence $s$:
%   * $s_0 = i$
%   * $s_{j+1} = s_j div 10$
%
% @param Integer A nonnegative integer.
% @param Sequence A list of of nonnegative integers.
%
% @bug The definition of this function does not put a limit on the length
%      of the list. It is reasonable to not include any zeros.

digitRemainderSeq(N, []):-
  N < 10, !.
digitRemainderSeq(N, [H|T]):-
  H is N div 10,
  digitRemainderSeq(H, T).

%! digitSeq(+Integer:nonneg, -Sequence:list(Integer)) is det.
% Maps each nonnegative integer to a sequence of integers.
%
% Returns the sequence $s$ for which  $s_j = digitRemainderSeq(i)_j mod 10$.
%
% @param Integer A nonnegative integer.
% @param Sequence A list of decimal digigts.
%
% @see decimal_to_digits/2 in module RADIX.

digitSeq(I, S2):-
  digitRemainderSeq(I, S1),
  findall(X2, (member(X1, S1), X2 is X1 mod 10), S2).

%! lastSignificantDigit(+Sequence:list(between(0,9)), -Integer) is det.
% Maps a sequence of nonnegative integers to the index of the first zero term.
%
% Return the smallest nonnegative integer $j$ such that $s(i)_{j+1} = 0$.
%
% @param Sequence A sequence of nonnegative integers.
% @param A nonnegative integer.

lastSignificantDigit(S, I):-
  nth0chk(J, S, 0),
  I is J - 1.

%! noDecimalPtCanonicalMap(+Integer, -LEX) is det.
% Maps an integer to a noDecimalPtNumeral//, its canonical representation.
%
% If the integer is negative, return "-" followed by
% unsignedNoDecimalPtCanonicalMap/1 with the positive variant of integer.
% Otherwise, return unsignedNoDecimalPtCanonicalMap/1.
%
% @param Integer An integer.
% @param LEX Matches noDecimalPtNumeral//

noDecimalPtCanonicalMap(I1, LEX):-
  I2 is abs(I1),
  unsignedNoDecimalPtCanonicalMap(I2, LEX).

%! unsignedNoDecimalPtCanonicalMap(+Integer:nonneg, -LEX) is det.
% Maps a nonnegative integer to a unsignedNoDecimalPtNumeral//,
% its canonical representation.
%
% @param Integer A nonnegative integer.
% @param LEX Matches unsignedNoDecimalPtNumeral//.

unsignedNoDecimalPtCanonicalMap(I, LEX):-
  digit(·digitSeq·(i)·lastSignificantDigit·(·digitRemainderSeq·(i))) & . . . & ·digit·(·digitSeq·(i)0) .   (Note that the concatenation is in reverse order.)

