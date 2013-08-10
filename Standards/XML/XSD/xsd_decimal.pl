:- module(
  xsd_decimal,
  [
    decimalCanonicalMap/2, % +Decimal:number
                           % -LEX:list(code)
    decimalLexicalMap/2, % +LEX:list(code)
                         % -Decimal:number
    decimalLexicalMap1/2, % ?LEX:list(code)
                          % ?Decimal:number
    decimalLexicalMap2/2, % ?LEX:list(code)
                          % ?Decimal:compound
% DCG COMPONENTS
    decimalPtNumeral//2, % -Sign:oneof([-1,1])
                         % -Decimal:float
    fractionDigitsCanonicalFragmentMap//1, % ?Fraction:rational
    noDecimalPtCanonicalMap//1, % +Integer:integer
    noDecimalPtNumeral//2, % -Sign:oneof([-1,1])
                           % -Integer:integer
    unsignedDecimalPtCanonicalMap//1, % +Decimal:rational
    unsignedDecimalPtNumeral//1, % -Decimal:float
    unsignedNoDecimalPtCanonicalMap//1, %+Integer:nonneg
    unsignedNoDecimalPtNumeral//1 % -Integer:nonneg
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

#### Facets

Decimal has the following constraining facets with fixed values:
  * =|whiteSpace = collapse|=

Datatypes derived by restriction from decimal may also specify values for
the following constraining facets:
  * =assertions=
  * =enumeration=
  * =fractionDigits=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =pattern=
  * =totalDigits=

The decimal datatype has the following values for its fundamental facets:
  * =|ordered = total|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = true|=

--

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(math(math_ext)).
:- use_module(math(rational_ext)).



% CANONICAL MAPPING %

%! decimalCanonicalMap(
%!   +Decimal:oneof([integer,rational]),
%!   -LEX:list(code)
%! ) is det.

decimalCanonicalMap(N, LEX):-
  phrase(decimalCanonicalMap(N), LEX).

%! decimalCanonicalMap(+Decimal:oneof([integer,rational]))//

decimalCanonicalMap(I) -->
  {integer(I)}, !,
  noDecimalPtCanonicalMap(I).
decimalCanonicalMap(F) -->
  {rational(F)}, !,
  decimalPtCanonicalMap(F).

%! decimalPtCanonicalMap(+Decimal:rational)//

decimalPtCanonicalMap(F) -->
  {F < 0}, !,
  "-",
  {G is copysign(F, -1)},
  unsignedDecimalPtCanonicalMap(G).
decimalPtCanonicalMap(F) -->
  unsignedDecimalPtCanonicalMap(F).

%! fractionDigitsCanonicalFragmentMap(?Fraction:rational)//

fractionDigitsCanonicalFragmentMap(F) -->
  {F =:= 0}, !,
  [].
fractionDigitsCanonicalFragmentMap(F) -->
  {
    G is F * 10,
    div(G, 1, H),
    mod(G, 1, NewF)
  },
  decimal_digit(H),
  fractionDigitsCanonicalFragmentMap(NewF).

%! noDecimalPtCanonicalMap(+Integer:integer)//

noDecimalPtCanonicalMap(I) -->
  {I < 0},
  "-",
  {J is copysign(I, 1)},
  unsignedNoDecimalPtCanonicalMap(J).
noDecimalPtCanonicalMap(I) -->
  unsignedNoDecimalPtCanonicalMap(I).

%! unsignedDecimalPtCanonicalMap(+Decimal:rational)//

unsignedDecimalPtCanonicalMap(F) -->
  {rational_parts(F, F_I, F_F)},
  unsignedNoDecimalPtCanonicalMap(F_I),
  ".",
  fractionDigitsCanonicalFragmentMap(F_F).

%! unsignedNoDecimalPtCanonicalMap(+Integer:nonneg)//

unsignedNoDecimalPtCanonicalMap(0) --> !, [].
unsignedNoDecimalPtCanonicalMap(F) -->
  {
    mod(F, 10, G),
    div(F, 10 ,H)
  },
  unsignedNoDecimalPtCanonicalMap(H),
  decimal_digit(G).



% LEXICAL MAPPING %

%! decimalLexicalMap(+LEX:list(code), -Decimal:number) is nondet.
% This predicate cannot work for instantiation (-,+) due to the following
% DCG rules contain arithmetic functions that are unidirectional:
%   * fracFrag//1
%   * unsignedNoDecimalPtNumeral//1
%
% @see decimalLexicalMap1/2 and decimalLexicalMap2/2 for bidirectional
%      implementations.

decimalLexicalMap(LEX, N):-
  phrase(decimalLexicalRep(N), LEX).

%! decimalLexicalRep(-Decimal:float)//
% ~~~{.ebnf}
% decimalLexicalRep ::= decimalPtNumeral | noDecimalPtNumeral
% ~~~

decimalLexicalRep(N) -->
  decimalPtNumeral(_Sign, N).
decimalLexicalRep(N) -->
  noDecimalPtNumeral(_Sign, N).

%! decimalPtNumeral(-Sign:oneof([-1,1]), -Decimal:float)//
% ~~~{.ebnf}
% decimalPtNumeral ::= ('+' | '-')? unsignedDecimalPtNumeral
% ~~~

decimalPtNumeral(Sign, N) -->
  (sign(Sign) ; {Sign = 1}),
  unsignedDecimalPtNumeral(N1),
  {N is copysign(N1, Sign)}.

%! fracFrag(-Fraction:between(0.0,1.0))//
% ~~~{.ebnf}
% fracFrag ::= digit+
% ~~~

fracFrag(F) -->
  fracFrag(0, F).

fracFrag(I, NewSum) -->
  decimal_digit(D),
  {succ(I, NewI)},
  fracFrag(NewI, Sum),
  {NewSum is Sum + D * 10 ** (-1 * NewI)}.
fracFrag(_, 0.0) --> !, [].

%! noDecimalPtNumeral(-Sign:oneof([-1,1]), -Integer:integer)//
% ~~~{.ebnf}
% noDecimalPtNumeral ::= ('+' | '-')? unsignedNoDecimalPtNumeral
% ~~~

noDecimalPtNumeral(Sign, N) -->
  (sign(Sign) ; {Sign = 1}),
  unsignedNoDecimalPtNumeral(N1),
  {N is copysign(N1, Sign)}.

%! unsignedDecimalPtNumeral(-Decimal:float)//
% ~~~{.ebnf}
% unsignedDecimalPtNumeral ::= (unsignedNoDecimalPtNumeral '.' fracFrag?)
%                              | ('.' fracFrag)
% ~~~

unsignedDecimalPtNumeral(N) -->
  unsignedNoDecimalPtNumeral(I),
  ".",
  (fracFrag(F), {N is I + F} ; {N = I}).
unsignedDecimalPtNumeral(F) -->
  ".",
  fracFrag(F).

%! unsignedNoDecimalPtNumeral(-Integer:nonneg)//
% ~~~{.ebnf}
% unsignedNoDecimalPtNumeral ::= digit+
% ~~~

unsignedNoDecimalPtNumeral(N) -->
  unsignedNoDecimalPtNumeral(_, N).

unsignedNoDecimalPtNumeral(NewToEnd, NewN) -->
  decimal_digit(D),
  unsignedNoDecimalPtNumeral(ToEnd, N),
  {
    NewN is N + D * 10 ** ToEnd,
    succ(ToEnd, NewToEnd)
  }.
unsignedNoDecimalPtNumeral(0, 0) --> [].



% BIDIRECTIONAL IMPLEMENTATIONS %

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(math(radix)).

decimalLexicalMap1(LEX, D):-
  phrase(decimalLexicalRep1(D), LEX).

decimalLexicalMap2(LEX, D):-
  phrase(decimalLexicalRep2(_Tree, D), LEX).

%! decimalLexicalRep1(D)//
% Processes a decimal value that is internally represented as
% a SWI-Prolog float.
%
% @see For a full DCG alternative, that does not use number_codes/2
%      nor format/3, see decimalLexicalRep2//2.

decimalLexicalRep1(D) -->
  {var(D)}, !,
  (sign(H1) -> {L1 = [H1|T1]} ; {L1 = T1}),
  dcg_multi(decimal_digit, between(1,_), T1),
  (dot(H2), dcg_multi(decimal_digit, _, T2) -> {L2 = [H2|T2]} ; {L2 = T2}),
  {append(L1, L2, L), number_codes(D, L)}.
decimalLexicalRep1(D, H, T):-
  number(D), !,
  format(codes(H,T), '~w', [D]).

%! decimalLexicalRep2(-Tree:compound, ?Decimal:compound)//
% Processes a decimal value using the compound term notation for a decimal.
% Also returns the parse tree.

decimalLexicalRep2(T0, decimal(I,N)) -->
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
    I is copysign(I_, Sign),
    length(I2s, N),
    parse_tree(decimalLexicalRep, [T1,I1s,T3,I2s], T0)
  }.
decimalLexicalRep2(T0, decimal(I,N)) -->
  {
    integer(I), integer(N), !,
    length(I2s, N),
    (I < 0 -> Sign = -1 ; Sign = 1),
    I_ is copysign(I, Sign),
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

