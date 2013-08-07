:- module(
  xsd_decimal,
  [
    decimalCanonicalMap/2, % +Decimal:number
                           % -LEX:list(code)
    decimalLexicalMap/2 % ?LEX:list(code)
                        % ?Decimal:number
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



% CANONICAL MAPPING %

%! decimalCanonicalMap(+Decimal:number, -LEX:list(code)) is det.

decimalCanonicalMap(N, LEX):-
  phrase(decimalCanonicalMap(N), LEX).

%! decimalCanonicalMap(+Decimal:number)//

decimalCanonicalMap(I) -->
  {integer(I)}, !,
  noDecimalPtCanonicalMap(I).
decimalCanonicalMap(F) -->
  decimalPtCanonicalMap(F).

%! decimalPtCanonicalMap(+Decimal:float)//

decimalPtCanonicalMap(F) -->
  {F < 0.0}, !,
  "-",
  {G is copysign(F, -1)},
  unsignedDecimalPtCanonicalMap(G).
decimalPtCanonicalMap(F) -->
  unsignedDecimalPtCanonicalMap(F).

%! fractionDigitsCanonicalFragmentMap(?Fraction:between(0.0,1.0))//

fractionDigitsCanonicalFragmentMap(0.0) --> !, [].
fractionDigitsCanonicalFragmentMap(F) -->
  {G is (F mod 1) * 10, H is G div 1},
  digit(H),
  fractionDigitsCanonicalFragmentMap(G).

%! noDecimalPtCanonicalMap(+Integer:integer)//

noDecimalPtCanonicalMap(I) -->
  {I < 0},
  "-",
  {J is copysign(I, -1)},
  unsignedNoDecimalPtCanonicalMap(J).
noDecimalPtCanonicalMap(I) -->
  unsignedNoDecimalPtCanonicalMap(I).

%! unsignedDecimalPtCanonicalMap(+Decimal:between(0.0,_))//

unsignedDecimalPtCanonicalMap(F) -->
  {G is F div 1},
  unsignedNoDecimalPtCanonicalMap(G),
  ".",
  {H is F mod 1},
  fractionDigitsCanonicalFragmentMap(H).

%! unsignedNoDecimalPtCanonicalMap(+Integer:nonneg)//

unsignedNoDecimalPtCanonicalMap(0) --> !, [].
unsignedNoDecimalPtCanonicalMap(F) -->
  {G is F mod 10, H is G div 10},
  unsignedNoDecimalPtCanonicalMap(H),
  {digit(G)}.





% LEXICAL MAPPING %

%! decimalLexicalMap(?LEX:list(code), ?Decimal:number) is nondet.

decimalLexicalMap(LEX, N):-
  phrase(decimalLexicalRep(N), LEX).

%! decimalLexicalRep(?Decimal:float)//
% ~~~{.ebnf}
% decimalLexicalRep ::= decimalPtNumeral | noDecimalPtNumeral
% ~~~

decimalLexicalRep(N) -->
  decimalPtNumeral(N).
decimalLexicalRep(N) -->
  noDecimalPtNumeral(N).

%! decimalPtNumeral(?Decimal:float)//
% ~~~{.ebnf}
% decimalPtNumeral ::= ('+' | '-')? unsignedDecimalPtNumeral
% ~~~

decimalPtNumeral(N):-
  {var(N)}, !,
  (sign(S) ; {S = 1}),
  unsignedDecimalPtNumeral(M),
  {M is copysign(N, S)}.
decimalPtNumeral(N) -->
  (sign(S) ; {S = 1}),
  {M is copysign(N, S)},
  unsignedDecimalPtNumeral(M).

%! fracFrag(-Fraction:between(0.0,1.0))//
% ~~~{.ebnf}
% fracFrag ::= digit+
% ~~~

fracFrag(F) -->
  fracFrag(0, F).

fracFrag(_, 0.0) --> !, [].
fracFrag(I, NewSum) -->
  digit(D),
  {succ(I, NewI)},
  fracFrag(NewI, Sum),
  {NewSum is Sum + D * 10 ** I}.

%! noDecimalPtNumeral(?Integer:integer)//
% ~~~{.ebnf}
% noDecimalPtNumeral ::= ('+' | '-')? unsignedNoDecimalPtNumeral
% ~~~

noDecimalPtNumeral(N) -->
  {var(N)}, !,
  (sign(S) ; {S = 1}),
  unsignedNoDecimalPtNumeral(M),
  {M is copysign(N, S)}.
noDecimalPtNumeral(N) -->
  (sign(S) ; {S = 1}),
  {M is copysign(N, S)},
  unsignedNoDecimalPtNumeral(M).

%! unsignedDecimalPtNumeral(?Decimal:float)//
% ~~~{.ebnf}
% unsignedDecimalPtNumeral ::= (unsignedNoDecimalPtNumeral '.' fracFrag?)
%                              | ('.' fracFrag)
% ~~~

unsignedDecimalPtNumeral(F) -->
  unsignedNoDecimalPtNumeral(N1),
  ".",
  (fracFrag(N2), {N is N1 + N2} ; {N = N1}).
unsignedDecimalPtNumeral(F) -->
  ".",
  fracFrag(N).

%! unsignedNoDecimalPtNumeral(?Integer:nonneg)//
% ~~~{.ebnf}
% unsignedNoDecimalPtNumeral ::= digit+
% ~~~

unsignedNoDecimalPtNumeral(N) -->
  unsignedNoDecimalPtNumeral(_, N).

unsignedNoDecimalPtNumeral(NewToEnd, NewN) -->
  digit(D),
  unsignedNoDecimalPtNumeral(ToEnd, N),
  {NewN is N + D * 10 ** ToEnd, succ(ToEnd, NewToEnd)}.
unsignedNoDecimalPtNumeral(0, 0) --> [].

