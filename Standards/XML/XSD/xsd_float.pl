:- module(
  xsd_float,
  [
    doubleCanonicalMap/2, % +Double:double
                          % -LEX:list(code)
    doubleLexicalMap/2, % +LEX:list(code)
                        % -Double:double
    floatCanonicalMap/2, % +Float:float
                         % -LEX:list(code)
    floatLexicalMap/2 % +LEX:list(code)
                      % -Float:float
  ]
).

/** <module> XSD_FLOAT

The *|=float= datatype|* is patterned after the IEEE single-precision 32-bit
floating point datatype IEEE 754-2008. Its value space is a subset of the
rational numbers. Floating point numbers are often used to approximate
arbitrary real numbers.

### Value Space

The value space of float contains the non-zero numbers =|M * 2 ** E|=,
where =M= is an integer whose absolute value is less than =|2 ** 24|=, and =E=
is an integer between =|−149|= and =104=, inclusive.
In addition to these values, the value space of float also contains the
following special values:
  * =positiveZero=, sometimes called '0'.
  * =negativeZero=, sometimes called '-0'.
  * =positiveInfinity=, sometimes called 'INF'.
  * =negativeInfinity=, sometimes called '-INF'.
  * =notANumber=, sometimes called 'NaN'.

#### Equality & identity

Equality is identity, except that =|0 = −0|= (although they are not identical)
and =|NaN ≠ NaN|=  (although =NaN= is of course identical to itself).

=0= and =|−0|= are thus equivalent for purposes of enumerations and
identity constraints, as well as for minimum and maximum values.

#### Order

  * For the basic values, the order relation on float is the order relation
    for rational numbers.
  * =INF= is greater than all other non-=NaN= values.
  * =|−INF|= is less than all other non-=NaN= values.
  * =NaN= is incomparable with any value in the value space including itself.
  * =0= and =|−0|= are greater than all the negative numbers and less than all
  * the positive numbers.

#### Bounding facets

Any value incomparable with the value used for the four bounding facets
(=minInclusive=, =maxInclusive=, =minExclusive=, and =maxExclusive=)
will be excluded from the resulting restricted ·value space·.
In particular, when NaN is used as a facet value for a bounding facet,
since no float values are comparable with it, the result is a value space
that is empty. If any other value is used for a bounding facet, NaN will be
excluded from the resulting restricted value space; to add NaN back in
requires union with the NaN-only space (which may be derived using the
pattern 'NaN').

#### Compatibility

The Schema 1.0 version of this datatype did not differentiate between
0 and −0 and NaN was equal to itself. The changes were made to make the
datatype more closely mirror IEEE 754-2008.

### Lexical space

The lexical space of float is the set of all decimal numerals with or without
a decimal point, numerals in scientific (exponential) notation, and the
literals =|'INF'|=, =|'+INF'|=, |='-INF'|=, and =|'NaN'|=.

#### Rounding

IEEE 754-2008 sets forth requirements on a lexical mapping for the
single-precision floating-point datatype.
Since IEEE allows some variation in rounding of values, processors conforming
to this specification may exhibit some variation in their lexical mappings.
The lexical mapping floatLexicalMap// is provided as an example of a simple
algorithm that yields a conformant mapping, and that provides the most
accurate rounding possible, and is thus useful for insuring
inter-implementation reproducibility and inter-implementation round-tripping.
The simple rounding algorithm used in floatLexicalMap// may be more
efficiently implemented using the algorithms of Clinger1990.

#### Canonical mapping

The canonical mapping floatCanonicalMap// is provided as an example of
a mapping that does not produce unnecessarily long canonical representations.
Other algorithms which do not yield identical results for mapping from float
values to character strings are permitted by IEEE 754-2008.

### Facets

The float datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

Datatypes derived by restriction from float may specify values for
the following constraining facets:
  * =assertions=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =pattern=

The float datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = true|=
  * =|cardinality = finite|=
  * =|numeric = true|=

### Double-precision

The *|=double= datatype|* is patterned after the IEEE double-precision 64-bit
floating point datatype IEEE 754-2008. Each floating point datatype has a
value space that is a subset of the rational numbers.

The only significant differences between float and double are the three
defining constants 53 (vs 24), −1074 (vs −149), and 971 (vs 104).

The value space of double contains the non-zero numbers =|m × 2e|=, where =m=
is an integer whose absolute value is less than =253=, and =e= is an integer
between =−1074= and =971=, inclusive.

--

@author Wouter Beek
@see IEEE 754-2008
@tbd Implement restrictions to the lexical map as defined in IEEE 754-2008.
@tbd Implement the efficient rounding algorithm in Clinger1990.
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(library(debug)).
:- use_module(math(math_ext)).
:- use_module(xsd(xsd_decimal)).

:- debug(xsd_float).



% CANONICAL MAPPING %

%! doubleCanonicalMap(+Double:or([float,atom]), -LEX:list(code))//
% Maps a float to its canonical representation, a floatRep//.
%
% @param Float A float value.
% @param LEX A literal matching floatRep//.

doubleCanonicalMap(Float, LEX):-
  phrase(floatCanonicalMap(double, Float), LEX).

%! floatApprox(+C:nonneg, +E:integer, +J:nonneg, -D:decimal) is nondet.
% Maps a decimal number =|C * 10 ** E|= to successive approximations.
%
% @param C A nonnegative integer.
% @param E An integer.
% @param J A nonnegative integer.
% @param D A decimal number.

floatApprox(C, E, J, D):-
  round(C, J, Round),
  D is Round * 10 ** E.

%! floatCanonicalMap(+Float:float, -LEX:list(code)) is det.
% Maps a float to its canonical representation, a floatRep//.
%
% @param Float A float value.
% @param LEX A literal matching floatRep//.

floatCanonicalMap(Float, LEX):-
  phrase(floatCanonicalMap(single, Float), LEX).

%! floatCanonicalMap(
%!   +Precision:oneof([double,single]),
%!   +Float:or([float,atom])
%! )//
% Maps a float to its canonical representation, a floatRep//.
%
% @param Float A float value.

floatCanonicalMap(_Precision, F) -->
  specialRepCanonicalMap(F), !.
floatCanonicalMap(_Precision, positiveZero) -->
  !, "0.0E0".
floatCanonicalMap(_Precision, negativeZero) -->
  !, "-0.0E0".
% =F= is numeric and non-zero.
floatCanonicalMap(Precision, F) -->
  {
    (F < 0 -> Sign = -1 ; Sign = 1),
    ABS is abs(F),
    % Let =C= be the smallest integer for which there exists an integer =E=
    % for which  =|abs(F) = C * 10 ** E|=.
    smallest_c(ABS, E, C),
    % Let =L= be the largest nonnegative integer for which
    % =|C * 10 ** E = floatingPointRound(floatApprox(C, E, L), 24, −149, 104)|=
    largest_l(Precision, C, E, Approx),
    F1 is copysign(Approx, Sign)
  },
  scientificCanonicalMap(F1).

%! largest_l(
%!   +Precision:oneof([double,single]),
%!   +Significant:positive_integer,
%!   +Exponent:integer,
%!   -Approx:float
%! ) is det
% I think this corrects for the number of trailing zeros?

largest_l(Precision, C, E, Approx):-
  largest_l(Precision, 0, C, E, _Round, _Approx, Approx).

largest_l(Precision, L, C, E, _OldRound, _OldApprox, SolApprox):-
  floatApprox(C, E, L, Approx),
  floating_point_precision(Precision, P, EMin, EMax),
  floatingPointRound(Approx, P, EMin, EMax, Round),
  Round =:= C * 10 ** E, !,
  succ(L, NextL),
  largest_l(Precision, NextL, C, E, Round, Approx, SolApprox).
largest_l(_Precision, TooBigL, _C, _E, Round, Approx, Approx):-
  succ(L, TooBigL),
  debug(xsd_float, 'L=~w:\tApprox=~w\tRound=~w', [L,Approx,Round]).

%! round(+N:decimal, +K:nonneg, -D:decimal) is det.
% Maps a decimal number to that value rounded by some power of =10=.
%
% @param N A decimal number.
% @param K A nonnegative integer.
% @param D A decimal number.

round(N, K, D):-
  X is (N / 10 ** K) + 0.5,
  div(X, 1, Y),
  D is Y * 10 ** K.

scientificCanonicalMap(N) -->
  ({N < 0} -> sign(Sign) ; {Sign = 1}),
  {N1 is Sign * N},
  unsignedScientificCanonicalMap(N1).

%! smallest_c(+ABS:float, -E:integer, -C:positive_integer) is det.

smallest_c(ABS, E2, C):-
  smallest_c(1, ABS, E1, C),
  E2 is integer(E1).

smallest_c(C, ABS, E, C):-
  E is log10(ABS / C),
  % =E= must be an integer.
  mod(E, 1, MOD),
  MOD =:= 0, !,
  debug(xsd_float, '~w = ~w * 10 ** ~w', [ABS,C,E]).
smallest_c(C, ABS, SolE, SolC):-
  succ(C, NextC),
  smallest_c(NextC, ABS, SolE, SolC).

%! specialRepCanonicalMap(
%!   +SpecialValue:oneof([negativeInfinity,notANumber,positiveInfinity])
%! )//
% Maps the special values used with some numerical datatypes to their
% canonical representations.
%
% @param SpecialValue One of =positiveInfinity=, =negativeInfinity=,
%        and =notANumber=.

specialRepCanonicalMap(negativeInfinity) -->
  "-INF".
specialRepCanonicalMap(notANumber) -->
  "NaN".
specialRepCanonicalMap(positiveInfinity) -->
  "INF".

unsignedScientificCanonicalMap(N) -->
  {
    X1 is log10(N),
    div(X1, 1, Y1),
    N1 is N / 10 ** Y1
  },
  unsignedDecimalPtCanonicalMap(N1),
  "E",
  {
    X2 is log10(N),
    div(X2, 1, N2)
  },
  noDecimalPtCanonicalMap(N2).



% LEXICAL MAPPING %

doubleLexicalMap(LEX, Double):-
  phrase(doubleRep(Double), LEX).

%! doubleRep(-Double:or([double,atom)))//
% ~~~{.ebnf}
% doubleRep ::= noDecimalPtNumeral
%             | decimalPtNumeral
%             | scientificNotationNumeral
%             | numericalSpecialRep
% ~~~
%
% ~~~{.re}
% (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee](\+|-)?[0-9]+)? |(\+|-)?INF|NaN
% ~~~
%
% @param Either a double precision floating point value or one of
%        =negativeZero=, =positiveZero=, =negativeInfinity=,
%        =positiveInfinity=.

doubleRep(N) -->
  floatRep(N).

floatLexicalMap(LEX, Float):-
  phrase(floatRep(Float), LEX).

%! floatRep(-Float:or([float,atom]))//
% ~~~{.ebnf}
% floatRep ::= noDecimalPtNumeral
%            | decimalPtNumeral
%            | scientificNotationNumeral
%            | numericalSpecialRep
% ~~~
% The floatRep production is equivalent to this RE:
% ~~~{.re}
% (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee](\+|-)?[0-9]+)?|(\+|-)?INF|NaN
% ~~~

floatRep(N) -->
  floatRep(single, N).

floatRep(_Precision, N) -->
  numericalSpecialRep(N).
floatRep(Precision, N) -->
  (
    noDecimalPtNumeral(Sign, N1)
  ;
    decimalPtNumeral(Sign, N1)
  ;
    scientificNotationNumeral(Sign, N1)
  ),
  {
    floating_point_precision(Precision, P, EMin, EMax),
    (N1 =\= 0 -> floatingPointRound(N1, P, EMin, EMax, N2) ; N2 = N1),
    (N2 =:= 0 -> (Sign > 0 -> N = negativeZero ; N = positiveZero) ; N = N2)
  }.

%! minimalNumericalSpecialRep(
%!   ?Literal:oneof([negativeInfinity,positiveInfinity,'NaN'])
%! )//
% ~~~{.ebnf}
% minimalNumericalSpecialRep ::= 'INF' | '-INF' | 'NaN'
% ~~~

minimalNumericalSpecialRep(positiveInfinity) -->
  "INF".
minimalNumericalSpecialRep(negativeInfinity) -->
  "-INF".
minimalNumericalSpecialRep('NaN') -->
  "NaN".

%! numericalSpecialRep(?Literal:oneof([negativeInfinity,positiveInfinity,'NaN']))//
% ~~~{.ebnf}
% numericalSpecialRep ::= '+INF' | minimalNumericalSpecialRep
% ~~~

numericalSpecialRep(positiveInfinity) -->
  "+INF".
numericalSpecialRep(Literal) -->
  minimalNumericalSpecialRep(Literal).

%! scientificNotationNumeral(-Sign:oneof([-1,1]), -N:float)//
% ~~~{.ebnf}
% scientificNotationNumeral ::= ('+' | '-')? unsignedScientificNotationNumeral
% ~~~

scientificNotationNumeral(Sign, N) -->
  (sign(Sign) ; {Sign = 1}),
  unsignedScientificNotationNumeral(N1),
  {N is copysign(N1, Sign)}.

%! unsignedScientificNotationNumeral(-N:float)//
% ~~~{.ebnf}
% unsignedScientificNotationNumeral ::=
%     (unsignedNoDecimalPtNumeral | unsignedDecimalPtNumeral)
%     ('e' | 'E')
%     noDecimalPtNumeral
% ~~~
%
% @see The DCG subrules are defined in module [xsd_decimal.pl].

unsignedScientificNotationNumeral(N) -->
  (unsignedNoDecimalPtNumeral(N1) ; unsignedDecimalPtNumeral(N1)),
  e,
  noDecimalPtNumeral(_Sign, N2),
  {N is N1 * 10 ** N2}.



% GENERIC PREDICATES %

%! floatingPointRound(
%!   +NV:float,
%!   +CWidth:positive_integer,
%!   +EMin:integer,
%!   +EMax:integer,
%!   -Round:or([float,oneof([negativeInfinity,positiveInfinity])])
%! ) is det.
% Rounds a non-zero decimal number to the nearest floating-point value.
%
% @param NV An initially non-zero decimal number
%        (may be set to zero during calculations).
% @param CWidth A positive integer.
%        The precision of the float.
% @param EMin An integer.
%        The minimum exponent of the float.
% @param EMax An integer greater than =EMin=.
%        The maximum exponent of the float.
% @param Round Either a decimal number or a special value
%        (=positiveInfinity= or =negativeInfinity=).

floatingPointRound(NV1, CWidth, EMin, EMax, Round):-
  % =S= be an integer initially 1,
  % =C= be a nonnegative integer, and
  % =E= be an integer.

  % Set =S= to =−1= when =|NV < 0|=.
  (NV1 < 0 -> Sign = -1 ; Sign = 1),

  % So select =E= that
  % =|2 ** CWidth * 2 ** (E − 1) =< abs(NV) < 2 ** CWidth * 2 ** E|=.
  ABS is abs(NV1),
  % The logarithm of 0 does not exist...
  (ABS =:= 0 -> E1 = EMin ; log(2, ABS, LOG), E1 is ceil(LOG - CWidth)),

  % OVERFLOW
  % When =|EMax < E|=, return:
  %   * =positiveInfinity=, when =S= is positive.
  %   * =negativeInfinity=, otherwise.
  (
    EMax < E1
  ->
    (Sign > 0 -> Round = positiveInfinity ; Round = negativeInfinity)
  ;
    % So select =C= that =|(C − 1) * 2 ** E =< abs(NV) < C * 2 ** E|=.
    C is ceil(ABS / 2 ** E1),

    % UNDERFLOW
    % When =|E < EMin|=, set =|E = EMin|=.
    (
      E1 < EMin
    ->
      E2 = EMin
    ;
      E2 = E1
    ),

    % Set =NV= to
    %   * =|C * 2 ** E|=, when =|abs(NV) > C * 2 ** E − 2 ** (E − 1)|=.
    %   * =|(C − 1) * 2 ** E|=, when =|abs(NV) < C * 2 ** E − 2 ** (E − 1)|=.
    %   * =|c * 2 ** E|= or =|(C − 1) * 2 ** E|=, according to whether =C=
    %     is even or =|C − 1|= is even, otherwise
    %     (i.e., =|abs(NV) = C * 2 ** E − 2 * (E − 1)|=, the midpoint
    %     between the two values).
    X is C * 2 ** E2 - 2 ** (E2 - 1),
    catch(
      assign_nv(ABS, X, C, E2, NV2),
      E,
      (
        Sign > 0
      ->
        Round = positiveInfinity
      ;
        Round = negativeInfinity
      )
    ),
    write(E), nl,
    % Return:
    %   * =|S * NV|=, when =|NV < 2 ** CWidth * 2 ** EMax|=.
    %   * =positiveInfinity=, when =S= is positive.
    %   * =negativeInfinity=, otherwise.
    Round is Sign * NV2
  ).

assign_nv(ABS, X, C, E2, NV2):-
  (
    ABS > X
  ->
    NV2 is C * 2 ** E2
  ;
    ABS < X
  ->
    NV2 is (C - 1) * 2 ** E2
  ;
    even(C)
  ->
    NV2 is C * 2 ** E2
  ;
    succ(Y, C),
    even(Y)
  ->
    NV2 is (C - 1) * 2 ** E2
  ;
    % MIDPOINT
    NV2 is C * 2 ** E2 - 2 ** (E2 - 1)
  ).

floating_point_precision(single, 24, -149,  104).
floating_point_precision(double, 53, -1074, 971).

