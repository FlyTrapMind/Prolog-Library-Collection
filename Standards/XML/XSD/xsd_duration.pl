:- module(
  xsd_duration,
  [
    durationCanonicalMap/2, % +Duration:compound
                            % -LEX:list(code)
    durationCanonicalMap//1, % +Duration:compound
    durationLexicalMap/2, % +LEX:list(code)
                          % -Duration:compound
% COMPONENTS
    duYearMonthFrag//1 % -Month:nonneg
  ]
).

/** <module> XSD_DURATION

*=duration=* is a datatype that represents durations of time.
The concept of duration being captured is drawn from those of ISO 8601,
specifically durations without fixed endpoints.
For example, "15 days" (whose most common lexical representation in duration
is =P15D=) is a duration value; "15 days beginning 12 July 1995" and
"15 days ending 12 July 1995" are not duration values.
Duration can provide addition and subtraction operations between duration
values and between duration/dateTime value pairs, and can be the result of
subtracting dateTime values. However, only addition to dateTime is required
for XML Schema processing and is defined in dateTimePlusDuration/2.

### Value Space

Duration values can be modelled as two-property tuples.
Each value consists of an integer number of months and a decimal number of
seconds. The seconds value must not be negative if the months value is
positive and must not be positive if the months is negative.

Durations are partially ordered.
Equality of duration is defined in terms of equality of dateTime;
order for duration is defined in terms of the order of dateTime.
Specifically, the equality or order of two duration values is determined
by adding each duration in the pair to each of the following four
dateTime values:
~~~
1696-09-01T00:00:00Z
1697-02-01T00:00:00Z
1903-03-01T00:00:00Z
1903-07-01T00:00:00Z
~~~
If all four resulting dateTime value pairs are ordered the same way
(less than, equal, or greater than), then the original pair of duration
values is ordered the same way; otherwise the original pair is incomparable.

These four values are chosen so as to maximize the possible differences
in results that could occur, such as the difference when adding =P1M= and
=P30D= (since =|P1M <> P30D|=). Example:
~~~
1697-02-01T00:00:00Z + P1M < 1697-02-01T00:00:00Z + P30D
1903-03-01T00:00:00Z + P1M > 1903-03-01T00:00:00Z + P30D
~~~
If two duration values are ordered the same way when added to each of these
four dateTime values, they will retain the same order when added to any
other dateTime values.
Therefore, two duration values are incomparable if and only if they can ever
result in different orders when added to any dateTime value.

Under the definition just given, two duration values are equal
if and only if they are identical.

Two totally ordered datatypes =yearMonthDuration= and =dayTimeDuration=
are derived from duration.

### Lexical mapping

The lexical representations of duration are more or less based on the pattern:
~~~
PnYnMnDTnHnMnS
~~~
More precisely, the lexical space is the set of character strings satisfying
durationLexicalRep//

~~~{.ebnf}
duYearFrag ::= unsignedNoDecimalPtNumeral 'Y'
duMonthFrag ::= unsignedNoDecimalPtNumeral 'M'
duDayFrag ::= unsignedNoDecimalPtNumeral 'D'
duHourFrag ::= unsignedNoDecimalPtNumeral 'H'
duMinuteFrag ::= unsignedNoDecimalPtNumeral 'M'
duSecondFrag ::= (unsignedNoDecimalPtNumeral | unsignedDecimalPtNumeral) 'S'
duYearMonthFrag ::= (duYearFrag duMonthFrag?) | duMonthFrag
duTimeFrag ::=
    'T'
    ( (duHourFrag duMinuteFrag? duSecondFrag?)
    | (duMinuteFrag duSecondFrag?)
    | duSecondFrag)
duDayTimeFrag ::= (duDayFrag duTimeFrag?) | duTimeFrag
~~~

### Facets

The duration datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

Datatypes derived by restriction from duration may also specify values for
the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The duration datatype has the following values for its ·fundamental facets·:

  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

The following built-in datatypes are derived from duration:
  * xsd_yearMonthDuration.pl
  * xsd_dayTimeDuration.pl

--

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(xsd(xsd_decimal)).



% CANONICAL MAPPING %

%! duDayCanonicalFragmentMap(+NumberOfDays:nonneg)//
% Maps a nonnegative integer, presumably the day normalized value from the
% seconds of a duration value, to a duDayFrag//, a fragment of a duration
% lexical representation.
%
% @param NumberOfDays A nonnegative integer.

duDayCanonicalFragmentMap(NumberOfDays) -->
  {NumberOfDays =:= 0}, !.
duDayCanonicalFragmentMap(NumberOfDays) -->
  unsignedNoDecimalPtCanonicalMap(NumberOfDays),
  "D".

%! duDayTimeCanonicalFragmentMap(+Seconds:float)
% Maps a nonnegative decimal number, presumably the absolute value of
% the seconds of a duration value, to a duDayTimeFrag//,
% a fragment of a duration lexical representation.
%
% @param Seconds A nonnegative decimal number.

duDayTimeCanonicalFragmentMap(NumberOfSeconds) -->
  {NumberOfSeconds =:= 0}, !,
  "T0S".
duDayTimeCanonicalFragmentMap(NumberOfSeconds1) -->
  {
    % Days.
    div(NumberOfSeconds1, 86400, NumberOfDays),
    % Hours.
    % h is (ss mod 86400) div 3600.
    mod(NumberOfSeconds1, 86400, X),
    div(X, 3600, NumberOfHours),
    % Minutes.
    % m is (ss mod 3600) div 60.
    mod(NumberOfSeconds1, 3600, Y),
    div(Y, 60, NumberOfMinutes),
    % Seconds.
    % s is ss mod 60.
    mod(NumberOfSeconds1, 60, NumberOfSeconds2)
  },
  duDayCanonicalFragmentMap(NumberOfDays),
  duTimeCanonicalFragmentMap(
    NumberOfHours,
    NumberOfMinutes,
    NumberOfSeconds2
  ).

%! duHourCanonicalFragmentMap(+NumberOfHours:nonneg)//
% Maps a nonnegative integer, presumably the hour normalized value from the
% seconds of a duration value, to a duHourFrag//, a fragment of a duration
% lexical representation.
%
% NumberOfHours A nonnegative integer.

duHourCanonicalFragmentMap(H) -->
  {H =:= 0}, !.
duHourCanonicalFragmentMap(H) -->
  unsignedNoDecimalPtCanonicalMap(H),
  "H".

%! duMinuteCanonicalFragmentMap(+NumberOfMinutes:nonneg)//
% Maps a nonnegative integer, presumably the minute normalized value from
% the ·seconds· of a duration value, to a duMinuteFrag//, a fragment of
% a duration lexical representation.
%
% @param NumberOfMinutes A nonnegative integer.

duMinuteCanonicalFragmentMap(M) -->
  {M =:= 0}, !.
duMinuteCanonicalFragmentMap(M) -->
  unsignedNoDecimalPtCanonicalMap(M),
  "M".

%! durationCanonicalMap(+Duration:compound, -LEX:list(code)) is det.

durationCanonicalMap(Duration, LEX):-
  phrase(durationCanonicalMap(Duration), LEX).

%! durationCanonicalMap(+Duration:compound)//
% Maps a duration's property values to durationLexicalRep// fragments and
% combines the fragments into a complete durationLexicalRep//.
%
% @param Duration A complete duration value.

durationCanonicalMap(duration(M1,S1)) -->
  {M1 > 0, S1 > 0}, !,
  "P",
  {M2 is abs(M1)},
  duYearMonthCanonicalFragmentMap(M2),
  {S2 is abs(S1)},
  duDayTimeCanonicalFragmentMap(S2).
durationCanonicalMap(duration(M1,S1)) -->
  {M1 =\= 0, S1 =:= 0}, !,
  minus_sign,
  "P",
  {M2 is abs(M1)},
  duYearMonthCanonicalFragmentMap(M2).
durationCanonicalMap(duration(M1,S1)) -->
  {M1 =:= 0}, !,
  minus_sign,
  "P",
  {S2 is abs(S1)},
  duDayTimeCanonicalFragmentMap(S2).

%! duSecondCanonicalFragmentMap(+NumberOfSeconds:float)//
% Maps a nonnegative decimal number, presumably the second normalized value
% from the seconds of a duration value, to a duSecondFrag//, a fragment of
% a duration lexical representation.
%
% @param NumberOfSeconds A nonnegative decimal number.

duSecondCanonicalFragmentMap(S) -->
  {S =:= 0}, !.
duSecondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsignedNoDecimalPtCanonicalMap(S),
  "S".
duSecondCanonicalFragmentMap(S) -->
  unsignedDecimalPtCanonicalMap(S),
  "S".

%! duTimeCanonicalFragmentMap(
%!   +NumberOfHours:nonneg,
%!   +NumberOfMinutes:nonneg,
%!   +NumberOfSeconds:float
%! )//
% Maps three nonnegative numbers, presumably the hour, minute, and second
% normalized values from a duration's seconds, to a duTimeFrag//, a fragment
% of a duration lexical representation.
%
% @param NumberOfHours A nonnegative integer.
% @param NumberOfMinutes A nonnegative integer.
% @param NumberOfSeconds A nonnegative decimal number.

duTimeCanonicalFragmentMap(H, M, S) -->
  {H =:= 0, M =:= 0, S =:= 0}, !.
duTimeCanonicalFragmentMap(H, M, S) -->
  "T",
  duHourCanonicalFragmentMap(H),
  duMinuteCanonicalFragmentMap(M),
  duSecondCanonicalFragmentMap(S).

%! duYearMonthCanonicalFragmentMap(+NumberOfMonths:nonneg)//
% Maps a nonnegative integer, presumably the absolute value of the months
% of a duration value, to a duYearMonthFrag//, a fragment of a duration
% lexical representation.
%
% @param NumberOfMonths A nonnegative integer.

duYearMonthCanonicalFragmentMap(NumberOfMonths1) -->
  {NumberOfYears is NumberOfMonths1 div 12},
  (
    {NumberOfYears =:= 0}
  ;
    unsignedNoDecimalPtCanonicalMap(NumberOfYears),
    "Y"
  ), !,
  {NumberOfMonths2 is NumberOfMonths1 mod 12},
  (
    {NumberOfMonths2 =:= 0}
  ;
    unsignedNoDecimalPtCanonicalMap(NumberOfMonths2),
    "M"
  ), !.



% LEXICAL MAPPING %

durationLexicalMap(LEX, D):-
  phrase(durationLexicalRep(D), LEX).

%! durationLexicalRep(-Duration:compound)//
% ~~~{.ebnf}
% durationLexicalRep ::=
%     '-'? 'P' ((duYearMonthFrag duDayTimeFrag?) | duDayTimeFrag)
% ~~~
%
% #### RE
%
% Seperate REs:
%   * =|-?P[0-9]+Y?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+(\.[0-9]+)?S)?)?|=
%   * =|.*[YMDHS].*|=, at least one field occurs.
%   * =|.*[^T]|=, where =T= cannot be the final character.
%
% Combined RE:
% ~~~{.re}
% -?P( ( ( [0-9]+Y([0-9]+M)?([0-9]+D)?
%      | ([0-9]+M)([0-9]+D)?
%      | ([0-9]+D)
%      )
%      (T ( ([0-9]+H)([0-9]+M)?([0-9]+(\.[0-9]+)?S)?
%         | ([0-9]+M)([0-9]+(\.[0-9]+)?S)?
%         | ([0-9]+(\.[0-9]+)?S)
%         )
%      )?
%   )
% | (T ( ([0-9]+H)([0-9]+M)?([0-9]+(\.[0-9]+)?S)?
%      | ([0-9]+M)([0-9]+(\.[0-9]+)?S)?
%      | ([0-9]+(\.[0-9]+)?S)
%      )
%   )
% )
% ~~~
%
% @param Duration A compound term of the form
%        =|duration(Months:integer,Seconds:float)|=.

durationLexicalRep(duration(M,S)) -->
  (minus_sign -> {Sign = -1} ; {Sign = 1}),
  "P",
  (
    duYearMonthFrag(Y),
    (duDayTimeFrag(D) ; {D = 0})
  ;
    {Y = 0},
    duDayTimeFrag(D)
  ),
  {
    M is copysign(Y, Sign),
    S is copysign(D, Sign)
  }.

%! duDayFrag(-Day:nonneg)//
% ~~~{.ebnf}
% duDayFrag ::= unsignedNoDecimalPtNumeral 'D'
% ~~~

duDayFrag(D) -->
  unsignedNoDecimalPtNumeral(D),
  "D".

%! duDayTimeFrag(-Seconds:nonneg)//
% ~~~{.ebnf}
% duDayTimeFrag ::= (duDayFrag duTimeFrag?) | duTimeFrag
% ~~~

duDayTimeFrag(DT) -->
  (
    duDayFrag(D), (duTimeFrag(T) ; {T = 0})
  ;
    {D = 0}, duTimeFrag(T)
  ),
  {DT is 86400 * D + T}.

%! duHourFrag(-Hour:nonneg)//
% ~~~{.ebnf}
% duHourFrag ::= unsignedNoDecimalPtNumeral 'H'
% ~~~

duHourFrag(H) -->
  unsignedNoDecimalPtNumeral(H),
  "H".

%! duMinuteFrag(-Minute:nonneg)//
% ~~~{.ebnf}
% duMinuteFrag ::= unsignedNoDecimalPtNumeral 'M'
% ~~~

duMinuteFrag(M) -->
  unsignedNoDecimalPtNumeral(M),
  "M".

%! duMonthFrag(-Month:nonneg)//
% ~~~{.ebnf}
% duMonthFrag ::= unsignedNoDecimalPtNumeral 'M'
% ~~~

duMonthFrag(M) -->
  unsignedNoDecimalPtNumeral(M),
  "M".

%! duSecondFrag(-Second:or([float,nonneg]))//
% ~~~{.ebnf}
% duSecondFrag ::= (unsignedNoDecimalPtNumeral | unsignedDecimalPtNumeral) 'S'
% ~~~

duSecondFrag(S) -->
  (unsignedNoDecimalPtNumeral(S) ; unsignedDecimalPtNumeral(S)),
  "S".

%! duTimeFrag(-Second:nonneg)//
% ~~~{.ebnf}
% duTimeFrag ::=
%     'T'
%     ((duHourFrag duMinuteFrag? duSecondFrag?)
%     | (duMinuteFrag duSecondFrag?)
%     | duSecondFrag)
% ~~~

duTimeFrag(S2) -->
  "T",
  (
    (duHourFrag(H), (duMinuteFrag(M) ; ""), (duSecondFrag(S1) ; ""))
  ;
    {H = 0}, (duMinuteFrag(M), (duSecondFrag(S1) ; ""))
  ;
    {H = 0}, {M = 0}, duSecondFrag(S1)
  ),
  {S2 is 3600 * H + 60 * M + S1}.

%! duYearFrag(-Year:nonneg)//
% ~~~{.ebnf}
% duYearFrag ::= unsignedNoDecimalPtNumeral 'Y'
% ~~~

duYearFrag(Y) -->
  unsignedNoDecimalPtNumeral(Y),
  "Y".

%! duYearMonthFrag(-Month:nonneg)//
% ~~~{.ebnf}
% duYearMonthFrag ::= (duYearFrag duMonthFrag?) | duMonthFrag
% ~~~

duYearMonthFrag(M2) -->
  (
    duYearFrag(Y),
    (duMonthFrag(M1) ; {M1 = 0})
  ;
    {Y = 0},
    duMonthFrag(M1)
  ),
  {M2 is 12 * Y + M1}.

