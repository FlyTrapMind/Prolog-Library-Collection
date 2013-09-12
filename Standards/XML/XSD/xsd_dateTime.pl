:- module(
  xsd_dateTime,
  [
    dateTimeCanonicalMap/2, % +DateTime:compound
                            % -LEX:list(code)
    dateTimeLexicalMap/2, % +LEX:list(code)
                          % -DateTime:compound
% COMPONENTS
    dayCanonicalFragmentMap//1, % +Day:between(1,31)
    dayFrag//1, % -Day:between(1,31)
    dayInMonth/2, % +Month:between(1,12)
                  % +DayInMonth:between(1,31)
    dayInMonth/3, % +Year:integer
                  % +Month:between(1,12)
                  % +DayInMonth:between(1,31)
    daysInMonth/3, % ?Year:integer
                   % +Month:between(1,12)
                   % ?DaysInMonth:between(28,31)
    endOfDayFrag//3, % -Hour:oneof([24])
                     % -Minute:oneof([0])
                     % -Second:oneof([0])
    hourCanonicalFragmentMap//1, % +Hour:between(0,23)
    hourFrag//1, % -Hour:between(1,23)
    minuteCanonicalFragmentMap//1, % +Minute:between(0,59)
    minuteFrag//1, % -Minute:between(1-59)
    monthCanonicalFragmentMap//1, % +Month:between(1,12)
    monthFrag//1, % -Month:between(1,12)
    newDateTime/8, % ?Year:integer
                   % ?Month:between(1,12)
                   % ?Day:between(1,31)
                   % ?Hour:between(0,24)
                   % ?Minute:between(0,59)
                   % ?Second:between(0.0,60.0)
                   % ?Timezone:between(-840,840)
                   % -DateTime:compound
    secondCanonicalFragmentMap//1, % +Second:between(0.0,60.0)
    secondFrag//1, % -Second:float
    timeOnTimeline/2, % +DateTime:compound
                      % -Seconds:float
    timezoneCanonicalFragmentMap//1, % +Timezone:between(-840,840)
    timezoneFrag//1, % -Minutes:between(-840,840)
    yearCanonicalFragmentMap//1, % +Year:integer
    yearFrag//1 % -Year:integer
  ]
).

/** <module> XSD_DATE_TIME

*=dateTime=* represents instants of time, optionally marked with
a particular time zone offset.

### Value space

dateTime uses the date/timeSevenPropertyModel, with no properties except
timezoneOffset permitted to be absent.
The timezoneOffset property remains optional.

#### Constraint: Day-of-month Values

The day value must be no more than 30 if month is one of 4, 6, 9, or 11;
no more than 28 if month is 2 and year is not divisible by 4,
or is divisible by 100 but not by 400;
and no more than 29 if month is 2 and year is divisible by 400,
or by 4 but not by 100.

#### Order

dateTime values are ordered by their timeOnTimeline/2 value.

Since the order of a dateTime value having a timezoneOffset relative to
another value whose timezoneOffset is absent is determined by imputing
time zone offsets of both =|+14:00|= and =|−14:00|= to the value
with no time zone offset, many such combinations will be incomparable
because the two imputed time zone offsets yield different orders.

Although dateTime and other types related to dates and times have only
a partial order, it is possible for datatypes derived from dateTime
to have total orders, if they are restricted (e.g. using the pattern facet)
to the subset of values with, or the subset of values without,
time zone offsets. Similar restrictions on other date- and time-related
types will similarly produce totally ordered subtypes.

Note, however, that such restrictions do not affect the value shown,
for a given Simple Type Definition, in the =ordered= facet.

#### XSD 1.0 compatibility

Order and equality are essentially the same for dateTime in this version
of this specification as they were in version 1.0.
However, since values now distinguish time zone offsets,
equal values with different timezoneOffsets are not identical,
and values with extreme timezoneOffsets may no longer be equal
to any value with a smaller timezoneOffset.

##### Identity & equality

Values representing the same instant but having different time zone offsets
are equal but not identical.

### Lexical space

#### Redundant values

=Z=, =|+00:00|=, and =|-00:00|= are redundant.

Trailing fractional zero digits for secondFrag// are redundant.

Otherwise these mappings are one-to-one.

#### =endOfDayFrag=

There is no lexical mapping for endOfDayFrag//.
It is handled specially by the relevant lexical mappings
(e.g., =dateTimeLexicalMap=).

### Facets

The dateTime datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The dateTime datatype has the following constraining facets with the values
shown; these facets may be specified in the derivation of new types,
if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from dateTime may also specify values for
the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The dateTime datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

The following built-in datatype is derived from dateTime:
  * =dateTimeStamp=

### Concepts

*|Universal Coordinated Time (UTC)|* is an adaptation of TAI which closely
approximates UT1 by adding leap-seconds to selected UTC days.

### 7-property model for date and time

Two isomorphic ways to model moments in time:
  * Year, month, day, hour, minute and second.
  * Time (measured generally in seconds or days) from some starting moment.

The time zone offset is defined as the number of minutes of offset from UTC.
Values for the six primary properties are always stored in their local values
(the values shown in the lexical representations), rather than converted to
UTC.

7-property model:
  * =year=
  An integer.
  * =month=
  An integer between 1 and 12 inclusive.
  * =day=
  An integer between 1 and 31 inclusive, possibly restricted further
  depending on month and year.
  * =hour=
  An integer between 0 and 23 inclusive.
  * =minute=
  An integer between 0 and 59 inclusive.
  * =second=
  A decimal number greater than or equal to 0 and less than 60.
  * =timezoneOffset=
  An optional integer between −840 and 840 inclusive (i.e., 14 hours).

Values less than =1582= in the =year= property represent years in the
"proleptic Gregorian calendar".
A value of zero in the =year= property represents the year 1 BCE;
a value of =−1= represents the year 2 BCE, =−2= is 3 BCE, etc.

While calculating, property values from the dateTime
~~~
1972-12-31T00:00:00
~~~
are used to fill in for those that are absent, except that if day
is absent but month is not, the largest permitted day for that month is used.

### XSD 1.0 compatibility

In XSD 1.0 the year property was not permitted to have the value zero.
The year before the year 1 in the proleptic Gregorian calendar,
traditionally referred to as 1 BC or as 1 BCE, was represented by a year
value of −1, 2 BCE by −2, and so forth. Of course, many, perhaps most,
references to 1 BCE (or 1 BC) actually refer not to a year in the proleptic
Gregorian calendar but to a year in the Julian or "old style" calendar;
the two correspond approximately but not exactly to each other.

In this version of this specification, two changes are made in order to
agree with existing usage. First, year is permitted to have the value zero.
Second, the interpretation of year values is changed accordingly:
a year value of zero represents 1 BCE, −1 represents 2 BCE, etc.
This representation simplifies interval arithmetic and leap-year
calculation for dates before the common era (which may be why astronomers
and others interested in such calculations with the proleptic Gregorian
calendar have adopted it), and is consistent with the current edition of
ISO 8601.

Note that 1 BCE, 5 BCE, and so on (years 0000, −0004, etc. in the lexical
representation defined here) are leap years in the proleptic Gregorian
calendar used for the date/time datatypes defined here.
XSD 1.0 was unclear about the treatment of leap years before the common era.
If existing schemas or data specify dates of 29 February for any years
before the common era, then some values giving a date of 29 February which
were valid under a plausible interpretation of XSD 1.0 will be invalid
under this specification, and some which were invalid will be valid.
With that possible exception, schemas and data valid under the old
interpretation remain valid under the new.

### Leap seconds

A *|leap-second|* is an additional second added to the last day of December,
June, October, or March, when such an adjustment is deemed necessary by the
International Earth Rotation and Reference Systems Service (IERS) in order to
keep UTC within 0.9 seconds of observed astronomical time.
When leap seconds are introduced, the last minute in the day has more than
sixty seconds. In theory leap seconds can also be removed from a day,
but this has not yet occurred.
Leap seconds are not supported by the types defined here.

Because the simple types defined here do not support leap seconds,
they cannot be used to represent the final second, in UTC,

### Order

Values from any one date/time datatype using the seven-component model
(all except =duration=) are ordered the same as their timeOnTimeline/2 values,
except that if one value's timezoneOffset is absent and the other's is not,
and using maximum and minimum timezoneOffset values for the one whose
timezoneOffset is actually absent changes the resulting (strict) inequality,
the original two values are incomparable.

--

@author Wouter Beek
@see ISO 8601
@see ITU-R TF.460-6
@see USNO Historical List includes the times when the difference between
     TAI and UTC has changed tai_utc.txt.
@tbd Document what TAI and UT1 are.
@version 2013/08
*/

:- use_module(datetime(iso8601)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_multi)).
:- use_module(generics(meta_ext)).
:- use_module(math(math_ext)).
:- use_module(xsd(xsd_decimal)).



% CANONICAL MAPPING %

%! dateTimeCanonicalMap(+DateTime:compound, -LEX:list(code)) is det.

dateTimeCanonicalMap(DateTime, LEX):-
  once(phrase(dateTimeCanonicalMap(DateTime), LEX)).

%! dateTimeCanonicalMap(+DateTime:compound)//
% Maps a dateTime value to a dateTimeLexicalRep//.
%
% @param DateTime A compound term.

% The SWI-Prolog float representation for date-time values.
dateTimeCanonicalMap(Float) -->
  {float(Float)}, !,
  {stamp_date_time(Float, Compound, local)},
  dateTimeCanonicalMap(Compound).
% The SWI-Prolog compound term for date-time representations.
% Notice that the timezone (`TZ`) is an atom (e.g. `CEST`) and `Offset`
% is an integer representing the offset relative to UTC in seconds.
% XSD defines the timezone as the offset relative to UTC in minutes.
dateTimeCanonicalMap(date(Y,M,D,H,MM,S,Offset,_TZ,_DST)) --> !,
  {TZ is Offset / 60},
  dateTimeCanonicalMap(dateTime(Y,M,D,H,MM,S,TZ)).
dateTimeCanonicalMap(dateTime(Y,M,D,H,MM,S,TZ)) -->
  yearCanonicalFragmentMap(Y),
  hyphen,
  monthCanonicalFragmentMap(M),
  hyphen,
  dayCanonicalFragmentMap(D),
  "T",
  hourCanonicalFragmentMap(H),
  colon,
  minuteCanonicalFragmentMap(MM),
  colon,
  secondCanonicalFragmentMap(S),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.

%! dayCanonicalFragmentMap(+Day:between(1,31))//
% Maps an integer, presumably the day property of a
% date/timeSevenPropertyModel value, onto a dayFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Day An integer between 1 and 31 inclusive (may be limited further
%        depending on associated year and month).

dayCanonicalFragmentMap(D) -->
  unsTwoDigitCanonicalFragmentMap(D).

%! fourDigitCanonicalFragmentMap(+Integer:between(-9999,9999))//
% Maps an integer between =|-10000|= and =10000= onto an always-four-digit
% numeral.
%
% @param Integer An integer whose absolute value is less than =10000=.

fourDigitCanonicalFragmentMap(I1) -->
  ({I1 < 0} -> minus_sign ; ""),
  {I2 is copysign(I1, 1)},
  {N1 is I2 div 100},
  unsTwoDigitCanonicalFragmentMap(N1),
  {N2 is I2 mod 100},
  unsTwoDigitCanonicalFragmentMap(N2).

%! hourCanonicalFragmentMap(+Hour:between(0,23))//
% Maps an integer, presumably the hour property of a
% date/timeSevenPropertyModel value, onto a hourFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Hour An integer between 0 and 23 inclusive.

hourCanonicalFragmentMap(H) -->
  unsTwoDigitCanonicalFragmentMap(H).

%! minuteCanonicalFragmentMap(+Minute:between(0,59))//
% Maps an integer, presumably the minute property of a
% date/timeSevenPropertyModel value, onto a minuteFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Minute An integer between 0 and 59 inclusive.

minuteCanonicalFragmentMap(M) -->
  unsTwoDigitCanonicalFragmentMap(M).

%! monthCanonicalFragmentMap(+Month:between(1,12))//
% Maps an integer, presumably the month property of a
% date/timeSevenPropertyModel value, onto a monthFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Month An integer between 1 and 12 inclusive.

monthCanonicalFragmentMap(M) -->
  unsTwoDigitCanonicalFragmentMap(M).

%! secondCanonicalFragmentMap(+Second:between(0.0,60.0))//
% Maps a decimal number, presumably the second property of a
% date/timeSevenPropertyModel value, onto a secondFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Second A nonnegative decimal number less than 70.

secondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsTwoDigitCanonicalFragmentMap(S).
secondCanonicalFragmentMap(S) -->
  {div(S, 1.0, N1)},
  unsTwoDigitCanonicalFragmentMap(N1),
  dot,
  {mod(S, 1.0, N2)},
  fractionDigitsCanonicalFragmentMap(N2).

%! timezoneCanonicalFragmentMap(+Timezone:between(-840,840))//
% Maps an integer, presumably the timezoneOffset property of a
% date/timeSevenPropertyModel value, onto a timezoneFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Timezone An integer between =|−840|= and =840= inclusive.

timezoneCanonicalFragmentMap(0) -->
  "Z".
timezoneCanonicalFragmentMap(TZ1) -->
  minus_sign,
  {TZ2 is copysign(TZ1, 1)},
  {N1 is TZ2 div 60},
  unsTwoDigitCanonicalFragmentMap(N1),
  colon,
  {N2 is TZ2 mod 60},
  unsTwoDigitCanonicalFragmentMap(N2).

%! unsTwoDigitCanonicalFragmentMap(+Integer:between(0,99))//
% Maps a nonnegative integer less than =100= onto an unsigned
% always-two-digit numeral.
%
% @paam Integer A nonnegative integer less than =100=.

unsTwoDigitCanonicalFragmentMap(I) -->
  {D1 is I div 10},
  decimal_digit(D1),
  {D2 is I mod 10},
  decimal_digit(D2).

%! yearCanonicalFragmentMap(+Year:integer)//
% Maps an integer, presumably the year property of a
% date/timeSevenPropertyModel value, onto a yearFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @param Year An integer.

yearCanonicalFragmentMap(Y) -->
  {abs(Y) > 9999}, !,
  noDecimalPtCanonicalMap(Y).
yearCanonicalFragmentMap(Y) -->
  fourDigitCanonicalFragmentMap(Y).



% LEXICAL MAPPING %

%! dateTimeLexicalMap(+LEX:list(code), -DateTime:compound) is det.

dateTimeLexicalMap(LEX, DateTime):-
  once(phrase(dateTimeLexicalRep(DateTime), LEX)).

%! dateTimeLexicalRep(-DateTime:compound)//
% Subsequent =|-|=, =T=, and =|:|=, separate the various numerals.
%
% #### Grammar definitions
%
% ~~~{.ebnf}
% dateTimeLexicalRep ::=
%     yearFrag '-' monthFrag '-' dayFrag 'T'
%     ((hourFrag ':' minuteFrag ':' secondFrag) | endOfDayFrag)
%     timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})
% -(0[1-9]|1[0-2])
% -(0[1-9]|[12][0-9]|3[01])
% T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?|(24:00:00(\.0+)?))
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~
%
% Note that the day-of-month representation constraint is not yet encoded in
% these definitions.
%
% #### Example
%
% Noon on 10 October 2002, Central Daylight Savings Time as well as
% Eastern Standard Time in the U.S.:
% ~~~
% 2002-10-10T12:00:00−05:00
% 2002-10-10T17:00:00Z
% ~~~
%
% #### XSD 1.0 compatibility
%
% XSD 1.1 distinguished between the terms 'timezone' and 'timezone offset'.
% XSD 1.0 did not make this distinction, but used the term 'timezone' for
% the time zone offset.
%
% @param DateTime A compound term.
% @see W3C Working with Time Zones (WG Note)
%      http://www.w3.org/TR/2011/NOTE-timezone-20110705/

dateTimeLexicalRep(DT) -->
  yearFrag(Y), hyphen, monthFrag(M), hyphen, dayFrag(D),
  "T",
  (
    hourFrag(H), colon, minuteFrag(MM), colon, secondFrag(S)
  ;
    endOfDayFrag(H, MM, D)
  ),
  (timezoneFrag(TZ) ; ""),
  {newDateTime(Y, M, D, H, MM, S, TZ, DT)}.

%! dayFrag(-Day:between(1,31))//
% Processes a day value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% dayFrag ::= ('0' [1-9]) | ([12] digit) | ('3' [01])
% ~~~

dayFrag(D) -->
  zero(C1),
  nonzero_decimal_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.
dayFrag(D) -->
  (one(C1) ; two(C1)),
  decimal_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.
dayFrag(D) -->
  three(C1),
  binary_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.

%! endOfDayFrag(-Hour:oneof([24]), -Minute:oneof([0]), -Second:oneof([0]))//
% Combines the hourFrag//, minuteFrag//, secondFrag,
% and their separators to represent midnight of the day,
% which is the first moment of the next day.
%
% ~~~{.ebnf}
% endOfDayFrag ::= '24:00:00' ('.' '0'+)?
% ~~~

endOfDayFrag(24, 0, 0) -->
  "24:00:00",
  (dot, dcg_multi(zero)).

%! hourFrag(-Hour:between(1,23))//
% Processes an hour value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% hourFrag ::= ([01] digit) | ('2' [0-3])
% ~~~

hourFrag(H) -->
  binary_digit(_, C1),
  decimal_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(H), [C1,C2])}.
hourFrag(H) -->
  two(C1),
  (binary_digit(_, C2) ; two(C2) ; three(C2)),
  {phrase(unsignedNoDecimalPtNumeral(H), [C1,C2])}.

%! minuteFrag(-Minute:between(1-59))//
% Processes a minute value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% minuteFrag ::= [0-5] digit
% ~~~

minuteFrag(M) -->
  (
    binary_digit(_, C1)
  ;
    two(C1)
  ;
    three(C1)
  ;
    four(C1)
  ;
    five(C1)
  ),
  decimal_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.

%! monthFrag(-Month:between(1,12))//
% Processes a month value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% monthFrag ::= ('0' [1-9]) | ('1' [0-2])
% ~~~

monthFrag(M) -->
  zero(C1),
  nonzero_decimal_digit(_, C2),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.
monthFrag(M) -->
  one(C1),
  (binary_digit(_, C2) ; two(C2)),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.

%! secondFrag(-Second:float)//
% Processes a second value, i.e. a numeral consisting of exactly two
% decimal digits, or two decimal digits, a decimal point, and
% one or more trailing digits.
%
% ~~~{.ebnf}
% secondFrag ::= ([0-5] digit) ('.' digit+)?
% ~~~

secondFrag(S) -->
  (binary_digit(_Digit1, C1) ; two(C1) ; three(C1) ; four(C1) ; five(C1)),
  decimal_digit(_Digit2, C2),
  (
    dot(C3),
    dcg_multi(decimal_digit, 1-_, _Digits, CT, []),
    {phrase(unsignedDecimalPtNumeral(S), [C1,C2,C3|CT])}
  ;
    {phrase(unsignedNoDecimalPtNumeral(S), [C1,C2])}
  ).

%! timezoneFrag(-Minutes:between(-840,840))//
% Processes an offset between UTC and local time.
% Time zone offsets are a count of minutes (expressed as a count of hours and
% a count of minutes) that are added or subtracted from UTC time to get
% the "local" time.
%
% =Z= is an alternative representation of the time zone offset =|00:00|=,
% which is zero minutes from UTC.
%
% ~~~{.ebnf}
% timezoneFrag ::= 'Z'
%                | ('+' | '-')
%                  (('0' digit | '1' [0-3]) ':' minuteFrag | '14:00')
% ~~~

timezoneFrag(0) -->
  "Z".
timezoneFrag(840) -->
  "14:00".
timezoneFrag(TZ) -->
  sign(Sign),
  (
    zero(C1), decimal_digit(_, C2)
  ;
    one(C1), (binary_digit(_, C2) ; three(C2))
  ),
  % @compat Here we deviate from the XSD 1.1 standard,
  %         which uses unsignedDecimalPtNumeral//1 instead.
  %         But note that the definition of timezoneFrag//1
  %         excludes the appearance of a decimal separator.
  {phrase(unsignedNoDecimalPtNumeral(N1), [C1,C2])},
  colon,
  minuteFrag(N2),
  {TZ is copysign(N1 * 60 + N2, Sign)}.

%! yearFrag(-Year:integer)//
% Processes a year valie, i.e. a numeral consisting of at least four,
% decimal digits optionally preceded by a minus sign;
% leading '0' digits are prohibited except to bring the digit count
% up to four.
%
% ~~~{.ebnf}
% yearFrag ::= '-'? (([1-9] digit digit digit+)) | ('0' digit digit digit))
% ~~~
%
% @param Year An integer.

yearFrag(Y) -->
  (minus_sign(S), {Cs = [S,Code|Codes]} ; {Cs = [Code|Codes]}),
  (
    nonzero_decimal_digit(_Digit1, Code),
    dcg_multi(decimal_digit, 3-_, _Digits1, Codes, [])
  ;
    zero(Code),
    dcg_multi(decimal_digit, 3, _Digits2, Codes, [])
  ),
  {
    phrase(noDecimalPtNumeral(S, I), Cs),
    Y is copysign(I, S)
  }.



% SUPPORT PREDICATES %

%! dayInMonth(+Month:between(1,12), +DayInMonth:between(28,31)) is semidet.
% Succeeds if the given day can occur in the given month.
%
% The day value must be no more than 30 if month is one of 4, 6, 9, or 11,
% and no more than 29 if month is 2.

dayInMonth(2, D):- !,
  between(1, 29, D).
dayInMonth(M, D):-
  memberchk(M, [4,6,9,11]), !,
  between(1, 30, D).
dayInMonth(_M, D):-
  between(1, 31, D).

dayInMonth(Y, M, D):-
  var(Y), !,
  dayInMonth(M, D).
dayInMonth(Y, M, D):-
  daysInMonth(Y, M, MaxD),
  between(1, MaxD, D).

%! daysInMonth(
%!   ?Year:integer,
%!   +Month:between(1,12),
%!   -DaysInMonth:between(28,31)
%! ) is det.
%! daysInMonth(
%!   ?Year:integer,
%!   +Month:between(1,12),
%!   +DaysInMonth:between(28,31)
%! ) is semidet.
% Returns the number of the last day of the month for any combination
% of year and month.
%
% @param Year An optional integer.
% @param Month An integer between 1 and 12.
% @param DaysInMonth An integer between 28 and 31 inclusive.

% When m is 2 and y is not evenly divisible by 4,
% or is evenly divisible by 100 but not by 400, or is absent.
daysInMonth(Y, 2, 28):-
  (var(Y) ; \+ iso8601_leap_year(Y)), !.
% When m is 2 and y is evenly divisible by 400,
% or is evenly divisible by 4 but not by 100,
daysInMonth(Y, 2, 29):-
  iso8601_leap_year(Y), !.
% When m is 4, 6, 9, or 11.
daysInMonth(_Y, M, 30):-
  memberchk(M, [4,6,9,11]), !.
% Otherwise, i.e. m is 1, 3, 5, 7, 8, 10, or 12.
daysInMonth(_Y, _M, 31).

%! newDateTime(
%!   ?Year:integer,
%!   ?Month:between(1,12),
%!   ?Day:between(1,31),
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59),
%!   ?Second:between(0.0,60.0),
%!   ?Timezone:between(-840,840),
%!   -DateTime:compound
%! ) is det.
% Returns an instance of the date/timeSevenPropertyModel with property values
% as specified in the arguments. If an argument is omitted, the
% corresponding property is set to absent.
%
% @param Year An optional integer.
% @param Month An optional integer between 1 and 12 inclusive.
% @param Day An optional integer between 1 and 31 inclusive.
% @param Hour An optional integer between 0 and 24 inclusive.
% @param Minute An optional integer between 0 and 59 inclusive.
% @param Second An optional decimal number greater than or equal to
%        0 and less than 60.
% @param Timezone An optional integer between −840 and 840 inclusive.
%
% ~~~
% dt be an instance of the date/timeSevenPropertyModel
% yr be Yr when Yr is not absent, otherwise 1
% mo be Mo when Mo is not absent, otherwise 1
% da be Da when Da is not absent, otherwise 1
% hr be Hr when Hr is not absent, otherwise 0
% mi be Mi when Mi is not absent, otherwise 0
% se be Se when Se is not absent, otherwise 0
%
% normalizeSecond(yr, mo, da, hr, mi, se)
% Set the year property of dt to absent when Yr is absent, otherwise yr.
% Set the month property of dt to absent when Mo is absent, otherwise mo.
% Set the day property of dt to absent when Da is absent, otherwise da.
% Set the hour property of dt to absent when Hr is absent, otherwise hr.
% Set the minute property of dt to absent when Mi is absent, otherwise mi.
% Set the second property of dt to absent when Se is absent, otherwise se.
% Set the timezoneOffset property of dt to Tz
% Return dt.
% ~~~

newDateTime(Y1, M1, D1, H1, MM1, S1, TZ, DT):-
  % Set the values that are used for performing the nprmalization.
  default(Y1,  1,   Y2 ),
  default(M1,  1,   M2 ),
  default(D1,  1,   D2 ),
  default(H1,  0,   H2 ),
  default(MM1, 0,   MM2),
  default(S1,  0.0, S2 ),
  normalizeSecond(Y2, M2, D2, H2, MM2, S2, Y3, M3, D3, H3, MM3, S3),

  % Variables stay variable.
  % Non-variables get the normalized value.
  var_or_value(Y1,  Y3,  Y4 ),
  var_or_value(M1,  M3,  M4 ),
  var_or_value(D1,  D3,  D4 ),
  var_or_value(H1,  H3,  H4 ),
  var_or_value(MM1, MM3, MM4),
  var_or_value(S1,  S3,  S4 ),
  DT = dateTime(Y4,M4,D4,H4,MM4,S4,TZ).

%! normalizeDay(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   -NormalizedYear:integer,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer
%! ) is det.
% If month is out of range, or day is out of range for the appropriate month,
% then adjust values accordingly, otherwise make no change.
%
% ~~~
% normalizeMonth(yr, mo)
% Repeat until da is positive and not greater than daysInMonth(yr, mo):
%   If da exceeds daysInMonth(yr, mo) then:
%     Subtract that limit from da.
%     Add 1 to mo.
%     normalizeMonth(yr, mo)
%   If da is not positive then:
%     Subtract 1 from mo.
%     normalizeMonth(yr, mo)
%     Add the new upper limit from the table to da.
% ~~~

normalizeDay(Y1, M1, D1, Y2, M2, D2):-
  normalizeMonth(Y1, M1, Y3, M3),
  normalizeDay_(Y3, M3, D1, Y2, M2, D2).

normalizeDay_(Y1, M1, D1, Y2, M2, D2):-
  daysInMonth(Y1, M1, D1Max),
  (
    D1 > D1Max
  ->
    D3 is D1 - D1Max,
    MX is M1 + 1,
    normalizeMonth(Y1, MX, Y3, M3),
    normalizeDay_(Y3, M3, D3, Y2, M2, D2)
  ;
    D1 < 0
  ->
    MX is M1 - 1,
    normalizeMonth(Y1, MX, Y3, M3),
    daysInMonth(Y3, M3, D3Max),
    D3 is D1 + D3Max,
    normalizeDay_(Y3, M3, D3, Y2, M2, D2)
  ;
    Y2 = Y1,
    M2 = M1,
    D2 = D1
  ).

%! normalizeMinute(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   +Hour:integer,
%!   +Minute:integer,
%!   -NormalizedYear:itneger,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer,
%!   -NormalizedHour:integer,
%!   -NormalizedMinute:integer
%! ) is det.
% Normalizes minute, hour, month, and year values to values that obey
% the appropriate constraints.
%
% ~~~
% Add mi div 60 to hr.
% Set mi to mi mod 60.
% Add hr div 24 to da.
% Set hr to hr mod 24.
% normalizeDay(yr, mo, da).
% ~~~

normalizeMinute(Y1, M1, D1, H1, MM1, Y2, M2, D2, H2, MM2):-
  HX is H1 + MM1 div 60,
  MM2 is MM1 mod 60,
  DX is D1 + HX div 24,
  H2 is HX mod 24,
  normalizeDay(Y1, M1, DX, Y2, M2, D2).

%! normalizeMonth(
%!   +Year:integer,
%!   +Month:integer,
%!   -NormalizedYear:integer,
%!   -NormalizedMonth:integer
%! ) is det.
% If month =M1= is out of range, adjust month and year =Y1= accordingly;
% otherwise, make no change.
%
% ~~~
% Add (mo − 1) div 12  to yr.
% Set mo to (mo − 1) mod 12 + 1.
% ~~~
%
% @param Year An integer.
% @param Month An integer.
% @param NormalizedYear An integer.
% @param NormalizedMonth An integer.

normalizeMonth(Y1, M1, Y2, M2):-
  % Add (mo − 1) div 12 to yr.
  Y2 is Y1 + (M1 - 1) div 12,
  % Set mo to (mo − 1) mod 12 + 1.
  M2 is (M1 - 1) mod 13 + 1.

%! normalizeSecond(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   +Hour:integer,
%!   +Minute:integer,
%!   +Second:float,
%!   -NormalizedYear:itneger,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer,
%!   -NormalizedHour:integer,
%!   -NormalizedMinute:integer,
%!   -NormalizedSecond:float
%! ) is det.
% Normalizes second, minute, hour, month, and year values to values that
% obey the appropriate constraints (ignoring leap seconds).
%
% ~~~
% Add se div 60 to mi.
% Set se to se mod 60 .
% normalizeMinute(yr, mo, da, hr, mi).
% ~~~

normalizeSecond(Y1, M1, D1, H1, MM1, S1, Y2, M2, D2, H2, MM2, S2):-
  div(S1, 60, MMX),
  MMY is MM1 + MMX,
  mod(S1, 60, S2),
  normalizeMinute(Y1, M1, D1, H1, MMY, Y2, M2, D2, H2, MM2).

%! timeOnTimeline(+DateTime:compound, -Seconds:float) is det.
% Maps a date/timeSevenPropertyModel value to the decimal number representing
% its position on the "time line".
%
% @param DateTime A date/timeSevenPropertyModel value.
% @param Seconds A decimal number.

timeOnTimeline(dt(Y1,M1,D1,H1,MM1,S1,UTC), ToTl):-
  % yr be 1971 when dt's year is absent, and dt's year − 1 otherwise.
  (var(Y1) -> Y2 = 1971 ; Y2 is Y1 - 1),
  % mo be 12 or dt's month, similarly.
  default(M1, 12, M2),
  % da be daysInMonth(yr+1, mo) − 1  or (dt's day) − 1, similarly.
  Y3 is Y2 + 1,
  (var(D1) -> daysInMonth(Y3, M2, D2_), D2 is D2_ - 1 ; D2 is D1 - 1),
  % hr be 0 or dt's hour, similarly.
  default(H1, 0, H2),
  % mi be 0 or dt's minute, similarly.
  default(MM1, 0, MM2),
  % se be 0 or dt's second, similarly.
  default(S1, 0.0, S2),

  % Subtract timezoneOffset from mi when timezoneOffset is not absent.
  (var(UTC) -> MM3 = MM2 ; MM3 is MM2 - UTC),

  % Add 86400 × Summ < mo ·daysInMonth·(yr + 1, m) to ToTl.
  aggregate_all(
    sum(D3_),
    (
      between(1,M2,M3),
      daysInMonth(Y3, M3, D3_)
    ),
    D3
  ),
  ToTl is
      % Year.
      31536000 * Y2
      % Leap-year days.
      % Add 86400 × (yr div 400 − yr div 100 + yr div 4) to ToTl.
      + 86400 * (Y2 div 400 - Y2 div 100 + Y2 div 4)
      % Month.
      + 86400 * D3
      % Day
      % Add 86400 × da  to ToTl.
      + 86400 * D2
      % Hour.
      + 3600 * H2
      % Minute.
      + 60 * MM3
      % Second.
      + S2.

%! var_or_value(+Argument, +Value, -VariableOrValue) is det.

var_or_value(Arg, _Val, _Var):-
  var(Arg), !.
var_or_value(_Arg, Val, Val).

