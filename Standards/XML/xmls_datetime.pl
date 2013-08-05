:- module(
  xmls_datetime,
  [
    xmls_duration//3 % -Tree:compound
                     % ?Sign:between
                     % ?Duration:compound
  ]
).

/** <module> XMLS_DATETIME

This modules covers the deviations from ISO 8601 for the datatypes
defined in _|XML Schema 2 Datatypes|_.

The primitive datatypes =duration=, =dateTime=, =time=, =date=, =gYearMonth=,
=gMonthDay=, =gDay=, =gMonth= and =gYear= use lexical formats inspired by
ISO 8601. The lexical forms of these datatypes can include only the
characters =|#20|= (space) through =|#7F|= (delete).

The datatypes described in this specification do not cover all the types of
data covered by ISO 8601, nor do they support all the lexical representations
for those types of data.

Components:
  * =|C|= Represents a digit used in the thousands and hundreds components,
    the "century" component, of the time element "year".
    Legal values are from 0 to 9.
  * =|Y|= Represents a digit used in the tens and units components of
    the time element "year". Legal values are from 0 to 9.
  * =|M|= Represents a digit used in the time element "month".
    The two digits in a MM format can have values from 1 to 12.
  * =|D|= Represents a digit used in the time element "day".
    The two digits in a DD format can have values from 1 to 28 if the month
    value equals 2, 1 to 29 if the month value equals 2 and the year is
    a leap year, 1 to 30 if the month value equals 4, 6, 9 or 11, and
    1 to 31 if the month value equals 1, 3, 5, 7, 8, 10 or 12.
  * =|h|= Represents a digit used in the time element "hour".
    The two digits in a hh format can have values from 0 to 24.
    If the value of the hour element is 24 then the values of the minutes
    element and the seconds element must be 00 and 00.
  * =|m|= Represents a digit used in the time element "minute".
    The two digits in a =mm= format can have values from 0 to 59.
  * =|s|= Represents a digit used in the time element "second".
    The two digits in a =ss= format can have values from 0 to 60.
    In the formats described in this specification the whole number of
    seconds may be followed by decimal seconds to an arbitrary level of
    precision. This is represented by =|ss.sss|=.
    A value of 60 or more is allowed only in the case of leap seconds.
    Strictly speaking, a value of 60 or more is not sensible unless the month
    and day could represent March 31, June 30, September 30, or December 31
    in UTC. Because the leap second is added or subtracted as the last second
    of the day in UTC time, the long (or short) minute could occur at other
    times in local time. In cases where the leap second is used with an
    inappropriate month and day it, and any fractional seconds, should be
    considered as added or subtracted from the following minute.

Leading zeros are required where indicated.

Designators:
  * =T=
    Time-zone designator, indicating the start of the representation of
    the time of day in =dateTime=.
  * =Z=
    Time-zone designator, expressing the time of day in UTC in =dateTime=,
    =time=, =date=, =gYearMonth=, =gMonthDay=, =gDay=, =gMonth=, and =gYear=.

Duration designators:
    P -- is used as the time duration designator, preceding a data element representing a given duration of time.
    Y -- follows the number of years in a time duration.
    M -- follows the number of months or minutes in a time duration.
    D -- follows the number of days in a time duration.
    H -- follows the number of hours in a time duration.
    S -- follows the number of seconds in a time duration.

The values of the Year, Month, Day, Hour and Minutes components are not
restricted but allow an arbitrary integer. Similarly, the value of the
Seconds component allows an arbitrary decimal. Thus, the lexical format for
duration and datatypes derived from it does not follow the alternative format
of ISO 8601.

# Truncated and reduced formats

Left truncated formats are, in general, not permitted for the datatypes
defined in XML Schema with three exceptions:
  1. =time= uses a truncated format for =dateTime= which represents an
     instant of time that recurs every day.
  2. =gMonthDay= and =gDay= use left-truncated formats for =date=.
  3. =gMonth= uses a right and left truncated format for =date=.

Right truncated formats are also, in general, not permitted for the datatypes
defined in XML Schema with the following exceptions:
  * Right-truncated representations of =dateTime= are used as lexical
    representations for =date=, =gMonth=, =gYear=.

# Deviations from ISO 8601

## Sign Allowed

An optional minus sign is allowed immediately preceding, without a space,
the lexical representations for =duration=, =dateTime=, =date=, =gYearMonth=,
=gYear.=

## No Year Zero

The year 0000 is an illegal year value.

## More Than 9999 Years

To accommodate year values greater than 9999, more than four digits are
allowed in the year representations of =dateTime=, =date=, =gYearMonth=, and
=gYear=. This follows ISO 8601:2000 Second Edition.

## Time zone permitted

The lexical representations for the datatypes =date=, =gYearMonth=,
=gMonthDay=, =gDay=, =gMonth= and =gYear= permit an optional trailing
time zone specificiation.

--

@author Wouter Beek
@see http://www.w3.org/TR/xmlschema-2/#isoformats
@version 2013/08
*/

:- use_module(datetime(iso8601_time_interval)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(plunit)).



%! duration_smaller_than(+Duration1:compound, +Duration2:compound) is semidet.
% Definition: =|x < y|= iff =|s+x < s+y|= for each qualified =dateTime= =s=
% in the list below:
% ~~~
% 1696-09-01T00:00:00Z
% 1697-02-01T00:00:00Z
% 1903-03-01T00:00:00Z
% 1903-07-01T00:00:00Z
% ~~~

duration_smaller_than(D1, D2):-
  summate_time('1696-09-01T00:00:00Z', D1, DT11),
  summate_time('1696-09-01T00:00:00Z', D2, DT12),
  dateTime_smaller_than(DT11, DT12),
  summate_time('1697-02-01T00:00:00Z', D1, DT21),
  summate_time('1697-02-01T00:00:00Z', D2, DT22),
  dateTime_smaller_than(DT21, DT22),
  summate_time('1903-03-01T00:00:00Z', D1, DT31),
  summate_time('1903-03-01T00:00:00Z', D2, DT32),
  dateTime_smaller_than(DT31, DT32),
  summate_time('1903-07-01T00:00:00Z', D1, DT41),
  summate_time('1903-07-01T00:00:00Z', D2, DT42),
  dateTime_smaller_than(DT41, DT42).

%! 'fQuotient'(+A:float, +B:nonneg, -Quotient:integer) is det.
% Definition: The greatest integer less than or equal to =A= divided by =B=.
%
% Examples:
% ~~~
% fQuotient(-1,3) = -1
% fQuotient(0,3)...fQuotient(2,3) = 0
% fQuotient(3,3) = 1
% fQuotient(3.123,3) = 1
% ~~~

'fQuotient'(A, B, X):-
  B =\= 0.0,
  X is floor(A / B).

%! 'fQuotient'(
%!   +A:float,
%!   +Low:integer,
%!   +High:integer,
%!   -Quotient:integer
%! ) is det.
% Definition:
% ~~~
% fQuotient(a, low, high) = fQuotient(a - low, high - low)
% ~~~
%
% Examples:
% ~~~
% fQuotient(0, 1, 13) = -1
% fQuotient(1, 1, 13) ... fQuotient(12, 1, 13) = 0
% fQuotient(13, 1, 13) = 1
% fQuotient(13.123, 1, 13) = 1
% ~~~

'fQuotient'(A, L, H, Q):-
  H =\= L,
  X is A - L,
  Y is H - L,
  'fQuotient'(X, Y, Q).

%! modulo(+A:float, +B:nonneg, -Modulo:float) is det.\
% Definition:
% ~~~
% modulo(a, b) = a - fQuotient(a,b)*b
% ~~~
%
% Examples:
% ~~~
% modulo(-1,3) = 2
% modulo(0,3)...modulo(2,3) = 0...2
% modulo(3,3) = 0
% modulo(3.123,3) = 0.123
% ~~~

modulo(A, B, M):-
  B =\= 0.0,
  'fQuotient'(A, B, Q),
  M is A - Q * B.

%! modulo(+A:float, +Low:integer, +High:integer, -Modulo:float) is det.
% Definition
% ~~~
% modulo(a, low, high) = modulo(a - low, high - low) + low
% ~~~
%
% Examples:
% ~~~
% modulo(0, 1, 13) = 12
% modulo(1, 1, 13) ... modulo(12, 1, 13) = 1...12
% modulo(13, 1, 13) = 1
% modulo(13.123, 1, 13) = 1.123
% ~~~

modulo(A, L, H, M):-
  H =\= L,
  X is A - L,
  Y is H - L,
  modulo(X, Y, M_),
  M is M_ + L.

sign(_NoTree, true) -->
  [].
sign(sign('-'), false) -->
  minus.

%! summate_time(+DateTime:compound, +Duration:compound, -Sum:compound) is det.
% =S= stands for the =dateTime= value; =D= stands for the =duration= value.
%
% Essentially, this calculation is equivalent to separating =D= into
% =|<year,month>|= and =|<day,hour,minute,second>|= fields.
% The =|<year,month>|= is added to =S=. If the day is out of range,
% it is pinned to be within range. Thus April 31 turns into April 30.
% Then the =|<day,hour,minute,second>|= is added. This latter addition can
% cause the year and month to change.
%
% Leap seconds are handled by the computation by treating them as overflows.
% Essentially, a value of 60 seconds in =S= is treated as if it were a
% duration of 60 seconds added to =S= (with a zero seconds field).
% All calculations thereafter use 60 seconds per minute.
%
% Thus the addition of either =PT1M= or =PT60S= to any =dateTime= will always
% produce the same result. This is a special definition of addition which is
% designed to match common practice, and -- most importantly -- be stable
% over time.
%
% A definition that attempted to take leap-seconds into account would need to
% be constantly updated, and could not predict the results of future
% implementation's additions. The decision to introduce a leap second in UTC
% is the responsibility of the International Earth Rotation Service (IERS).
% They make periodic announcements as to when leap seconds are to be added,
% but this is not known more than a year in advance. For more information on
% leap seconds, see U.S. Naval Observatory Time Service Department.
%
% Algorithm:
% ~~~
% Months (may be modified additionally below)
%   temp := S[month] + D[month]
%   E[month] := modulo(temp, 1, 13)
%   carry := fQuotient(temp, 1, 13)
% Years (may be modified additionally below)
%   E[year] := S[year] + D[year] + carry
% Zone
%   E[zone] := S[zone]
% Seconds
%   temp := S[second] + D[second]
%   E[second] := modulo(temp, 60)
%   carry := fQuotient(temp, 60)
% Minutes
%   temp := S[minute] + D[minute] + carry
%   E[minute] := modulo(temp, 60)
%   carry := fQuotient(temp, 60)
% Hours
%   temp := S[hour] + D[hour] + carry
%   E[hour] := modulo(temp, 24)
%   carry := fQuotient(temp, 24)
% Days
%   if S[day] > maximumDayInMonthFor(E[year], E[month])
%     tempDays := maximumDayInMonthFor(E[year], E[month])
%   else if S[day] < 1
%     tempDays := 1
%   else
%     tempDays := S[day]
%   E[day] := tempDays + D[day] + carry
%   START LOOP
%     IF E[day] < 1
%       E[day] := E[day] + maximumDayInMonthFor(E[year], E[month] - 1)
%       carry := -1
%     ELSE IF E[day] > maximumDayInMonthFor(E[year], E[month])
%       E[day] := E[day] - maximumDayInMonthFor(E[year], E[month])
%       carry := 1
%     ELSE EXIT LOOP
%     temp := E[month] + carry
%     E[month] := modulo(temp, 1, 13)
%     E[year] := E[year] + fQuotient(temp, 1, 13)
%     GOTO START LOOP
% ~~~
%
% Examples:
% ~~~
% =|2000-01-12T12:13:14Z + P1Y3M5DT7H10M3.3S = 2001-04-17T19:23:17.3Z|=
% =|2000-01 + -P3M = 1999-10|=
% =|2000-01-12 + PT33H = 2000-01-13|=
% ~~~
%
% # Application order
%
% The order in which durations are added to a date-time is significant,
% since adding a month adds a variable number of days.
%
% Example:
% ~~~
% (2000-03-30 + P1D) + P1M = 2000-03-31 + P1M = 2000-04-30
% (2000-03-30 + P1M) + P1D = 2000-04-30 + P1D = 2000-05-01
% ~~~

summate_time(
  date_time(date(Y1,M1,D1),utc_time(time(H1,MM1,S1),UTC_Correction)),
  Sign-duration(Y2,M2,D2,H2,MM2,S2),
  date_time(date(Y3,M3,D3),utc_time(time(H3,MM3,S3),UTC_Correction))
):-
  % Month
  MX is M1 + (Sign * M2),
  modulo(MX, 1, 13, MY),
  'fQuotient'(MX, 1, 13, Y_Carry),

  % Year
  YX is Y1 + (Sign * Y2) + Y_Carry,

  % Second
  SX is S1 + (Sign * S2),
  modulo(SX, 60, S3),
  'fQuotient'(SX, 60, MM_Carry),

  % Minute
  MMX is MM1 + (Sign * MM2) + MM_Carry,
  modulo(MMX, 60, MM3),
  'fQuotient'(MMX, 60, H_Carry),

  % Hour
  HX is H1 + (Sign * H2) + H_Carry,
  modulo(HX, 60, H3),
  'fQuotient'(HX, 60, D_Carry),

  % Day
  calendar_month(YX, MY, MaxD, _),
  (D1 < 1 -> DX = MaxD ;
   D1 < 1 -> DX = 1 ;
   DX = D1),
  DY is DX + (Sign * D2) + D_Carry,
  summate_time(YX, MY, DY, Y3, M3, D3).

summate_time(Y1, M1, D1, Y2, M2, D2):-
  D1 < 1, !,
  M1_ is M1 - 1,
  calendar_month(Y1, M1_, MaxD_, _),
  DX is D1 + MaxD_,
  MX is M1 - 1,
  summate_time_(Y1, MX, DX, Y2, M2, D2).
summate_time(Y1, M1, D1, Y2, M2, D2):-
  calendar_month(YX, MY, MaxD, _),
  D1 > MaxD, !,
  DX is D1 - MaxD,
  MX is M1 + 1,
  summate_time_(Y1, MX, DX, Y2, M2, D2).
summate_time(Y, M, D, Y, M, D).

summate_time_(Y1, M1, D1, Y3, M3, D3):-
  modulo(M1, 1, 13, M2),
  'fQuotient'(M1, 1, 13, YX),
  Y2 is Y1 + YX,
  summate_time(Y2, M2, D2, Y3, M3, D3).

%! xmls_duration(External, Canonical:atom)
% Duration represents a duration of time. The value space of duration is a
% six-dimensional space where the coordinates designate the Gregorian year,
% month, day, hour, minute, and second components.
% These components are ordered in their significance by their order of
% appearance i.e. as year, month, day, hour, minute, and second.
%
% ### Year and second rate
%
% All minimally conforming processors must support year values with
% a minimum of 4 digits (i.e., =YYYY=) and a minimum fractional
% second precision of milliseconds or three decimal digits (i.e. =|s.sss|=).
% However, minimally conforming processors may set an application-defined
% limit on the maximum number of digits they are prepared to support in these
% two cases, in which case that application-defined maximum number must
% be clearly documented.
%
% ### ISO 8601 based format
%
% The lexical representation for duration is the ISO 8601 extended format
% =|PnYnMnDTnHnMnS|=, where =nY= represents the number of years,
% =nM= the number of months, =nD= the number of days, =T= is the date/time
% separator, =nH= the number of hours, =nM= the number of minutes and
% =nS= the number of seconds. The number of seconds can include decimal
% digits to arbitrary precision.
%
% ### Component values
%
% The values of the Year, Month, Day, Hour and Minutes components are not
% restricted but allow an arbitrary unsigned integer, i.e., an integer that
% conforms to the pattern =|[0-9]+|=. Similarly, the value of the Seconds
% component allows an arbitrary unsigned decimal. Following ISO 8601,
% at least one digit must follow the decimal point if it appears.
% That is, the value of the Seconds component must conform to the pattern
% =|[0-9]+(\.[0-9]+)?|=. Thus, the lexical representation of duration
% does not follow the alternative format of ISO 8601.
%
% ### Sign
%
% An optional preceding minus sign is allowed, to indicate a negative
% duration. If the sign is omitted a positive duration is indicated.
%
% ### Example
%
% For example, to indicate a duration of 1 year, 2 months, 3 days, 10 hours,
% and 30 minutes, one would write: =P1Y2M3DT10H30M=. One could also indicate
% a duration of minus 120 days as: =|-P120D|=.
%
% ### Reduced precision & truncated representation
%
% Reduced precision and truncated representations of this format are
% allowed provided they conform to the following:
%   * If the number of years, months, days, hours, minutes, or seconds
%     in any expression equals zero, the number and its corresponding
%     designator may be omitted. However, at least one number and its
%     designator must be present.
%   * The seconds part may have a decimal fraction.
%   * The designator =T= must be absent if and only if all of the time
%     items are absent. The designator =P= must always be present.
%
% For example, =P1347Y=, =P1347M= and =P1Y2MT2H= are all allowed;
% =P0Y1347M= and =P0Y1347M0D= are allowed.
% =|P-1347M|= is not allowed although =|-P1347M|= is allowed.
% =P1Y2MT= is not allowed.
%
% # Order relation on duration
%
% The order-relation on duration is a PO since there is no determinate
% relationship between certain durations such as one month (=P1M=) and
% 30 days (=P30D=). The order-relation of two duration values =x= and =y=
% is =|x < y|= iff =|s+x < s+y|= for each qualified =dateTime= =s= in the
% list below. These values for =s= cause the greatest deviations in the
% addition of =dateTimes= and durations.
% Addition of durations to time instants is defined by summate_time/3.
%
% ~~~
% 1696-09-01T00:00:00Z
% 1697-02-01T00:00:00Z
% 1903-03-01T00:00:00Z
% 1903-07-01T00:00:00Z
% ~~~
%
% The following table shows the strongest relationship that can be
% determined between example durations. The symbol =|<>|= means that the
% order relation is indeterminate. Note that because of leap-seconds,
% a seconds field can vary from 59 to 60. However, because of the way that
% addition is defined, they are still totally ordered.
%
% | *|=|P1Y|=|* | =|> P364D|= | =|<> P365D|= | =|<> P366D|= | =|< P367D|=  |              |             |
% | *|=|P1M|=|* | =|> P27D|=  | =|<> P28D|=  | =|<> P29D|=  | =|<> P30D|=  | =|<> P31D|=  | =|< P32D|=  |
% | *|=|P5M|=|* | =|> P149D|= | =|<> P150D|= | =|<> P151D|= | =|<> P152D|= | =|<> P153D|= | =|< P154D|= |
%
% ### Differences with ISO 8601:
%
%  1. The time designator is never allowed to be abscent when there is a
%     non-empty time segment.
%  2. The number of weeks does not occur.
%  3. A sign is added (in front of the duration designator).

%! xmls_duration(-Tree:compound, ?Sign:between, ?Duration:compound)//

xmls_duration(T0, Sign, Duration) -->
  sign(T1, Sign),
  iso8601_duration(T2, Duration),
  {parse_tree(xmls_duration, [T1,T2], T0)}.



% UNIT TESTS %

:- begin_tests(xmls_datetime).

summate_time_example(
  '2000-01-12T12:13:14Z',
  'P1Y3M5DT7H10M3.3S',
  '2001-04-17T19:23:17.3Z'
).
summate_time_example('2000-01', '-P3M', '1999-10').
summate_time_example('2000-01-12', 'PT33H', '2000-01-13').

test(
  summate_time,
  [forall(summate_time_example(DT1_A, D_A, DT2_A)), true(DT2_A == DT3_A)]
):-
  atom_codes(DT1_A, DT1_Cs),
  once(phrase(xmls_dateTime(_T1, F, DT1), DT1_Cs)),
  atom_codes(D_A, D_Cs),
  once(phrase(xmls_duration(_T2, Sign, D), D_Cs),
  summate_time(DT1, Sign-D, DT3),
  once(phrase(xmls_dateTime(_T3, F, DT3), DT3_Cs)),
  atom_codes(DT3_A, DT3_Cs).

xmls_duration_positive_example('P1347Y',     true,  duration(1247,_,   _,_,_,_)).
xmls_duration_positive_example('P1347M',     true,  duration(_,   1247,_,_,_,_)).
xmls_duration_positive_example('P1Y2MT2H',   true,  duration(1,   2,   _,2,_,_)).
xmls_duration_positive_example('P0Y1347M',   true,  duration(0,   1347,_,_,_,_)).
xmls_duration_positive_example('P0Y1347M0D', true,  duration(0,   1347,0,_,_,_)).
xmls_duration_positive_example('-P1347M',    false, duration(_,   1347,_,_,_,_)).

xmls_duration_negative_example('P-1347M',    false, duration(_,   1347,_,_,_,_)).
xmls_duration_negative_example('P1Y2MT',     true,  duration(1,   2,   _,_,_,_)).

test(
  xmls_duration_generate,
  [forall(xmls_duration_positive_example(A1, S, D)), true(A1 == A2)]
):-
  once(phrase(xmls_duration(_T, S, D), Cs)),
  atom_codes(A2, Cs).

test(
  xmls_duration_generate,
  [fail, forall(xmls_duration_positive_example(A, S, D))]
):-
  phrase(xmls_duration(_T, S, D), Cs),
  atom_codes(A, Cs).

test(
  xmls_duration_parse,
  [
    forall(xmls_duration_positive_example(A, S1, D1)),
    true(maplist(=, [S1,D1], [S2,D2]))
  ]
):-
  atom_codes(A, Cs),
  once(phrase(xmls_duration(_T, S2, D2), Cs)).

test(
  xmls_duration_parse,
  [fail, forall(xmls_duration_positive_example(A, _S1, _D1))]
):-
  atom_codes(A, Cs),
  phrase(xmls_duration(_T, _S2, _D2), Cs).

:- end_tests(xmls_datetime).

