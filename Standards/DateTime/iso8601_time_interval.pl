:- module(
  iso8601_time_interval,
  [
    iso8601_time_interval//3, % -Tree:compound,
                               % ?Format:oneof([basic,extended]),
                               % ?DateTime:compound
    iso8601_time_interval//4 % -Tree:compound,
                             % ?Format:oneof([basic,extended])
                             % ?DateTime1:compound
                             % ?DateTime2:compound
  ]
).

/** <module> ISO8601_TIME_INTERVAL

# Custom datatypes

Date-time combinations:
~~~
date_time(
  ?Date:compound,
  ?UTC_Time:compound
)
~~~

--

# Time interval

A time interval is expressed in one of the following ways:
  1. by a start and an end;
  2. by a duration and context information;
  3. by a start and a duration;
  4. by a duration and an end.

In the second casethe time interval is not fully determined by the information
provided by the expression.
It is assumed that, where needed, additional information to completely
determine the time interval is available from the context.

Separators and designators
  * A solidus =|[/]|= is used to separate the two components.
    Variants 1, 3, 4.
    In certain application areas a double hyphen is used as a separator
    instead of a solidus.
  * =|[P]|= directly precedes the remainder of the expression of duration.
    Variants 2, 3, 4.

*Duration* can be expressed by a combination of components with
_|accurate duration|_ (hour, minute and second) and components with
_|nominal duration|_ (year, month, week and day).

A duration can also be designated by an expression containing components with
both accurate and nominal duration.

See ISO 31-1 for accurate durations using the time unit day.

Duration is used as a component in representations of time intervals and
recurring time intervals, representation of duration as such is not
facilitated.

## Format with designators

In expressions of time interval or recurring time interval, duration can be
represented by a combination of components with designators:
  * The number of years is followed by the designator =|[Y]|=.
  * The number of months is followed by the designator =|[M]|=.
  * The number of weeks is followed by the designator =|[W]|=.
  * The number of days is followed by the designator =|[D]|=.
  * The part including time components is preceded by the designator =|[T]|=.
  * The number of hours is followed by =|[H]|=
  * The number of minutes is followed by the designator =|[M]|=.
  * The number of seconds is followed by the designator =|[S]|=.

For the day component on can express a arbitrary multiple of the duration
of a calendar day, i.e. =|[n_nD]|=.

In both basic and extended format the complete representation of
the expression for duration is =|[PnnW]|= or =|[PnnYnnMnnDTnnHnnMnnS]|=.

In these representations the maximum number of digits in a component needs
to be agreed by the partners in information interchange.

For reduced accuracy or decimal representations of this representation,
the following rules apply:
  * If necessary for a particular application, the lowest order components
    may be omitted to represent duration with reduced accuracy.
  * If necessary for a particular application, the lowest order components
    may have a decimal fraction.
    The decimal fraction shall be divided from the integer part by
    the decimal sign (see ISO 31-0).
    The decimal fraction shall at least have one digit, the maximum number
    of digits in the decimal component needs to be agreed by the
    partners in information interchange.
    If the magnitude of the number is less than unity, the decimal sign
    shall be preceded by a zero (see ISO 31-0).
  * If the number of years, months, days, hours, minutes or seconds
    in any of these expressions equals zero, the number and
    the corresponding designator may be absent;
    however, at least one number and its designator shall be present.
  * The designator =|[T]|= is absent if all of the time components are absent.

## Alternative format

By mutual agreement of the partners in information interchange,
duration may be expressed in conformity with the following format for time
points: (1) calendar date, (2) ordinal date,
(3) possibly including the time designator =|[T]|=,
and (4) combinations of dates (cardinal dates and ordinal dates) and
times (restricted to local time's complete representations, representations
with reduced accuracy, and representations with decimal fraction).

The values expressed must not exceed the "carry over points" of 12 months,
30 days, 24 hours, 60 minutes and 60 seconds.
Since weeks have no defined carry over point (52 or 53), weeks should not
be used in these applications.

In these expressions a possible value of the year time element is =|[0000]|=,
of the calendar month and calendar day of the month time elements =|[00]|= and
of the calendar day of the year time element =|[000]|=.

The complete representation of the expression for duration in
the alternative format is as follows:
  * Basic format: =|PYYYYMMDDThhmmss|= or =|PYYYYDDDThhmmss|=
  * Extended format: =|PYYYY-MM-DDThh:mm:ss|= or =|PYYYY-DDDThh:mm:ss|=

## Complete representation

### Intervals indentified by start and end

When the application identifies the need for a complete representation of
a time interval, identified by its start and its end, it shall use an
expression in accordance with the format designators (see above),
combining any two complete date and time-of-day representations,
provided that the resulting expression is either consistently
in basic format or consistently in extended format.

The following examples represent a time interval beginning at 20 minutes
and 50 seconds past 23 hours on 12 April 1985 local time and
ending at 30 minutes past 10 hours on 25 June 1985 local time.

Basic format:
~~~
YYYYMMDDThhmmss/YYYYMMDDThhmmss
~~~
Example: =|19850412T232050/19850625T103000|=

Extended format:
~~~
YYYY-MM-DDThh:mm:ss/YYYY-MM-DDThh:mm:ss
~~~
Example: =|1985-04-12T23:20:50/1985-06-25T10:30:00|=

### Intervals indentified by duration and context information

#### Format with designators

When an application identifies the need for a complete representation of
a time interval through its duration and context information, with duration
in the format with designators, it shall use an expression in accordance
with the separators and designators for intervals (see above)
using a complete duration representation as defined by the format with
designators (see above).

Basic and extended format:
~~~
PnnYnnMnnDTnnHnnMnnS
PnnW
~~~

Example 1: =|P2Y10M15DT10H30M20S|=
Example 2: =|P6W|=

Example 1 represents a time interval with a duration of 2 years, 10 months,
15 days, 10 hours, 30 minutes and 20 seconds.
Example 2 represents a time interval with a duration of six weeks.

#### Alternative format

If, by agreement, a complete representation of a time interval through its duration and context information,
with duration in the alternative format, is used, the expression shall be in accordance with 4.4.2 and use a
complete duration representation as defined in 4.4.3.3.
Basic format: PYYYYMMDDThhmmss Example: P00021015T103020
Extended format: PYYYY-MM-DDThh:mm:ss Example: P0002-10-15T10:30:20
The examples represent a time interval with a duration of 2 years, 10 months, 15 days, 10 hours, 30 minutes
and 20 seconds.

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(datetime(iso8601_date_time)).
:- use_module(datetime(iso8601_generic)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).



% Start and end of interval.
% Any two date and time-of-day formats.
iso8601_time_interval(T0, Format, DateTime1, DateTime2) -->
  (
    iso8601_calendar_date_time(T1, Format, DateTime1)
  ;
    iso8601_ordinal_date_time(T1, Format, DateTime1)
  ),
  iso8601_interval_separator(T2),
  (
    iso8601_calendar_date_time(T3, Format, DateTime2)
  ;
    iso8601_ordinal_date_time(T3, Format, DateTime2)
  ),
  {parse_tree(time_interval, [T1,T2,T3], T0)}.

%! iso8601_time_interval(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:integer,
%!   ?Month:integer,
%!   ?Week:integer,
%!   ?Day:integer,
%!   ?TimeSeparator:boolean,
%!   ?Hour:integer,
%!   ?Minute:integer,
%!   ?Second:integer
%! )//
% Duration and context information.
% Any two date and time-of-day formats.

iso8601_time_interval(T0, _Format, date_time(date(Y,M,W,D),UTC_Time)) -->
  {var(M), var(W), UTC_Time = utc_time(time(H,MM,S),_UTC)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(D)} ; iso8601_number_of_days(T3, D)),
  {iso8601_time_designator(UTC_Time, T)},
  (
    {T = true}, iso8601_generic:iso8601_time_designator(T4)
  ;
    {T = false, var(H), var(MM), var(S), var(T4)}
  ),
  ({var(H)} ; iso8601_number_of_hours(T5, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T6, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T7, S)),
  {parse_tree(time_interval, [T1,T2,T3,T4,T5,T6,T7], T0)}.
iso8601_time_interval(T0, _Format, date_time(date(Y,M,W,D),UTC_Time)) -->
  {var(W), UTC_Time = utc_time(time(H,MM,S),_UTC)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(M)} ; iso8601_number_of_months(T3, M)),
  ({var(D)} ; iso8601_number_of_days(T4, D)),
  {iso8601_time_designator(UTC_Time, T)},
  (
    {T = true}, iso8601_generic:iso8601_time_designator(T5)
  ;
    {T = false, var(H), var(MM), var(S), var(T5)}
  ),
  ({var(H)} ; iso8601_number_of_hours(T6, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T7, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T8, S)),
  {parse_tree(time_interval, [T1,T2,T3,T4,T5,T6,T7,T8], T0)}.
iso8601_time_interval(T0, _Format, date_time(date(Y,M,W,D),UTC_Time)) -->
  {var(M), UTC_Time = utc_time(time(H,MM,S),_UTC)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(W)} ; iso8601_number_of_weeks(T3, W)),
  ({var(D)} ; iso8601_number_of_days(T4, D)),
  {iso8601_time_designator(UTC_Time, T)},
  (
    {T = true}, iso8601_generic:iso8601_time_designator(T5)
  ;
    {T = false, var(H), var(MM), var(S), var(T5)}
  ),
  ({var(H)} ; iso8601_number_of_hours(T6, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T7, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T8, S)),
  {parse_tree(time_interval, [T1,T2,T3,T4,T5,T6,T7,T8], T0)}.



% SUPPORT PREDICATES %

iso8601_day_designator('D') -->
  "D".

iso8601_duration_designator(duration_designator('P')) -->
  "P".

iso8601_hour_designator('H') -->
  "H".

iso8601_interval_separator(interval_separator('/')) -->
  "/".

iso8601_minute_designator('M') -->
  "M".

iso8601_month_designator('M') -->
  "M".

iso8601_number_of_days(days(D,X), D) -->
  decimal_number(D),
  iso8601_day_designator(X).

iso8601_number_of_hours(hours(H,X), H) -->
  decimal_number(H),
  iso8601_hour_designator(X).

iso8601_number_of_minutes(minutes(M,X), M) -->
  decimal_number(M),
  iso8601_minute_designator(X).

iso8601_number_of_months(months(M,X), M) -->
  decimal_number(M),
  iso8601_month_designator(X).

iso8601_number_of_seconds(seconds(S,X), S) -->
  decimal_number(S),
  iso8601_second_designator(X).

iso8601_number_of_weeks(weeks(W,X), W) -->
  decimal_number(W),
  iso8601_week_designator(X).

iso8601_number_of_years(years(Y,X), Y) -->
  decimal_number(Y),
  iso8601_year_designator(X).

iso8601_second_designator('S') -->
  "S".

iso8601_year_designator('Y') -->
  "Y".

