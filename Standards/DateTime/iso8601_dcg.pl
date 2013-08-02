:- module(
  iso8601_dcg,
  [
% CALENDAR DATE
    iso8601_calendar_date//5, % ?Tree:compound
                              % ?Format:oneof([basic,extended])
                              % ?Year:between(0,9999)
                              % ?Month:between(1,12)
                              % ?Day:between(28,31)
    iso8601_calendar_date_time//13, % -Tree:compound
                                    % ?Format:oneof([basic,extended])
                                    % ?Year:between(0,9999)
                                    % ?Month:between(1,12)
                                    % ?Day:between(28,31)
                                    % ?TimeDesignator:boolean
                                    % ?Hour:float
                                    % ?Minute:float
                                    % ?Second:float
                                    % ?UTC:boolean
                                    % ?Sign:boolean
                                    % ?HourCorrection:float
                                    % ?MinuteCorrection:float
% LOCAL DATE
    iso8601_local_time//10, % -Tree:compound
                            % ?Format:oneof([basic,extended])
                            % ?TimeDesignator:boolean
                            % ?Hour:float
                            % ?Minute:float
                            % ?Second:float
                            % ?UTC:boolean
                            % ?Sign:boolean
                            % ?HourCorrection:float
                            % ?MinuteCorrection:float
% ORDINAL DATE
    iso8601_ordinal_date//4, % -Tree:compound
                             % ?Format:oneof([basic,extended])
                             % ?Year:between(0,9999)
                             % ?Day:between(1,366)
    iso8601_ordinal_date_time//12, % -Tree:compound
                                   % ?Format:oneof([basic,extended])
                                   % ?Year:between(0,9999)
                                   % ?Day:between(1,366)
                                   % ?TimeDesignator:boolean
                                   % ?Hour:float
                                   % ?Minute:float
                                   % ?Second:float
                                   % ?UTC:boolean
                                   % ?Sign:boolean
                                   % ?HourCorrection:float
                                   % ?MinuteCorrection:float
% WEEK DATE
    iso8601_week_date//5, % -Tree:compound
                          % ?Format:oneof([basic,extended])
                          % ?Year:between(0,9999)
                          % ?Week:between(1,53)
                          % ?Day:between(1,7)
    iso8601_week_date_time//13 % -Tree:compound
                               % ?Format:oneof([basic,extended])
                               % ?Year:between(0,9999)
                               % ?Week:between(1,53)
                               % ?Day:between(1,7)
                               % ?TimeDesignator:boolean
                               % ?Hour:float
                               % ?Minute:float
                               % ?Second:float
                               % ?UTC:boolean
                               % ?Sign:boolean
                               % ?HourCorrection:float
                               % ?MinuteCorrection:float
  ]
).

/** <module> ISO8601_DCG

# Concepts

## Date and time representation

Expression indicating a time point, time interval or recurring time interval.

## Date and time format representation

Expression describing the format of a group of date and time representations.

## Basic format

Format of a date and time representation or date and time format
representation comprising the minimum number of time elements necessary for
the accuracy required.

The basic format should be avoided in plain text.

## Extended format

Extension of the basic format that includes additional separators.

## Complete representation

Representation that includes all the date and time components associated
with the expression; limited, if applicable, for time elements of
representations expressing a calendar year to four digits.

## Decimal representation

Expansion of a representation by addition of a decimal fraction to the lowest
order component of the expression.

## Representation with reduced accuracy

Abbreviation of a representation by omission of lower order components.

## Expanded representation

Expansion of a representation to allow identification of dates in calendar
years outside the range =|[0000]|= till =|[9999]|=.

Permitted only by mutual agreement of the partners in information interchange.

## Case

Lower case characters may be used when upper case characters are not
available.

## Underlining

The date and time format representations use characters that potentially
expand into more than one character in the date and time representation;
this is indicated by underlining.

If at the time of information interchange of the date and time format
representation the number of characters to be used in the date and time
representation is known, the variable expansion representation
(i.e. underlining) shall not be used.

In environments that do not support the representation of underlined
characters, the underline shall precede the character to be underlined.

## Characters representing date and time characters

In date and time format representations characters are used to represent
characters in the date and time representations as follows:
  * =|[Y]|= represents a digit used in the time element "year";
  * =|[M]|= represents a digit used in the time element "month";
  * =|[D]|= represents a digit used in the time element "day";
  * =|[w]|= represents a digit used in the time element "week";
  * =|[h]|= represents a digit used in the time element "hour";
  * =|[m]|= represents a digit used in the time element "minute";
  * =|[s]|= represents a digit used in the time element "second";
  * =|[n]|= represents a digit from a positive integer or zero;
  * =|[±]|= represents a plus sign =|[+]|= if in combination with the
    following element a positive value or zero needs to be represented
    (in this case, unless explicitly stated otherwise, the plus sign shall
    not be omitted), or a minus sign =|[−]|= if in combination with the
    following element a negative value needs to be represented.

In addition the following convention applies:
  * =|[_]|+ When any of the characters representing a digit is underlined,
    it represents zero or more digits in the corresponding date and time
    representation.

Other characters in the date and time format representations are copied in
the date and time representations.

## Characters used as designators

In representations the following characters are used as designators:
  * =|[P]|= is used as duration designator, preceding the component which
    represents the duration;
    (Based on the historical use of the term "period" for duration.)
  * =|[R]|= is used as recurring time interval designator;
  * =|[T]|= is used as time designator to indicate:
    * The start of the representation of local time to designate
      local time expressions as such,
    * The start of the representation of the time of day in date and time
      of day expressions,
    * The start of the representation of the number of hours, minutes or
      seconds in expressions of duration;
  * =|[W]|= is used as week designator, preceding a data element which
    represents the ordinal number of a calendar week within the calendar year;
  * =|[Z]|= is used as UTC designator.
  * =|[M]|= may (but need not) be used to indicate "month" or "minute".

Note that the meaning of =|M|= is context-dependent, based on its position in
the expression.

## Characters used as separators

In representations the following characters are used as separators:
  * =|[-]|= (hyphen): separates "year" from "month", "year" from “week”,
    "year" from "day", "month" from "day", and "week" and "day";
  * =|[:]|= (colon): separates "hour" from "minute",
    and "minute" from "second";
  * =|[/]|= (solidus): separates components in the representation of time
    intervals and recurring time intervals.

## Leading zeros

If a time element in a defined representation has a defined length,
then leading zeros shall be used as required.

--

# Date representations

## Calendar date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|0000|= to
=|9999|=.
Values in the range =|0000|= through =|1582|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar month|* is represented by two digits.

*|Calendar day of the month|* is represented by two digits.

### Complete representations

Basic format:
~~~
YYYYMMDD
~~~
Example: =|19850412|=

Extended format:
~~~
YYYY-MM-DD
~~~
Example: =|1985-04-12|=

### Reduced representations

Reduced representations cannot be given in the extended format.

When only the day is omitted, a separator shall be inserted between the year
and the month, but separators shall not be used in the other representations
with reduced accuracy.

Month:
~~~
YYYY-MM
~~~
Example: =|1985-04|=

Year:
~~~
YYYY
~~~
Example: =|1985|=

Century:
~~~
YY
~~~
Example: =|19|=

### Expanded representations

Day (basic):
~~~
±_YYYYYYMMDD
~~~

Day (extended):
~~~
±_YYYYYY-MM-DD
~~~

Month:
~~~
±_YYYYYY-MM
~~~

Year:
~~~
±_YYYYYY
~~~

Century:
~~~
±_YYY
~~~

## Ordinal date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|0000|= to
=|9999|=.

Values in the range =|0000|= through =|1582|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar day of the year|* is represented by three decimal digits.
The calendar days are represented by =|001|= through =|365|= (leap year)
or =|366|= (common year), depending on the year.

### Complete representations

Basic format:
~~~
YYYYDDD
~~~
Example: =|1985102|=

Extended format:
~~~
YYYY-DDD
~~~
Example: =|1985-102|=

### Expanded representations

Basic format:
~~~
±YYYYYDDD
~~~
Example: =|+001985102|=

Extended format:
~~~
±YYYYY-DDD
~~~
Example: =|+001985-102|=

## Week date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|[0000]|= to
=|[9999]|=.

Values in the range =|[0000]|= through =|[1582]|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar week|* is represented by two decimal digits and ranges from
=|00|= to =|52|= or =|53|=, depending on the year.

*|Calendar day of the week|* is represented by one decimal digit,
see iso8601:calendar_day_name/2.

### Complete representations

Basic format:
~~~
YYYYWwwD
~~~
Example: =|1985W155|=

Extended format:
~~~
YYYY-Www-D
~~~
Example: =|1985-W15-5|=

### Representations with reduced accuracy

Basic format:
~~~
YYYYWww
~~~
Example: =|1985W15|=

Extended format:
~~~
YYYY-Www
~~~
Example: =|1985-W15|=

### Expanded representations

Basic format:
~~~
±YYYYYWwwD
~~~
Example: =|+001985W155|=

Extended format:
~~~
±YYYYY-Www-D
~~~
Example: =|+001985-W15-5|=

Basic format:
~~~
±YYYYYWww
~~~
Example: =|+001985W15|=

Extended format:
~~~
±YYYYY-Www
~~~
Example: =|+001985-W15|=

--

# Time of day representations

*Hour* is represented by two digits from =00= to =24=.
The representation of the hour by =24= is only allowed to indicate the end of
a calendar day.

*Minute* is represented by two digits from =00= to =59=.

*Second* is represented by two digits from =00= to =60=.

The representation of the second by =60= is only allowed to indicate
a positive leap second or an instant within that second.

These expressions apply to both UTC and non-UTC based time scales
for time of day.

## Local time

### Complete representations

Basic format:
~~~
hhmmss
~~~
Example: =|232050|=

Extended format:
~~~
hh:mm:ss
~~~
Example: =|23:20:50|=

### Reduced accuracy representations

Basic format:
~~~
hhmm
~~~
Example: =|2320|=

Extended format:
~~~
hh:mm
~~~
Example: =|23:20|=

Basic format:
~~~
hh
~~~
Example: =|23|=

### Representations with decimal fraction

If a decimal fraction is included, lower order time elements (if any) shall be
omitted and the decimal fraction shall be divided from the integer part by the
decimal sign specified in ISO 31-0, i.e. the comma =|[,]|= (preferred)
or full stop =|[.]|=.

If the magnitude of the number is less than unity
(i.e. $0,0 \leq d \leq 1,0$), the decimal sign shall be preceded by two zeros
in accordance with zero padding in the rest of this specification.

The interchange parties, dependent upon the application, shall agree
the number of digits in the decimal fraction. The format shall be
=|[hhmmss,ss]|=, =|[hhmm,mm]|= or =|[hh,hh]|= as appropriate, with as many
digits as necessary following the decimal sign.

A decimal fraction shall have at least one digit.

Basic format:
~~~
hhmmss,ss
~~~
Example: =|232050,5|=

Extended format:
~~~
hh:mm:ss,ss
~~~
Example: =|23:20:50,5|=

Basic format:
~~~
hhmm,mm
~~~
Example: =|2320,8|=

Extended format:
~~~
hh:mm,mm
~~~
Example: =|23:20,8|=

Basic format:
~~~
hh,hh
~~~
Example: =|23,3|=

### Time designator

In expressions of local time, applications may put the time designator =|[T]|=
immediately in front of the representations defined above.

### Midnight

The complete representations in basic and extended format for midnight:

#### The beginning of a calendar day

Basic format:
~~~
000000
~~~

Extended format:
~~~
00:00:00
~~~

#### The end of a calendar day

Basic format:
~~~
240000
~~~

Extended format:
~~~
24:00:00
~~~

These representations may have reduced accuracy and/or may contain the time
designator.

These representations may be expanded with a decimal fraction containing only
zeros.

Midnight will normally be represented as =|00:00|= or =|24:00|=.

The end of one calendar day =|24:00|= coincides with =|00:00|= at the start
of the next calendar day. For example, =|24:00|= on 12 April 1985 is the same
as =|00:00|= on 13 April 1985. If there is no association with a date or a
time interval, then these representations represent the same local time in
the 24-hour timekeeping system.

Representations where =|hh|= has the value =|24|= are only preferred to
represent the end of a time interval or recurring time interval (see below).

### UTC of day

To express UTC of day the complete local time representations, possibly
including decimal fractions and/or reduced accuracy, are used.

This means that the time designator is not allowed.

Such a local time representation is followed immediately by
the UTC designator =|[Z]|=.

The examples below are complete and reduced
accuracy representations of the UTC of day 20 minutes and 30 seconds past 23 hours:

Basic format:
~~~
hhmmssZ
hhmmZ
hhZ
~~~
Examples:
  * =|232030Z|=
  * =|2320Z|=
  * =|23Z|=

Extended format:
~~~
hh:mm:ssZ
hh:mmZ
~~~
Examples:
  * =|23:20:30Z|=
  * =|23:20Z|=

### Local time and UTC time

When it is required to indicate the difference between local time and UTC of
day, the representation of the difference can be expressed in hours and
minutes, or hours only. It shall be expressed as positive (i.e. with the
leading plus sign =|[+]|=) if the local time is ahead of or equal to UTC
of day and as negative (i.e. with the leading minus sign =|[-]|=) if it is
behind UTC of day.

The minutes time element of the difference may only be omitted if the
difference between the time scales is exactly an integral number of hours.

Basic format:
~~~
±hhmm
±hh
~~~
Examples:
  * =|+0100|=
  * =|+01|=

Extended format:
~~~
±hh:mm
~~~
Example: =|+01:00|=

Expressions of the difference between local time and UTC of day are a
component in the representations defined below.
They are not used in isolation.

When it is required to indicate local time and the difference between
the time scale of local time and UTC, the representation of the difference
shall be appended to the representation of the local time following
immediately, without space, the lowest order (extreme right-hand) time
element of the local time expression.

The difference between the time scale of local time and UTC shall be
expressed in hours-and-minutes, or hours-only independent of the accuracy of
the local time expression associated with it.

In the following examples we represent the complete representation of the time
of 27 minutes and 46 seconds past 15 hours locally in Geneva (in winter one
hour ahead of UTC), and in New York (in winter five hours behind UTC),
together with the indication of the difference between the time scale of
local time and UTC, are used as examples.

Basic format:
~~~
hhmmss±hhmm
hhmmss±hh
~~~
Examples:
  * =|152746+0100|=
  * =|152746−0500|=
  * =|152746+01|=
  * =|152746−05|=

Extended format:
~~~
hh:mm:ss±hh:mm
hh:mm:ss±hh
~~~

Examples:
  * =|15:27:46+01:00|=
  * =|15:27:46−05:00|=
  * =|15:27:46+01|=
  * =|15:27:46−05|=

In these expressions the local time component may be represented with
reduced accuracy and/or with decimal fraction.

# Date and time of day representations

There are 3 variants:
  1. For *|calendar dates|*: year – month – day of the month –
     time designator – hour – minute – second – zone designator.
  2. For *|ordinal dates|*: year – day of the year – time designator – hour –
     minute – second – zone designator
  3. For *|week dates|*: year – week designator – week – day of the week –
     time designator – hour – minute – second – zone designator.

The zone indicator is either =|[Z]|=, in case of UTC of day, or a difference
components, in case of local time.

Time designator =|[T]|= is used to indicate the start of the representation of
the time of day component. It may be omitted in applications where there is no
risk of confusing a date and time of day representation with others defined in
this International Standard, and where there is mutual agreement between the
partners in information interchange.

## Examples of complete representations

Basic format:
~~~
YYYYMMDDThhmmss
YYYYMMDDThhmmssZ
YYYYMMDDThhmmss±hhmm
YYYYMMDDThhmmss±hh
~~~
Examples:
  * =|19850412T101530|=
  * =|19850412T101530Z|=
  * =|19850412T101530+0400|=
  * =|19850412T101530+04|=

Extended format:
~~~
YYYY-MM-DDThh:mm:ss
YYYY-MM-DDThh:mm:ssZ
YYYY-MM-DDThh:mm:ss±hh:mm
YYYY-MM-DDThh:mm:ss±hh
~~~
Examples:
  * =|1985-04-12T10:15:30|=
  * =|1985-04-12T10:15:30Z|=
  * =|1985-04-12T10:15:30+04:00|=
  * =|1985-04-12T10:15:30+04|=

In complete representations, calendar dates, ordinal dates, and week dates
may be use.

--

@author Wouter Beek
@see Unit tests are in [iso8601_test].
@tbd Implement the extended representations of the various date and time
     formats.
@version 2013/07-2013/08
*/

:- use_module(datetime(iso8601)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(math(radix)).



% CALENDAR DATE %

%! iso8601_calendar_date(
%!   ?Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(28,31)
%! )//

% We put several representation forms together in this DCG rule
% for compactness.
iso8601_calendar_date(T0, Format, Y, M, D) -->
  % Note that a reduced representation may consist only of a century.
  % For generation purposes, we want to put the century before the year
  % (in generation we prefer shorter strings).
  ({Format = basic}, iso8601_century(T1, Y) ; iso8601_year(T1, Y)),
  (
    % Reduced representations may end here.
    {(var(M), var(D))}
  ;
    % Sometimes a separator occurs between year and month.
    (
      % The extended format of the complete representation
      % has a hyphen between year and month.
      hyphen_minus, {Format = extended}
    ;
      % The basic format of the reduced representation
      % has a hyphen between year and month.
      hyphen_minus, {Format = basic, var(D)}
    ;
      % The basic format of the complete representation
      % has no hyphen between year and month.
      {Format = basic}
    ),
    iso8601_month_in_year(T2, M),
    (
      % Reduced representations may end here.
      {var(D)}
    ;
      % Sometimes a separator occurs between month and day.
      (
        % The extended format of the complete representation
        % has a hyphen between month and day.
        hyphen_minus, {Format = extended}
      ;
        {Format = basic}
      ),
      iso8601_day_in_month(T3, D)
    )
  ),
  {parse_tree(calendar_date, [T1,T2,T3], T0)}.

iso8601_calendar_date_time(
  T0, Format, Y, M, D, T, H, MM, S, UTC, Sign, HH, MMM
) -->
  iso8601_calendar_date(T1, Format, Y, M, D),
  iso8601_local_time(T2, Format, T, H, MM, S, UTC, Sign, HH, MMM),
  {parse_tree(date_time, [T1,T2], T0)}.



% LOCAL TIME %

%! iso8601_local_time(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?Hour:float,
%!   ?Minute:float,
%!   ?Second:float,
%!   ?UTC:boolean,
%!   ?Sign:boolean,
%!   ?HourCorrection:float,
%!   ?MinuteCorrection:float
%! )//
% Processes local time representations.
%
% ### Midnight
%
% The representation of midnight necessitates the following restrictions
% on processing representations of local time:
%
% *|[A1]|* If hour is =24=, then minute (if present) must be =00=,
%          and any decimal expansion must consist of zeros.
%
% *|[A2]|* If hour is =24=, then second (if present) must be =00=,
%          and any decimal expansion must consist of zeros.
%
% *|[B1]|* If hour is =00= and hour has a decimal expansion, than that
%          expansion must consist of zeros exclusively.
%
% *|[B2]|* If hour and minute are =00= and minute has
%          a decimal expansion than that expansion must consist
%         of zeros exclusively.
%
% *|[B3]|* If hour, minute, and second are =00= and second has
%          a decimal expansion, than that expansion must consist
%          of zeros exclusively.
%
% *|[C1]|* If minute is instantiated, then hour must not have
%          a decimal expansion.
%
% *|[C2]|* If minute and second are instantiated, then hour and
%          minute must not have a decimal expansion.
%
% @arg Tree A compound term representing a parse tree.
% @arg Format Either `basic` for the basic or compressed format,
%      or `extended` for the extended or human readable format.
% @arg TimeDesignator A boolean indicating whether the time designator is
%      included or not.
% @arg Hour Either an integer between 0 and 24,
%      or a float between 0.0 and 24.0.
% @arg Minute Either an integer between 0 and 60,
%      or a float between 0.0 and 60.0 EXCLUSIVE.
% @arg Second Either an integer between 0 and 60,
%      or a float between 0.0 and 60.0 EXCLUSIVE.
% @arg UTC A boolean, representing whether the string represents local
%      or univesal time.

iso8601_local_time(T0, Format, T, H, M, S, UTC, Sign, HH, MM) -->
  % Time designator.
  ({T = false} ; {T = true}, iso8601_time_designator(T1)),

  % Hour
  iso8601_hour_in_day(T2, H),
  {number_components(H, H_I, H_F)},

  % Minute and second
  (
    {(var(M), var(S))},
    % [A1] If hour is =24=, then its decimal expansion must
    %      consist of zeros exclusively.
    % [B1] If hour is =00=, then its decimal expansion must
    %      consist of zeros exclusively.
    {(H_I =:= 0 -> H_F = 0 ; true)}
  ;
    % [C1] If minute is instantiated, then hour must not have
    %      a decimal expansion.
    {H_F = 0},
    (colon, {Format = extended} ; {Format = basic}),
    iso8601_minute_in_hour(T3, M),
    {number_components(M, M_I, M_F)},
    % [A1] If hour is =24=, then minutes must be =00= and its
    %      decimal expansion (if any) must consist of zeros exclusively.
    {(H_I =:= 24 -> M_I =:= 0, M_F = 0 ; true)},

    % Second
    (
      {var(S)},
      % [B2] If hour and minute are =00=, then minutes's decimal extension
      %      must consist of zeros exclusively.
      {(H_I =:= 0, M_I =:= 0 -> M_F = 0 ; true)}
    ;
      % [C2] If minute and second are instantiated, then hour and minute
      %      must not have a decimal expansion.
      {H_F = 0, M_F = 0},
      (colon, {Format = extended} ; {Format = basic}),
      iso8601_second_in_minute(T4, S),
      {number_components(S, S_I, S_F)},
      % [C3] If hour, minute, and second are =00=, then minutes's decimal
      %      extension must consist of zeros exclusively.
      {(H_I =:= 0, M_I =:= 0, S_I =:= 0 -> S_F = 0 ; true)},
      % [A2] If hour is =24=, then second must be =00= and its
      %      decimal expansion (if any) must consist of zeros exclusively.
      {(H_I =:= 24 -> S_I =:= 0, S_F = 0 ; true)}
    )
  ),

  % UTC designator
  (iso8601_utc_designator(T5), {UTC = true} ; {var(T5), UTC = false}),

  % UTC correction time (only hour and minute).
  (
    {UTC = false},
    iso8601_utc_correction(T6, Format, Sign, HH, MM)
  ;
    {var(Sign), var(HH), var(MM)}
  ),

  % Parse tree
  {parse_tree(local_time, [T1,T2,T3,T4,T5,T6], T0)}.

%! iso8601_utc_correction(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Sign:boolean,
%!   ?Hour:between(0,59),
%!   ?Minute:between(0,59)
%! )//

iso8601_utc_correction(T0, Format, Sign, H, M) -->
  % Sign
  (
    {var(Sign), var(H), var(M)}
  ;
    iso8601_sign(T1, Sign),
    
    % Hour
    iso8601_hour_in_day(T2, H),
    
    % Minute
    (
      {var(M)}
    ;
      (colon, {Format = extended} ; {Format = basic}),
      iso8601_minute_in_hour(T3, M)
    )
  ),

  % Parse tree
  {parse_tree(utc_correction, [T1,T2,T3], T0)}.



% ORDINAL DATE %

%! iso8601_ordinal_date(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Day:between(1,366)
%! )//
% No reduced representations are defined for ordinal dates.

iso8601_ordinal_date(T0, Format, Y, D) -->
  iso8601_year(T1, Y),
  % Sometimes a separator occurs between year and day.
  (
    % The extended format of the complete representation
    % has a hyphen between year and day.
    hyphen_minus, {Format = extended}
  ;
    {Format = basic}
  ),
  iso8601_day_in_year(T2, D),
  {parse_tree(ordinal_date, [T1,T2], T0)}.

iso8601_ordinal_date_time(T0, Format, Y, D, T, H, M, S, UTC, Sign, HH, MM) -->
  iso8601_ordinal_date(T1, Format, Y, D),
  iso8601_local_time(T2, Format, T, H, M, S, UTC, Sign, HH, MM),
  {parse_tree(date_time, [T1,T2], T0)}.



% WEEK DATE %

%! iso8601_week_date(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Week:between(1,53),
%!   ?Day:between(1,7)
%! )//

iso8601_week_date(T0, Format, Y, W, D) -->
  iso8601_year(T1, Y),
  (hyphen_minus, {Format = extended} ; {Format = basic}),
  "W",
  iso8601_week_in_year(T2, W),
  (
    {var(D)}
  ;
    (hyphen_minus, {Format = extended} ; {Format = basic}),
    iso8601_day_in_week(T3, D),
    {parse_tree(week_date, [T1,T2,T3], T0)}
  ).

iso8601_week_date_time(T0, Format, Y, W, D, T, H, M, S, UTC, Sign, HH, MM) -->
  iso8601_week_date(T1, Format, Y, W, D),
  iso8601_local_time(T2, Format, T, H, M, S, UTC, Sign, HH, MM),
  {parse_tree(date_time, [T1,T2], T0)}.



% GENERIC COMPONENTS %

% Comma is prefered.
fraction_separator(',') --> comma.
fraction_separator('.') --> dot.

%! iso8601_float(
%!   -Tree:compound,
%!   +Name:atom,
%!   +Length:integer,
%!   ?Number:number
%! )//

iso8601_float(T0, Name, Length, N) -->
  {var(N)}, !,
  iso8601_integer(T1, integer, Length, N_I),
  (
    fraction_separator(T2),
    % Unspecified length.
    iso8601_integer(T3, fraction, _UnspecifiedLength, N_F),
    {number_components(N, N_I, N_F)}
  ;
    {N = N_I}
  ),
  {parse_tree(Name, [T1,T2,T3], T0)}.
iso8601_float(T0, Name, Length, N) -->
  {integer(N)}, !,
  iso8601_integer(T0, Name, Length, N).
iso8601_float(T0, Name, Length, N) -->
  {float(N)}, !,
  {number_components(N, N_I, N_F)},
  iso8601_integer(T1, integer, Length, N_I),
  fraction_separator(T2),
  iso8601_integer(T3, fraction, _UnspecifiedLength, N_F),
  {parse_tree(Name, [T1,T2,T3], T0)}.

%! iso8601_integer(
%!   -Tree:compound,
%!   +Name:atom,
%!   +Length:integer,
%!   ?Number:integer
%! )//

iso8601_integer(T0, Name, Length, I) -->
  {var(I)}, !,
  dcg_multi(decimal_digit, Length, Is),
  % Notice that we cannot use the decimal number in the parse tree,
  % because then we would miss any padding zeros.
  {digits_to_decimal(Is, I)},
  {parse_tree(Name, Is, T0)}.
iso8601_integer(T0, Name, Length, I) -->
  {(nonvar(Length) -> Length_ = Length ; number_length(I, Length_))},
  {padded_number(I, Length_, Is)},
  dcg_multi(decimal_digit, Length, Is),
  {parse_tree(Name, Is, T0)}.

%! padded_list(+List1:list, +Length:integer, -List2:list) is det.
% Padds the given list with zeros until it has the indicated length.

padded_list(L, N, L):-
  N =< 0, !.
padded_list(L1, N, [0|L2]):-
  NewN is N - 1,
  padded_list(L1, NewN, L2).

%! padded_number(
%!   +DecimalNumber:integer,
%!   +Length:integer,
%!   -DecimalDigits:list(between(0,9))
%! ) is det.
% Turns a decimal number into a list of decimal digits, padded with zeros.

padded_number(DecimalNumber, Length, DecimalDigits2):-
  decimal_to_digits(DecimalNumber, DecimalDigits1),
  length(DecimalDigits1, NumberOfDigits),
  NumberOfZeros is Length - NumberOfDigits,
  padded_list(DecimalDigits1, NumberOfZeros, DecimalDigits2).



% SPECIFIC COMPONENTS %

iso8601_century(T0, C) -->
  {var(C)}, !,
  iso8601_integer(T0, century, 2, C_),
  {C is C_ * 100},
  {iso8601_year(C)}.
iso8601_century(T0, C) -->
  {C_ is C / 100},
  iso8601_integer(T0, century, 2, C_),
  {iso8601_year(C)}.

iso8601_day_in_month(T0, D) -->
  iso8601_integer(T0, day_in_month, 2, D),
  {iso8601_day_in_month(D)}.

iso8601_day_in_week(T0, D) -->
  iso8601_integer(T0, day_in_week, 1, D),
  {iso8601_day_in_week(D)}.

iso8601_day_in_year(T0, D) -->
  iso8601_integer(T0, day_in_year, 3, D),
  {iso8601_day_in_year(D)}.

iso8601_hour_in_day(T0, H) -->
  iso8601_float(T0, hour, 2, H),
  {iso8601_hour_in_day(H)}.

iso8601_minute_in_hour(T0, M) -->
  iso8601_float(T0, minute, 2, M),
  {iso8601_minute_in_hour(M)}.

iso8601_month_in_year(T0, M) -->
  iso8601_integer(T0, month, 2, M),
  {iso8601_month_in_year(M)}.

iso8601_second_in_minute(T0, S) -->
  iso8601_float(T0, second, 2, S),
  {iso8601_second_in_minute(S)}.

iso8601_sign(sign('+'), true) -->
  "+".
iso8601_sign(sign('-'), false) -->
  "-".

iso8601_week_in_year(T0, W) -->
  iso8601_integer(T0, week, 2, W),
  {iso8601_week_in_year(W)}.

iso8601_year(T0, Y) -->
  iso8601_integer(T0, year, 4, Y),
  {iso8601_year(Y)}.

iso8601_time_designator(time_designator('T')) -->
  "T".

iso8601_utc_designator(utc_designator('Z')) -->
  "Z".

