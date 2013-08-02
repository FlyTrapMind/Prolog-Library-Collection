:- module(
  iso8601_dcg,
  [
    iso8601_calendar_date//5, % ?Tree:compound
                              % ?Format:oneof([basic,extended])
                              % ?Year:between(0,9999)
                              % ?Month:between(1,12)
                              % ?Day:between(28,31)
    iso8601_local_time//6, % -Tree:compound
                           % ?Format:oneof([basic,extended])
                           % ?Hour:float
                           % ?Minute:float
                           % ?Second:float
                           % ?UTC:boolean
    iso8601_ordinal_date//4, % -Tree:compound
                             % ?Format:oneof([basic,extended])
                             % ?Year:between(0,9999)
                             % ?Day:between(1,366)
    iso8601_week_date//5 % -Tree:compound
                         % ?Format:oneof([basic,extended])
                         % ?Year:between(0,9999)
                         % ?Week:between(1,53)
                         % ?Day:between(1,7)
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

--

@author Wouter Beek
@tbd Implement the extended representations of the various date and time
     formats.
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(plunit)).
:- use_module(math(radix)).



% CALENDAR DATE %

%! iso8601_calendar_date(
%!   ?Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(28,31)
%! )//

% Reduced representation may consist only of a century.
iso8601_calendar_date(century(T1), basic, Y, M, D) -->
  iso8601_century(T1, Y),
  {var(M)},
  {var(D)}.
% We put several representation forms together in this DCG rule
% for compactness.
iso8601_calendar_date(T0, Format, Y, M, D) -->
  iso8601_year(T1, Y),
  (
    % Reduced representations may end here.
    {var(M), var(D)}
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
    iso8601_month(T2, M),
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

:- begin_tests(iso8601_calendar_date).

%! iso8601_calendar_date_example(
%!   ?Format:oneof([basic,extended]),
%!   ?Date:atom,
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(1,31)
%! ) is nondet.

iso8601_calendar_date_example(basic,    '19850412'  , 1985, 4, 12).
iso8601_calendar_date_example(extended, '1985-04-12', 1985, 4, 12).
% Reduced examples:
iso8601_calendar_date_example(basic,    '1985-04'   , 1985, 4, _ ).
iso8601_calendar_date_example(basic,    '1985'      , 1985, _, _ ).
iso8601_calendar_date_example(basic,    '19'        , 1900, _, _ ).

test(
  iso8601_calendar_date_generate,
  [
    forall(iso8601_calendar_date_example(Format, Atom1, Y, M, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_calendar_date(_T0, Format, Y, M, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_calendar_date_parse,
  [
    forall(iso8601_calendar_date_example(Format, Atom, Y1, M1, D1)),
    true(maplist(=, [Y1, M1, D1], [Y2, M2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_calendar_date(_T0, Format, Y2, M2, D2), Codes)).

:- end_tests(iso8601_calendar_date).



% LOCAL TIME %

%! iso8601_local_time(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Hour:float,
%!   ?Minute:float,
%!   ?Second:float,
%!   ?UTC:boolean
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
% *|[D]|* If the UTC designator is used, then the time designator
%         cannot occur.
%
% @arg Tree A compound term representing a parse tree.
% @arg Format Either `basic` for the basic or compressed format,
%             or `extended` for the extended or human readable format.
% @arg Hour Either an integer between 0 and 24,
%           or a float between 0.0 and 24.0.
% @arg Minute Either an integer between 0 and 60,
%             or a float between 0.0 and 60.0 EXCLUSIVE.
% @arg Second Either an integer between 0 and 60,
%             or a float between 0.0 and 60.0 EXCLUSIVE.
% @arg UTC A boolean, representing whether the string represents local
%          or univesal time.

iso8601_local_time(T0, Format, H, M, S, UTC) -->
  ("" ; time_designator(T1)),
  iso8601_hour(T2, H),
  {number_components(H, H_I, H_F)},
  (
    {var(M)},
    {var(S)},
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
    iso8601_minute(T3, M),
    {number_components(M, M_I, M_F)},
    % [A1] If hour is =24=, then minutes must be =00= and its
    %      decimal expansion (if any) must consist of zeros exclusively.
    {(H_I =:= 24 -> M_I =:= 0, M_F = 0 ; true)},
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
      iso8601_second(T4, S),
      {number_components(S, S_I, S_F)},
      % [C3] If hour, minute, and second are =00=, then minutes's decimal
      %      extension must consist of zeros exclusively.
      {(H_I =:= 0, M_I =:= 0, S_I =:= 0 -> S_F = 0 ; true)},
      % [A2] If hour is =24=, then second must be =00= and its
      %      decimal expansion (if any) must consist of zeros exclusively.
      {(H_I =:= 24 -> S_I =:= 0, S_F = 0 ; true)}
    )
  ),
  % *|[D]|* If the UTC designator is used, then the time designator
  %         cannot occur.
  ({UTC = false} ; {var(T1)}, utc_designator(T5), {UTC = true}),
  {parse_tree(local_time, [T1,T2,T3,T4,T5], T0)}.

:- begin_tests(iso8601_local_time).

%! iso8601_local_time_example(
%!   ?Format:oneof([basic,extended]),
%!   ?Time:atom,
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59),
%!   ?Second:between(0,60),
%!   ?UTC:boolean
%! ) is nondet.

iso8601_local_time_example(basic,    '232050',     23,   20,   50,   false).
iso8601_local_time_example(extended, '23:20:50',   23,   20,   50,   false).
iso8601_local_time_example(basic,    '2320',       23,   20,   _,    false).
iso8601_local_time_example(extended, '23:20',      23,   20,   _,    false).
iso8601_local_time_example(basic,    '23',         23,   _,    _,    false).
iso8601_local_time_example(basic,    '232050,5',   23,   20,   50.5, false).
iso8601_local_time_example(extended, '23:20:50,5', 23,   20,   50.5, false).
iso8601_local_time_example(basic,    '2320,8',     23,   20.8, _,    false).
iso8601_local_time_example(extended, '23:20,8',    23,   20.8, _,    false).
iso8601_local_time_example(basic,    '23,3',       23.3, _,    _,    false).
iso8601_local_time_example(basic,    '000000',     0,    0,    0,    false).
iso8601_local_time_example(extended, '00:00:00',   0,    0,    0,    false).
iso8601_local_time_example(basic,    '240000',     24,   0,    0,    false).
iso8601_local_time_example(extended, '24:00:00',   24,   0,    0,    false).
iso8601_local_time_example(basic,    '232030Z',    23,   20,   30,   true ).
iso8601_local_time_example(basic,    '2320Z',      23,   20,   _,    true ).
iso8601_local_time_example(basic,    '23Z',        23,   _,    _,    true ).
iso8601_local_time_example(extended, '23:20:30Z',  23,   20,   30,   true ).
iso8601_local_time_example(extended, '23:20Z',     23,   20,   _,    true ).

test(
  iso8601_local_time_generate,
  [
    forall(iso8601_local_time_example(Format, Atom1, H, M, S, UTC)),
    true(Atom1 = Atom2)
  ]
):-
  once(phrase(iso8601_local_time(_T0, Format, H, M, S, UTC), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_local_time_parse,
  [
    forall(iso8601_local_time_example(Format, Atom, H1, M1, S1, UTC1)),
    true(maplist(=, [H1, M1, S1, UTC1], [H2, M2, S2, UTC2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_local_time(_T0, Format, H2, M2, S2, UTC2), Codes)).

:- end_tests(iso8601_local_time).



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

:- begin_tests(iso8601_ordinal_date).

%! iso8601_ordinal_date_example(
%!   ?Format:oneof([basic,extended]),
%!   ?Date:atom,
%!   ?Year:between(0,9999),
%!   ?Day:between(1,366)
%! ) is nondet.

iso8601_ordinal_date_example(basic,    '1985102',  1985, 102).
iso8601_ordinal_date_example(extended, '1985-102', 1985, 102).

test(
  iso8601_ordinal_date_generate,
  [
    forall(iso8601_ordinal_date_example(Format, Atom1, Y, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_ordinal_date(_T0, Format, Y, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_ordinal_date_parse,
  [
    forall(iso8601_ordinal_date_example(Format, Atom, Y1, D1)),
    true(maplist(=, [Y1, D1], [Y2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_ordinal_date(_T0, Format, Y2, D2), Codes)).

:- end_tests(iso8601_ordinal_date).



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
  iso8601_week(T2, W),
  (
    {var(D)}
  ;
    (hyphen_minus, {Format = extended} ; {Format = basic}),
    iso8601_day_in_week(T3, D),
    {parse_tree(week_date, [T1,T2,T3], T0)}
  ).

:- begin_tests(iso8601_week_date).

%! iso8601_week_date_example(
%!   ?Format:oneof([basic,extended]),
%!   ?Date:atom,
%!   ?Year:between(0,9999),
%!   ?Week:between(1,53),
%!   ?Day:between(1,7)
%! ) is nondet.

iso8601_week_date_example(basic,    '1985W155',   1985, 15, 5).
iso8601_week_date_example(extended, '1985-W15-5', 1985, 15, 5).
% Reduced accuracy:
iso8601_week_date_example(basic,    '1985W15',    1985, 15, _).
iso8601_week_date_example(extended, '1985-W15',   1985, 15, _).

test(
  iso8601_week_date_generate,
  [
    forall(iso8601_week_date_example(Format, Atom1, Y, W, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_week_date(_Tree, Format, Y, W, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_week_date_parse,
  [
    forall(iso8601_week_date_example(Format, Atom, Y1, W1, D1)),
    true(maplist(=, [Y1, W1, D1], [Y2, W2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_week_date(_Tree, Format, Y2, W2, D2), Codes)).

:- end_tests(iso8601_week_date).



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

time_designator(time_designator('T')) -->
  "T".

utc_designator(utc_designator('Z')) -->
  "Z".



% SPECIFIC COMPONENTS %

iso8601_century(T0, C) -->
  {var(C)}, !,
  iso8601_integer(T0, century, 2, C_),
  {C is C_ * 100}.
iso8601_century(T0, C) -->
  {C_ is C / 100},
  iso8601_integer(T0, century, 2, C_).

iso8601_day_in_month(T0, D) -->
  iso8601_integer(T0, day_in_month, 2, D).

iso8601_day_in_year(T0, D) -->
  iso8601_integer(T0, day_in_year, 3, D).

iso8601_day_in_week(T0, D) -->
  iso8601_integer(T0, day_in_week, 1, D).

iso8601_hour(T0, H) -->
  iso8601_float(T0, hour, 2, H).

iso8601_minute(T0, M) -->
  iso8601_float(T0, minute, 2, M).

iso8601_month(T0, M) -->
  iso8601_integer(T0, month, 2, M).

iso8601_second(T0, S) -->
  iso8601_float(T0, second, 2, S).

iso8601_week(T0, W) -->
  iso8601_integer(T0, week, 2, W).

iso8601_year(T0, Y) -->
  iso8601_integer(T0, year, 4, Y).

