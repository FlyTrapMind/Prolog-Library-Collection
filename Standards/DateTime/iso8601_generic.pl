:- module(
  iso8601_generic,
  [
    iso8601_integer//4, % -Tree:compound
                        % +Name:atom
                        % +Length:integer
                        % ?Number:integer
    iso8601_time_designator/2, % +UTC_Time:compound
                               % -TimeDesignator:boolean
    iso8601_time_designator//1, % -Tree:compound
    iso8601_week_designator//1 % -Tree:compound
  ]
).

/** <module> ISO8601_GENERIC

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

@author Wouter Beek
@tbd Appendix A, relations to earlier ISO standards, is not processed.
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(math(radix)).



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

%! iso8601_time_designator(
%!   +UTC_Time:compound,
%!   -TimeDesignator:boolean
%! ) is det.
% Notice that we are strict about the insertion of the time designator
% =|[T]|=, in that we require it to be in place in all date-time
% representations with non-empty time.
%
% We do this because otherwise there will be abmbiguities.
% For example =198504122320Z= can be parsed as =|1985-04-12T23:20Z=|
% or as =|1985-04T12:23:20Z|=.

iso8601_time_designator(utc_time(time(H,M,S),_UTC), false):-
  var(H), var(M), var(S).
iso8601_time_designator(_UTC_Time, true).

iso8601_time_designator(time_designator('T')) -->
  "T".

iso8601_week_designator('W') -->
  "W".

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

