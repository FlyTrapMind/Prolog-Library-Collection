:- module(
  xsd_gMonthDay,
  [
    gMonthDayCanonicalMap/2, % +GregorianMonthDay:compound
                             % -LEX:list(code)
    gMonthDayLexicalMap/2 % +LEX:list(code)
                          % -GregorianMonthDay:compound
  ]
).

/** <module> XSD_G_MONTH_DAY

gMonthDay represents whole calendar days that recur at the same point
in each calendar year, or that occur in some arbitrary calendar year.
(Obviously, days beyond 28 cannot occur in all Februaries;
29 is nonetheless permitted.)

This datatype can be used, for example, to record birthdays;
an instance of the datatype could be used to say that someone's birthday
occurs on the 14th of September every year.

Because day/month combinations in one calendar only rarely correspond to
day/month combinations in other calendars, values of this type do not,
in general, have any straightforward or intuitive representation in terms of
most other calendars. This type should therefore be used with caution in
contexts where conversion to other calendars is desired.

### Value Space

gMonthDay uses the date/timeSevenPropertyModel, with year, hour, minute, and
second required to be absent. timezoneOffset remains optional.

#### Constraint: Day-of-month Values

The day value must be no more than 30 if month is one of 4, 6, 9, or 11,
and no more than 29 if month is 2.

#### XSD 1.0 compatibility

In version 1.0 of this specification, gMonthDay values did not retain
a time zone offset explicitly, but for time zone offsets not too far from
UTC their time zone offset could be recovered based on their value's first
moment on the timeline. The date/timeSevenPropertyModel retains all time zone
offsets.

An example that shows the difference from version 1.0:
A day is a calendar (or "local time") day offset from UTC by the appropriate
interval; this is now true for all day values, including those with time zone
offsets outside the range +12:00 through -11:59 inclusive:
=|12-12+13:00 < --12-12+11:00|= (just as =|12-12+12:00|= has always been less
than =|12-12+11:00|=, but in version 1.0 =|12-12+13:00 > --12-12+11:00|=,
since =|12-12+13:00|='s "recoverable time zone offset" was =|−11:00|=).

### Lexical Mapping

The lexical representations for gMonthDay are "projections" of those of
dateTime.

### Facets

The gMonthDay datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The gMonthDay datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of
new types, if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from gMonthDay may also specify values
for the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The gMonthDay datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(xsd(xsd_dateTime)).



% CANONICAL MAPPING %

%! gMonthDayCanonicalMap(+GregorianMonthDay:compound, -LEX:list(code)) is det.

gMonthDayCanonicalMap(GY, LEX):-
  phrase(gMonthDayCanonicalMap(GY), LEX).

%! gMonthDayCanonicalMap(+GregorianMonthDay:compound)//
% Maps a gMonthDay value to a gMonthDayLexicalRep//.
%
% @param GregorianMonthDay A complete gMonthDay value.

gMonthDayCanonicalMap(dateTime(_Y,M,D,_H,_MM,_S,TZ)) -->
  dcg_multi(hyphen, 2),
  monthCanonicalFragmentMap(M),
  hyphen,
  dayCanonicalFragmentMap(D),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAPPING %

%! gMonthDayLexicalMap(
%!   +LEX:list(code),
%!   -GregorianMonthDay:compound
%! ) is nondet.

gMonthDayLexicalMap(LEX, GY):-
  phrase(gMonthDayLexicalRep(GY), LEX).

%! gMonthDayLexicalRep(-GregorianMonthDay:compound)//
% Maps a gMonthDayLexicalRep// to a gMonthDay value.
%
% ~~~{.ebnf}
% gMonthDayLexicalRep ::= '--' monthFrag '-' dayFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% --(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

gMonthDayLexicalRep(GMD) -->
  dcg_multi(hyphen, 2),
  monthFrag(M),
  hyphen,
  dayFrag(D),
  {dayInMonth(M, D)},
  ("" ; timezoneFrag(TZ)), !,
  {newDateTime(_Y, M, D, _H, _MM, _S, TZ, GMD)}.

