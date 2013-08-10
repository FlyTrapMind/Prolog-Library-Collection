:- module(
  xsd_time,
  [
    timeCanonicalMap/2, % +Time:compound
                        % -LEX:list(code)
    timeLexicalMap/2 % +LEX:list(code)
                     % -Time:compound
  ]
).

/** <module> XSD_TIME

Time represents instants of time that recur at the same point in each calendar
day, or that occur in some arbitrary calendar day.

### Value Space

Time uses the date/timeSevenPropertyModel, with year, month, and day required
to be absent. timezoneOffset remains optional.

#### Timezone offset

A calendar (or "local time") day with a larger positive time zone offset
begins earlier than the same calendar day with a smaller (or negative)
time zone offset. Since the time zone offsets allowed spread over 28 hours,
it is possible for the period denoted by a given calendar day with one
time zone offset to be completely disjoint from the period denoted by
the same calendar day with a different offset — the earlier day ends
before the later one starts.

The moments in time represented by a single calendar day are spread over
a 52-hour interval, from the beginning of the day in the =|+14:00|= time zone
offset to the end of that day in the =|−14:00|= time zone offset.

#### Order

Time values (points in time in an "arbitrary" day) are ordered taking into
account their timezoneOffset.

The relative order of two time values, one of which has a timezoneOffset
of absent is determined by imputing time zone offsets of both =|+14:00|= and
=|−14:00|= to the value without an offset. Many such combinations will be
incomparable because the two imputed time zone offsets yield different orders.
However, for a given non-timezoned value, there will always be timezoned
values at one or both ends of the 52-hour interval that are comparable
(because the interval of incomparability is only 28 hours wide).

#### XSD 1.0 compatibility

Some pairs of time literals which in the 1.0 version of this specification
denoted the same value now (in this version) denote distinct values instead,
because values now include time zone offset information. Some such pairs,
such as =|05:00:00-03:00|= and =|10:00:00+02:00|=, now denote equal though
distinct values (because they identify the same points on the time line);
others, such as =|23:00:00-03:00|= and =|02:00:00Z|=, now denote unequal
values (=|23:00:00−03:00 > 02:00:00Z|= because =|23:00:00−03:00|= on any
given day is equal to =|02:00:00Z|= on the next day). 

### Lexical Mappings

The lexical representations for time are "projections" of those of
dateTime.

The lexical mapping maps =|00:00:00|= and =|24:00:00|= to the same value,
namely midnight (hour, minute, and second are zero).

### Facets

The time datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values;
these facets must not be changed from the values shown:
  * =|whiteSpace = collapse (fixed)|=

The time datatype has the following constraining facets with the values shown;
these facets may be specified in the derivation of new types,
if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from time may also specify values for
the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The time datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(xsd(xsd_dateTime)).



% CANONICAL MAPPING %

%! timeCanonicalMap(+Time:compound, -LEX:list(code)) is det.

timeCanonicalMap(T, LEX):-
  phrase(timeCanonicalMap(T), LEX).

%! timeCanonicalMap(+Time:compound)//
% Maps a time value to a timeLexicalRep//.
%
% @param Time A complete time value.

timeCanonicalMap(dateTime(_Y,_M,_D,H,M,S,TZ)) -->
  hourCanonicalFragmentMap(H),
  colon,
  minuteCanonicalFragmentMap(M),
  colon,
  secondCanonicalFragmentMap(S),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAPPING %

%! timeLexicalMap(+LEX:list(code), -Time:compound) is nondet.

timeLexicalMap(LEX, T):-
  phrase(timeLexicalRep(T), LEX).

%! timeLexicalRep(-DateTime:compound)//
%
% #### Grammar definitions
%
% ~~~{.ebnf}
% timeLexicalRep ::=
%     ((hourFrag ':' minuteFrag ':' secondFrag) | endOfDayFrag)
%     timezoneFrag?
% ~~~
%
% ~~~{.re}
% (([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?|(24:00:00(\.0+)?))
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~
%
% The EBNF and RE do not enforce additional constraints on timeLexicalRep//,
% that are actually part of its definition.

timeLexicalRep(DT) -->
  (
    hourFrag(H), colon, minuteFrag(M), colon, secondFrag(S)
  ;
    endOfDayFrag(H, M, S)
  ),
  (timezoneFrag(TZ) ; "").
  {newDateTime(_Y, _M, _D, H, M, S, TZ, DT)}.

