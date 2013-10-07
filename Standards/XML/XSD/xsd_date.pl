:- module(
  xsd_date,
  [
    dateCanonicalMap/2, % +Date:compound
                        % -LEX:list(code)
    dateLexicalMap/2 % +LEX:list(code)
                     % -Date:compound
  ]
).

/** <module> XSD date

*=date=* represents top-open intervals of exactly one day in length on
the timelines of dateTime, beginning on the beginning moment of each day,
up to but not including the beginning moment of the next day).
For non-timezoned values, the top-open intervals disjointly cover
the non-timezoned timeline, one per day. For timezoned values,
the intervals begin at every minute and therefore overlap.

### Value Space

Date uses the date/timeSevenPropertyModel, with hour, minute, and second
required to be absent. timezoneOffset remains optional.

### Lexical space

The lexical representations for gYearMonth are "projections" of those
of dateTime.

### Facets

The date datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The date datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of
new types, if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from date may also specify values
for the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The date datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

--

@author Wouter Beek
@version 2013/08-2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_date)).
:- use_module(dcg(dcg_generic)).
:- use_module(xsd(xsd_dateTime)).



% CANONICAL MAPPING %

%! dateCanonicalMap(+Date:compound, -LEX:list(code)) is det.
% A compound term that represents a data has the following form:
% ~~~
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
% Where the values for year, month, day, and time zone are given.

dateCanonicalMap(Date, LEX):-
  phrase(dateCanonicalMap(Date), LEX).

%! dateCanonicalMap(+Date:compound)//
% Maps a date value to a dateLexicalRep//.
%
% @param Date A complete date value.

dateCanonicalMap(Atom) -->
  {atom(Atom)}, !,
  {dcg_phrase(date(_Lang, Compound), Atom)},
  dateCanonicalMap(Compound).
dateCanonicalMap(date(Y,M,D)) --> !,
  dateCanonicalMap(dateTime(Y,M,D,_H,_MM,_S,_TZ)).
dateCanonicalMap(dateTime(Y,M,D,_H,_MM,_S,TZ)) -->
  yearCanonicalFragmentMap(Y), hyphen,
  monthCanonicalFragmentMap(M), hyphen,
  dayCanonicalFragmentMap(D),
  ({var(TZ)}, ! ; timezoneCanonicalFragmentMap(TZ)).



% LEXICAL MAPPING %

%! dateLexicalMap(+LEX:list(code), -Date:compound) is det.
% A compound term that represents a data has the following form:
% ~~~
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
% Where the values for year, month, day, and time zone are given.

dateLexicalMap(LEX, Date):-
  phrase(dateLexicalRep(Date), LEX).

%! dateLexicalRep(-DateTime:compound)//
%
% ~~~{.ebnf}
% dateLexicalRep ::= yearFrag '-' monthFrag '-' dayFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

dateLexicalRep(DT) -->
  yearFrag(Y), hyphen, monthFrag(M), hyphen, dayFrag(D),
  {dayInMonth(Y, M, D)},
  ("" ; timezoneFrag(TZ)),
  {newDateTime(Y,M,D,_H,_MM,_S,TZ,DT)}.
