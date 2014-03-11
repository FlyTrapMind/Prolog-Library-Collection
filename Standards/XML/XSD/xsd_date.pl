:- module(
  xsd_date,
  [
    xsd_date_canonical_map/2, % +Date:compound
                              % -Lexical:list(code)
    xsd_date_canonical_map//1, % +Date:compound
    xsd_date_lexical_map/2, % +Lexical:or([atom,list(code)])
                            % -Date:compound
    xsd_date_lexical_map//1 % -Date:compound
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

--

@author Wouter Beek
@version 2013/08-2013/09, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(nlp(dcg_date)).
:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAPPING %

%! xsd_date_canonical_map(+Date:compound, -Lexical:list(code)) is det.
% A compound term that represents a data has the following form:
% ~~~{.pl}
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
%
% Where the values for year, month, day, and time zone are given.

xsd_date_canonical_map(Date, Lexical):-
  phrase(xsd_date_canonical_map(Date), Lexical).


%! xsd_date_canonical_map(+Date:compound)//
% Maps a date value to a xsd_date_lexical_map//.
%
% @arg Date A compound date value.
% @tbd Are the alternative calls to this DCG rule useful?

xsd_date_canonical_map(dateTime(Y,M,D,_,_,_,TZ)) -->
  yearCanonicalFragmentMap(Y),
  hyphen,
  monthCanonicalFragmentMap(M),
  hyphen,
  dayCanonicalFragmentMap(D),
  ({var(TZ)}, ! ; timezoneCanonicalFragmentMap(TZ)).



% LEXICAL MAPPING %

%! xsd_date_lexical_map(
%!   +Lexical:or([atom,list(code)]),
%!   -Date:compound
%! ) is det.
% A compound term that represents a data has the following form:
% ~~~{.pl}
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
%
% Where the values for year, month, day, and time zone are given.

xsd_date_lexical_map(Lexical, Date):-
  dcg_phrase(xsd_date_lexical_map(Date), Lexical).


%! xsd_date_lexical_map(-DateTime:compound)//
%
% ~~~{.ebnf}
% xsd_date_lexical_map ::= yearFrag '-' monthFrag '-' dayFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_date_lexical_map(DT) -->
  yearFrag(Y), hyphen, monthFrag(M), hyphen, dayFrag(D),
  {dayInMonth(Y, M, D)},
  ("" ; timezoneFrag(TZ)),
  {newDateTime(Y,M,D,_H,_MM,_S,TZ,DT)}.

