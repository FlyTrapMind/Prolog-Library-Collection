:- module(
  xsd_gMonth,
  [
    gMonthCanonicalMap/2, % +GregorianMonth:compound
                          % -Lexical:list(code)
    gMonthLexicalMap/2 % +Lexical:list(code)
                       % -GregorianMonth:compound
  ]
).

/** <module> XSD Gregorian month

*=gMonth=* represents whole (Gregorian) months within an arbitrary year—months
that recur at the same point in each year. It might be used, for example,
to say what month annual Thanksgiving celebrations fall in different
countries (=|--11|= in the United States, =|--10|= in Canada, and possibly
other months in other countries).

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gMonth uses the date/timeSevenPropertyModel, with year, day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gMonth are "projections" of those of dateTime.

### Facets

The gMonth datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The gMonth datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of
new types, if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from gMonth may also specify values
for the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The gMonth datatype has the following values for its fundamental facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAPPING %

%! gMonthCanonicalMap(+GregorianMonth:compound, -Lexical:list(code)) is det.
% A compound term that represents a Gregorian year has the following form:
% ~~~
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
% Where only the values month and time zone are used.

gMonthCanonicalMap(GD, Lexical):-
  phrase(gMonthCanonicalMap(GD), Lexical).

%! gMonthCanonicalMap(+GregorianMonth:compound)//
% Maps a gMonth value to a gMonthLexicalRep//.
%
% @arg GregorianMonth A complete gMonth value.

gMonthCanonicalMap(dateTime(_Y,M,_D,_H,_MM,_S,TZ)) -->
  dcg_multi(hyphen, 2),
  monthCanonicalFragmentMap(M),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAPPING %

%! gMonthLexicalMap(+Lexical:list(code), -GregorianMonth:compound) is nondet.
% A compound term that represents a Gregorian year has the following form:
% ~~~
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
% Where only the values month and time zone are used.

gMonthLexicalMap(Lexical, GD):-
  phrase(gMonthLexicalRep(GD), Lexical).

%! gMonthLexicalRep(-GregorianMonth:compound)//
% Maps a gMonthLexicalRep// to a gMonth value.
%
% ~~~{.ebnf}
% gMonthLexicalRep ::= '--' monthFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% --(0[1-9]|1[0-2])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

gMonthLexicalRep(DT) -->
  dcg_multi(hyphen, 2),
  monthFrag(M),
  ("" ; timezoneFrag(TZ)), !,
  {newDateTime(_Y, M, _D, _H, _MM, _S, TZ, DT)}.
