:- module(
  xsd_gYearMonth,
  [
    gYearMonthCanonicalMap/2, % +GregorianYearMonth:compound
                              % -LEX:list(code)
    gYearMonthLexicalMap/2 % +LEX:list(code)
                           % -GregorianYearMonth:compound
  ]
).

/** <module> XSD_G_YEAR_MONTH

gYearMonth represents specific whole Gregorian months in specific Gregorian
years.

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gYearMonth uses the date/timeSevenPropertyModel, with day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gYearMonth are "projections" of those
of dateTime.

### Facets

The gYearMonth datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The gYearMonth datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of
new types, if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from gYearMonth may also specify values
for the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The gYearMonth datatype has the following values for its fundamental facets:
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

%! gYearMonthCanonicalMap(
%!   +GregorianYearMonth:compound,
%!   -LEX:list(code)
%! ) is det.

gYearMonthCanonicalMap(GYM, LEX):-
  phrase(gYearMonthCanonicalMap(GYM), LEX).

%! gYearMonthCanonicalMap(+GregorianYearMonth:compound)//
% Maps a gYearMonth value to a gYearMonthLexicalRep//.
%
% @param GregorianYearMonth A complete gYearMonth value.

gYearMonthCanonicalMap(dateTime(Y,M,_D,_H,_MM,_S,TZ)) -->
  yearCanonicalFragmentMap(Y), hyphen, monthCanonicalFragmentMap(M),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAPPING %

%! gYearMonthLexicalMap(
%!   +LEX:list(code),
%!   -GregorianYearMonth:compound
%! ) is nondet.

gYearMonthLexicalMap(LEX, GYM):-
  phrase(gYearMonthLexicalRep(GYM), LEX).

%! gYearMonthLexicalRep(-GregorianYearMonth:compound)//
% Maps a gYearMonthLexicalRep// to a gYearMonth value.
%
% ~~~{.ebnf}
% gYearMonthLexicalRep ::= yearFrag '-' monthFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

gYearMonthLexicalRep(GYM) -->
  yearFrag(Y), hyphen, monthFrag(M),
  ({var(TZ)} ; timezoneFrag(TZ)), !,
  {newDateTime(Y, M, _D, _H, _MM, _S, TZ, GYM)}.

