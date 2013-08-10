:- module(
  xsd_gYear,
  [
    gYearCanonicalMap/2, % +GregorianYear:compound
                         % -LEX:list(code)
    gYearLexicalMap/2 % +LEX:list(code)
                      % -GregorianYear:compound
  ]
).

/** <module> XSD_G_YEAR

gYear represents Gregorian calendar years.

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gYear uses the date/timeSevenPropertyModel, with month, day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gYear are "projections" of those
of dateTime.

### Facets

The gYear datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The gYear datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of
new types, if the value given is at least as restrictive as the one shown:
  * =|explicitTimezone = optional|=

Datatypes derived by restriction from gYear may also specify values
for the following constraining facets:
  * =pattern=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The gYear datatype has the following values for its fundamental facets:
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

%! gYearCanonicalMap(+GregorianYear:compound, -LEX:list(code)) is det.

gYearCanonicalMap(GY, LEX):-
  phrase(gYearCanonicalMap(GY), LEX).

%! gYearCanonicalMap(+GregorianYear:compound)//
% Maps a gYear value to a gYearLexicalRep//.
%
% @param GregorianYear A complete gYear value.

gYearCanonicalMap(dateTime(Y,_M,_D,_H,_MM,_S,TZ)) -->
  yearCanonicalFragmentMap(Y),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAPPING %

%! gYearLexicalMap(+LEX:list(code), -GregorianYear:compound) is nondet.

gYearLexicalMap(LEX, GY):-
  phrase(gYearLexicalRep(GY), LEX).

%! gYearLexicalRep(-GregorianYear:compound)//
% Maps a gYearLexicalRep// to a gYear value.
%
% ~~~{.ebnf}
% gYearLexicalRep ::= yearFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

gYearLexicalRep(GY) -->
  yearFrag(Y),
  ({var(TZ)} ; timezoneFrag(TZ)), !,
  {newDateTime(Y, _M, _D, _H, _MM, _S, TZ, GY)}.

