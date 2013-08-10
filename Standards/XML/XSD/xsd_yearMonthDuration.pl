:- module(
  xsd_yearMonthDuration,
  [
    yearMonthDurationCanonicalMap/2, % +YearMonthDuration:compound
                                     % -LEX:list(code)
    yearMonthDurationLexicalMap/2, % +LEX:list(code)
                                   % -YearMonthDuration:compound
  ]
).

/** <module> XSD_YEAR_MONTH_DURATION

*=yearMonthDuration=* is a datatype derived from =duration= by restricting its
lexical representations to instances of yearMonthDurationLexicalRep//.

### Value spae

The value space of =yearMonthDuration= is that of =duration= restricted to
those whose seconds property is =0=.

#### Order

This results in a duration datatype which is totally ordered.

#### Implementation

The always-zero seconds is formally retained in order that
=yearMonthDuration='s (abstract) value space truly be a subset of that
of duration An obvious implementation optimization is to ignore the zero
and implement =yearMonthDuration= values simply as integer values.

### Lexical mapping

The lexical space is reduced from that of =duration= by disallowing
duDayFrag// and duTimeFrag// fragments in the lexical representations.

#### RE

~~~
-?P((([0-9]+Y)([0-9]+M)?)|([0-9]+M))
~~~
or
~~~
-?P[0-9]+(Y([0-9]+M)?|M)
~~~
but the formal definition of yearMonthDuration uses a simpler RE in its
pattern facet: =|[^DT]*|=. This pattern matches only strings of characters
which contain no =D= and no =T=, thus restricting the lexical space of
duration to strings with no day, hour, minute, or seconds fields.

### Canonical mapping

The canonical mapping is that of duration restricted in its range to
the lexical space.

The yearMonthDuration value whose months and seconds are both zero has
no canonical representation in this datatype since its canonical
representation in duration (=PT0S=) is not in the lexical space of
yearMonthDuration.

### Facets

The yearMonthDuration datatype and all datatypes derived from it by
restriction have the following constraining facets with fixed values:
  * =|whiteSpace = collapse (fixed)|=

The yearMonthDuration datatype has the following constraining facets with
the values shown; these facets may be specified in the derivation of new
types, if the value given is at least as restrictive as the one shown:
  * =|pattern = [^DT]*|=

Datatypes derived by restriction from yearMonthDuration may also specify
values for the following constraining facets:
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

The yearMonthDuration datatype has the following values for its fundamental
facets:
  * =|ordered = partial|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

The ordered facet has the value partial even though the datatype is in fact
totally ordered, because the value of that facet is unchanged by derivation. 

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(xsd(xsd_duration)).



%! yearMonthDurationCanonicalMap(
%!   +YearMonthDuration:number,
%!   -LEX:list(code)
%! ) is det.

yearMonthDurationCanonicalMap(YMD, LEX):-
  phrase(yearMonthDurationCanonicalMap(YMD), LEX).

yearMonthDurationCanonicalMap(duration(M,0)) -->
  durationCanonicalMap(duration(M,0)).

%! yearMonthDurationLexicalMap(
%!   +LEX:list(code)
%!   -YearMonthDuration:number
%! ) is det.

yearMonthDurationLexicalMap(LEX, YMD):-
  phrase(yearMonthDurationLexicalRep(YMD), LEX).

%! yearMonthDurationLexicalRep(-Duration:compound)//
% ~~~{.ebnf}
% yearMonthDurationLexicalRep ::= '-'? 'P' duYearMonthFrag
% ~~~

yearMonthDurationLexicalRep(duration(M,0)) -->
  (minus_sign, {Sign = -1} ; {Sign = 1}),
  "P",
  duYearMonthFrag(M).

