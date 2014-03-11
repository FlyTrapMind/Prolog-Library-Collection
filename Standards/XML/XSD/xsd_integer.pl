:- module(
  xsd_integer,
  [
    integerCanonicalMap/2, % +Integer:integer
                           % -Lexical:list(code)
    integerLexicalMap/2 % +Lexical:list(code)
                        % -Integer:integer
  ]
).

/** <module> XSD_INTEGER

*=integer=* is derived from decimal by fixing the value of =fractionDigits=
to be 0 and disallowing the trailing decimal point. This results in the
standard mathematical concept of the integer numbers. The value space of
=integer= is the infinite set =|{...,-2,-1,0,1,2,...}|=. The base type of
integer is decimal.

### Lexical representation

=integer= has a lexical representation consisting of a finite-length sequence
of one or more decimal digits (=|#x30|=-=|#x39|=) with an optional leading
sign. If the sign is omitted, "=|+|=" is assumed.

Examples: =|-1|=, =0=, =12678967543233=, =|+100000|=.

### Canonical representation

The canonical representation for integer is defined by prohibiting certain
options from the Lexical representation. Specifically, the preceding optional
"=|+|=" sign is prohibited and leading zeroes are prohibited.

### Facets

The integer datatype and all datatypes derived from it by restriction have
the following constraining facets with fixed values:
  * =|fractionDigits = 0 (fixed)|=
  * =|whiteSpace = collapse (fixed)|=

The integer datatype has the following constraining facets.
For derived types, the value of this facet's value should at least be as
restrictive as this value:
  * =|pattern = [\-+]?[0-9]+|=

Datatypes derived by restriction from integer may also specify values for
the following constraining facets:
  * =totalDigits=
  * =enumeration=
  * =maxInclusive=
  * =maxExclusive=
  * =minInclusive=
  * =minExclusive=
  * =assertions=

Fundamental facet values:
  * =|ordered = total|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = true|=

Derived datatypes:
  * =nonPositiveInteger=
  * =long=
  * =nonNegativeInteger=

--

@author Wouter Beek
@version 2013/08-2013/09
*/

:- use_module(xsd(xsd_decimal)).



%! integerCanonicalMap(+Integer:integer, -Lexical:list(code)) is semidet.

integerCanonicalMap(I, Lexical):-
  decimalCanonicalMap(I, Lexical).

%! integerLexicalMap(+Lexical:list(code), -Integer:integer) is nondet.

integerLexicalMap(Lexical, I):-
  phrase(noDecimalPtNumeral(_Sign, I), Lexical).
