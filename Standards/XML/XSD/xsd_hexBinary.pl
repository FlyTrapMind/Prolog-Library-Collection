:- module(
  xsd_hexBinary,
  [
    hexBinaryCanonicalMap/2, % +HexBinary:list(between(0,1))
                             % -LEX:list(code)
    hexBinaryLexicalMap/2, % +LEX:list(code)
                          % -HexBinary:list(between(0,1))
  ]
).

/** <module> XSD_HEX_BINARY

*=hexBinary=* represents arbitrary hex-encoded binary data.

### Value Space

The value space of hexBinary is the set of finite-length sequences of zero or
more binary octets. The length of a value is the number of octets.

### Lexical Mapping

hexBinary's lexical space consists of strings of hex (hexadecimal) digits,
two consecutive digits representing each octet in the corresponding value
(treating the octet as the binary representation of a number between 0 and
255). For example, =0FB7= is a lexical representation of the two-octet value
=|00001111 10110111|=.

The set recognized by hexBinary is the same as that recognized by the regular expression '([0-9a-fA-F]{2})*'.

The ·lexical mapping· of hexBinary is ·hexBinaryMap·.

The ·canonical mapping· of hexBinary is given formally in ·hexBinaryCanonical·.
3.3.15.3 Facets

The hexBinary datatype and all datatypes derived from it by restriction have the following ·constraining facets· with fixed values; these facets must not be changed from the values shown:

    whiteSpace = collapse (fixed)

Datatypes derived by restriction from hexBinary may also specify values for the following ·constraining facets·:

    length
    minLength
    maxLength
    pattern
    enumeration
    assertions

The hexBinary datatype has the following values for its ·fundamental facets·:

    ordered = false
    bounded = false
    cardinality = countably infinite
    numeric = false
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(math(radix)).



% CANONICAL MAPPING %

%! hexBinaryCanonicalMap(
%!   +HexBinary:list(between(0,1)),
%!   -LEX:list(code)
%! ) is det.

hexBinaryCanonicalMap(BinaryDigigts, LEX):-
  phrase(hexBinaryCanonicalMap(BinaryDigits), LEX).

%! hexBinaryCanonicalMap(+HexBinary:list(between(0,1)))//
% Maps a hexBinary value to a literal matching the hexBinary// production.
%
% The sequence of literals formed by applying hexOctetCanonical//
% to each octet in =BinaryDigits=, in order, and concatenating the results.
%
% @param HexBinary A hexBinary value, i.e. a list of 4 binary digits.

hexBinaryCanonicalMap([]) --> [].
hexBinaryCanonicalMap([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8|BDs]) -->
  hexOctetCanonical([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8]),
  hexBinaryCanonicalMap(BDs).

%! hexDigitCanonical(+BinaryDigits:list(between(0,1)))//
% Maps a four-bit sequence to a hexadecimal digit (a literal matching the
% hexDigit// production).
%
% @param BinaryDigits A sequence of four binary digits.

hexDigitCanonical(BinaryDigits) -->
  {digits_to_decimal(BinaryDigits, 2, Weight)},
  hexadecimal_digit(Weight).

%! hexOctetCanonical(+BinaryOctet:list(between(0,1)))//
% Maps a binary octet to a literal matching the hexOctet// production.
%
% @param BinaryOctet A binary octet, i.e. a list of 8 binary digigts.

hexOctetCanonical([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8]) -->
  hexDigitCanonical([BD1,BD2,BD3,BD4]),
  hexDigitCanonical([BD5,BD6,BD7,BD8]).



% LEXICAL MAPPING %

%! hexBinaryLexicalMap(
%!   +LEX:list(code),
%!   -HexBinary:list(between(0,1))
%! ) is nondet.

hexBinaryLexicalMap(LEX, HOs):-
  phrase(hexBinary(HOs), LEX).

%! hexBinary(-HexBinary:list(between(0,1)))//
% Maps a literal matching the hexBinary// production to a sequence of octets
% in the form of a hexBinary value.
%
% ~~~{.ebnf}
% hexBinary ::= hexOctet*
% ~~~
%
% ~~~{.re}
% ([0-9a-fA-F]{2})*
% ~~~
%
% @param HexadecimalBinary A sequence of binary octets in the form of a
%        hexBinary value.

hexBinary([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8|BDs]) -->
  hexOctet([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8]),
  hexBinary(BDs).

%! hexDigit(-HexadecimalDigit:between(0,15))//
% Maps a hexadecimal digit (a character matching the hexDigit// production)
% to a sequence of four binary digits.
%
% ~~~{.ebnf}
% hexDigit ::= [0-9a-fA-F]
% ~~~
%
% @param HexadecimalDigit An integer between 0 and 15 inclusive.

hexDigit([BD1,BD2,BD3,BD4]) -->
  hexadecimal_digit(Weight),
  % Four binary digits represent one hexadecimal digit.
  {decimal_to_digits(Weight, 2, [BD1,BD2,BD3,BD4])}.

%! hexOctet(-HexOctet:list(between(0,1)))//
% Maps a literal matching the hexOctet// production to a single octet.
%
% ~~~{.ebnf}
% hexOctet ::= hexDigit hexDigit
% ~~~
%
% @param A single binary octet, i.e. a list of 8 binary digits.

hexOctet([BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8]) -->
  hexDigit([BD1,BD2,BD3,BD4]),
  hexDigit([BD5,BD6,BD7,BD8]).

