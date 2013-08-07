:- module(
  xsd_string,
  [
    stringCanonicalMap/2, % +String:atom
                          % -StringRep:list(code)
    stringLexicalMap/2 % ?LEX:list(code)
                       % ?String:atom
  ]
).

/** <module> SXD_STRING

The *|=string= datatype|* represents character strings in XML.

#### Value space

The value space of =string= is the set of finite-length sequences
of characters that match the xml_char//1 production from either XML 1.0
or XML 1.1.

A character is an atomic unit of communication; it is not further
specified except to note that every character has a corresponding
Universal Character Set code point, which is an integer.

Many human languages have writing systems that require child elements
for control of aspects such as bidirectional formating or ruby annotation
Thus, string, as a simple type that can contain only characters but not
child elements, is often not suitable for representing text.
In such situations, a complex type that allows mixed content should be
considered.

The fact that this specification does not specify an order-relation
for strings does not preclude other applications from treating strings
as being ordered.

#### Lexical mapping

The lexical space of =string= is the set of finite-length sequences of
zero or more characters (as defined in [XML]) that match the xml_char//
production from XML 1.0 or XML 1.1.

~~~{.ebnf}
stringRep ::= Char*
~~~

The lexical mapping for =string= is stringLexicalMap/2.
The canonical mapping·for =string= is stringCanonicalMap/2.

#### Factes

Datatypes derived by restriction from =string= can specify the following
constraining facets:
  * =assertions=
  * =enumeration=
  * =length=, =minLength=, =maxLength=
  * =pattern=
  * =|whitespace = preserved|=

Values for the fundamental facets:
  * =|ordered = false|=
  * =|bounded = false|=
  * =|cardinality = countably infinite|=
  * =|numeric = false|=

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(xml(xml_datatypes)).



%! stringCanonicalMap(+String:atom, -StringRep:list(code)) is det.
% Maps a string value to a stringRep//1.
%
% The function is the identity function on the domain.
%
% @param String An XML string value; a Prolog atom.
% @param StringRep The canonical XML string serialization. A list of codes.

stringCanonicalMap(String, LEX):-
  phrase(stringRep(String), LEX).

%! stringLexicalMap(?LEX:list(code), ?String:atom) is det.
% Maps a literal matching the stringRep//1 production to a string value.
%
% The function is the identity function on the domain.
%
% @param Literal A literal matching stringRep//1.
% @param String An XML string value; a Prolog atom.

stringLexicalMap(LEX, String):-
  phrase(stringRep(String), LEX).

%! stringRep(?String:atom)//

stringRep(String) -->
  {var(String)}, !,
  xml_chars(XML_Characters),
  {atom_codes(String, XML_Characters)}.
stringRep(String) -->
  {atom_codes(String, XML_Characters)},
  xml_chars(XML_Characters).

