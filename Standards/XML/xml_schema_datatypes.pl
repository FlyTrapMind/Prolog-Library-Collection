:- module(
  xml_schema_datatypes,
  [
% GENERIC
    xmls_canonical_value/3, % ?Datatype:uri
                            % ?External
                            % ?Canonical:atom
    xmls_datatype/2, % ?DatatypeName:atom
                     % ?Datatype:uri
% SPECIFIC DATATYPES
    xmls_boolean/2, % ?External:oneof([atom,boolean,between(0,1)])
                    % ?Canonical:atom
    xmls_decimal/2, % ?Decimal:pair(integer,integer)
                    % ?Canonical:atom
    xmls_string/2 % ?External
                  % ?Canonical
  ]
).

/** <module> XML SCHEMA DATATYPES

XML Schema 2: Datatypes (Second Edition)

# Standards version 1.0 or 1.1

When this specification is used to check the datatype validity of XML input,
implementations may provide the heuristic of using the 1.1 datatypes if
the input is labeled as XML 1.1, and using the 1.0 datatypes if the input
is labeled 1.0, but this heuristic should be subject to override by users,
to support cases where users wish to accept XML 1.1 input but validate it
using the 1.0 datatypes, or accept XML 1.0 input and validate it using
the 1.1 datatypes.

# Concepts

  * *|Ineffable value|*
    A value in a value space that is not mapped to by any literal
    from the corresponding lexical space.
  * 

# Datatype

A triple consisting of:
  * *|Value space|*
    A set of distinct values.
    This is only abstractly defined.
    An actual implementation has to implement these values
    (e.g. C, SWI-Prolog).
  * *|Lexical space|*
    A set of lexical representations or literals denoting values.
  * *Facets*
    Characterizing properties of the value space, individual values, or the
    lexical space.
  * *|Functions, relations, operations|*
    * A _|lexical mapping|_ from lexical to value space.
      This mapping is used by me to read values from a specific
      serialization (e.g. RDF-XML) to an implementation-specific
      datatype format (e.g. C).
    * A _|canonical mapping|_ from value to lexical space.
      This mapping is used by me to write implementation-specific (e.g. C)
      values to a specific serialization (e.g. RDF-XML).
    * _|Identity relation|_
    * _|Equality relation|_
    * _|Order relation|_

# Value space

The set of values for a given datatype.

Each value in the value space of a datatype is denoted by at least one
literal in the lexical space of the same datatype.

Value space definitions:
  * *Intensional*
    Axiomatic definition from fundamental notions.
  * *Extensional*
    Enumeration of the values.
  * *Derived*
    Defined by restricting an existing value space.
  * *Combined* out of existing value spaces, according to some
    construction procedure (e.g., list, union).

## Identity

I do not understand what the following means:
"The identity relation is always defined.
Every value space inherently has an identity relation.
Two things are identical if and only if they are actually the same thing:
i.e., if there is no way whatever to tell them apart." [XSD v1.1 sec2.2.1]

Lists =A= and =B= are identical iff they have the same length and
their items are pairwise identical.

Note that all the empty lists are identical.

Values from the value spaces of different primitive datatypes are
never identical.

## Equality

Each primitive datatype has prescribed an equality relation for its
value space.

The equality relation is not always the same as the identity relation.
Examples:
  * Float and double =-0= and =+0= are equal but not identical.
  * Two dateTime values may denote the same moment in time,
    but doing so with different local times and different (correcting)
    time zone offsets.
    These values are equal but not identical.

The equality relation is not always complete.
Example:
  * Float and double =NaN= is not even equal to itself,
    but is identical to itself.

Lists =A= and =B= are equal iff they have the same length and
their items are pairwise identical.

Note that all the empty lists are equal.

A list of length one containing value =V1= is equal to an atomic value =V2=
iff =V1= is equal to =V2=. (I.e., =|[V1] = V2|= iff =|V1 = V2|=.)

Values from the value spaces of different primitive datatypes are
never equal.

## Order

Values =a= and =b= are *incomparable* iff
$a \nleq b \land a \neq b \land a \ngeq b$.
Values are comparable if there are not incomparable.

The *|incomparable relation|* is denoted =|<>|=.

The weak order $\leq$ means $<$ or $=$ and _|one can tell which|_.
For example, the duration =P1M= (one month) is not $\leq$ =P31D=
(thirty-one days) because =P1M= $<$ =P31D= nor =P1M= $=$ =P31D= is the case.
Instead, =P1M= is incomparable with =P31D=.

Values from the value spaces of different primitive datatypes are always
incomparable.

# Lexical space

The set of lexica representations or literals that represent the values
of the value space of a datatype.

Characteristics:
  * Interoperability: minimum number of literals for the same value.
  * Readability: non-binary; text.
  * Parsing and serialization: taken from common languages and libraries.

## Canonical lexical representation

A subset of the lexical space for which there is a one-to-one mapping to
the value space.

# Facet

A single defining aspect of a value space.

## Fundamental facet

...

## Constraining facet

...

--

@author Wouter Beek
@compat XML Schema 2: Datatypes (Second Edition)
@see http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/
@version 2013/01, 2013/03-2013/05, 2013/07-2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_datatypes)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xmls_datetime)).

:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- discontiguous(xmls_canonical_value/3).

:- rdf_meta(xmls_canonical_value(r,?,?)).
:- rdf_meta(xmls_datatype(?,r)).
:- rdf_meta(xmls_datatype_(?,r)).



%! xmls_boolean(?Lexical, ?Canonical:atom)
% Boolean has the value space required to support the mathematical concept
% of binary-valued logic: =|{true, false}|=.
%
% Lexical representation: the literals =true=, =false=, =1=, and =0=.
%
% Canonical representation: the literals =true= and =false=.

% Standards compliant
xmls_boolean('0',   'false').
xmls_boolean('1',   'true' ).
xmls_boolean(false, 'false').
xmls_boolean(true,  'true' ).
% Prolog numberic variants of labels.
xmls_boolean(0,     'false').
xmls_boolean(1,     'true' ).
% Prolog mnemonic for false.
xmls_boolean(fail,  'false').

xml_chars -->
  [].
xml_chars -->
  xml_char(_),
  xml_chars.

xmls_canonical_value(xsd:boolean, L, C):-
  xmls_boolean(L, C).
xmls_canonical_value(xsd:decimal, L, C):-
  xmls_decimal(L, C).
xmls_canonical_value(xsd:duration, L, C):-
  xmls_duration(L, C).
xmls_canonical_value(xsd:string, L, C):-
  xmls_string(L, C).

%! xmls_decimal(?Lexical, ?Canonical:atom)
% Decimal represents a subset of the real numbers, which can be represented
% by decimal numerals. The value space of decimal is the set of numbers that
% can be obtained by multiplying an integer by a non-positive power of ten,
% i.e., expressible as =|i × 10^-n|= where =i= and =n= are integers and
% =|n >= 0|=. Precision is not reflected in this value space; the number
% =|2.0|= is not distinct from the number =|2.00|=. The order-relation
% on decimal is the order relation on real numbers, restricted to this subset.
%
% All minimally conforming processors must support decimal numbers with
% a minimum of =18= decimal digits (i.e., with a =totalDigits= of =18=).
% However, minimally conforming processors may set an application-defined
% limit on the maximum number of decimal digits they are prepared to support,
% in which case that application-defined maximum number must be clearly
% documented.
%
% Lexical representation: finite-length sequences of decimal digits
% (=|#x30-#x39|=) separated by a period as a decimal indicator. An optional
% leading sign is allowed. If the sign is omitted, "=|+|=" is assumed.
% Leading and trailing zeroes are optional.
% If the fractional part is zero, the period and following zero(es)
% can be omitted.
% For example: =|-1.23|=, =|12678967.543233|=, =|+100000.00|=, =210=.
%
% Canonical representation: prohibiting certain options from
% the lexical representation. Specifically, the preceding optional
% "=|+|=" sign is prohibited. The decimal point is required.
% Leading and trailing zeroes are prohibited subject to the following:
% there must be at least one digit to the right and to the left of
% the decimal point which may be a zero.
%
% @arg Decimal A pair of the form =|Value-Power|=,
%      where =|Decimal = Value * 10 ** -Power|=
%      and =Value= and =Power= are integers.

xmls_decimal(L, C):-
  dcg_phrase(xmls_decimal_lexical(L), C).

%! xmls_decimal_lexical(?Decimal)//
% @arg Decimal A pair of the form =|Value-Power|=,
%      where =|Decimal = Value * 10 ** -Power|=
%      and =Value= and =Power= are integers.

xmls_decimal_lexical(V-P) -->
  {var(V), var(P)},
  (minus_sign -> {Sign = -1} ; ("" ; plus_sign), {Sign = 1}),
  dcg_multi(decimal_digit, _, DIs),
  (
    dot,
    dcg_multi(decimal_digit, P, DFs)
  ;
    {P = 0, DFs = []}
  ),
  {
    append(DIs, DFs, Ds),
    digits_to_decimal(Ds, UnsignedV),
    V is Sign * UnsignedV
  }.
xmls_decimal_lexical(V-P) -->
  {
    number_length(V, L),
    IL is L - P,
    number_to_digits(V, Ds),
    length(DIs, IL),
    append(DIs, DFs, Ds)
  },
  % The plus sign is optional.
  ({V < 0} -> minus_sign ; ("" ; plus_sign)),
  dcg_multi(decimal_digit, IL, DIs),
  (
    {P = 0}
  ;
    dot,
    dcg_multi(decimal_digit, P, DFs)
  ).

%! xmls_string(?External, ?Canonical:atom)
% The string datatype represents character strings in XML.
% The value space of string is the set of finite-length sequences
% of characters that match the xml_char//1 production from.
% A character is an atomic unit of communication; it is not further
% specified except to note that every character has a corresponding
% Universal Character Set code point, which is an integer.
%
% Many human languages have writing systems that require child elements
% for control of aspects such as bidirectional formating or ruby annotation
% Thus, string, as a simple type that can contain only characters but not
% child elements, is often not suitable for representing text.
% In such situations, a complex type that allows mixed content should be
% considered.
%
% The fact that this specification does not specify an order-relation
% for string does not preclude other applications from treating strings
% as being ordered.
%
% @compat Uses XML characters as defined in _|XML 1.0 5th Edition|_,
%         even though _|XML Schema 2: Datatypes 2nd Edition|_
%         uses the XML character definition from _|XML 1.0 2nd Edition|_.

xmls_string(String, String):-
  phrase(xml_chars, String).

% Date
xmls_datatype_(date, Date, xsd:date, Atom):-
  (
    nonvar(Date)
  ->
    xmls_datatype_check(xsd:date, Date),
    format_time(atom(Atom), '%F%:z', Date)
  ;
    nonvar(Atom)
  ->
    parse_time(Atom, iso_8601, Date)
  ).
% Date & time
xmls_datatype_(dateTime, DateTime, xsd:dateTime, Atom):-
  (
    nonvar(DateTime)
  ->
    xmls_datatype_check(xsd:dateTime, DateTime),
    format_time(atom(Atom), '%FT%T%:z', DateTime)
  ;
    nonvar(Atom)
  ->
    parse_time(Atom, iso_8601, DateTime)
  ).
% Double
xmls_datatype_(double, Double, xsd:double, Atom):-
  (
    nonvar(Double)
  ->
    atom_number(Atom, Double)
  ;
    atom_number(Atom, Number),
    Double is float(Number)
  ),
  xmls_datatype_check(xsd:double, Double).
% Float
xmls_datatype_(float, Float, xsd:float, Atom):-
  (
    nonvar(Float)
  ->
    float(Float),
    atom_number(Atom, Float)
  ;
    atom_number(Atom, Number),
    Float is float(Number)
  ),
  xmls_datatype_check(xsd:float, Float).
% Day
xmls_datatype_(gDay, Day, xsd:gDay, Atom):-
  atom_number(Atom, Day),
  xmls_datatype_check(xsd:gDay, Day).
% Month
xmls_datatype_(gMonth, Month, xsd:gMonth, Atom):-
  atom_number(Atom, Month),
  xmls_datatype_check(xsd:gMonth, Month).
% Year
xmls_datatype_(gYear, Year, xsd:gYear, Atom):-
  atom_number(Atom, Year),
  xmls_datatype_check(xsd:gYear, Year).
% Integer
xmls_datatype_(int, Integer, xsd:int, Atom):-
  atom_number(Atom, Integer),
  xmls_datatype_check(xsd:int, Integer).
% String
xmls_datatype_(string, String, xsd:string, Atom):-
  (
    nonvar(String)
  ->
    term_to_atom(String, Atom)
  ;
    nonvar(Atom)
  ->
    String = Atom
  ).

%! xmls_datatype_check(+Datatype:uri, +Value) is semidet.
% Succeeds if the given value is of the given XML Schema datatype.

xmls_datatype_check(xsd:boolean, Boolean):-
  memberchk(Boolean, [0, 1, false, true]), !.
xmls_datatype_check(Datatype, DateTime):-
  compound(DateTime),
  rdf_memberchk(Datatype, [xsd:date, xsd:dateTime]),
  date_time_stamp(DateTime, _TimeStamp), !.
xmls_datatype_check(Datatype, TimeStamp):-
  rdf_memberchk(Datatype, [xsd:date, xsd:dateTime]),
  stamp_date_time(TimeStamp, _DateTime, 'UTC'), !.
xmls_datatype_check(xsd:double, Double):-
  float(Double), !.
xmls_datatype_check(xsd:float, Float):-
  float(Float), !.
xmls_datatype_check(xsd:gDay, Day):-
  integer(Day),
  between(1, 31, Day), !.
xmls_datatype_check(xsd:gMonth, Month):-
  integer(Month),
  between(1, 12, Month), !.
xmls_datatype_check(xsd:gYear, Year):-
  integer(Year), !.
xmls_datatype_check(xsd:int, Integer):-
  integer(Integer),
  (
    current_prolog_flag(bounded, true)
  ->
    current_prolog_flag(max_integer, Max),
    Integer =< Max,
    current_prolog_flag(min_integer, Min),
    Integer >= Min
  ;
    true
  ).

%! xmls_datatype(?DatatypeName:atom, ?Datatype:uri) is nondet.
% Translations between datatype names and datatype URIs.
%
% @arg DatatypeName The atomic name of an XML Schema datatype.
% @arg Datatype The URI of an XML Schema datatype.

xmls_datatype(DatatypeName, Datatype):-
  var(DatatypeName), var(Datatype), !,
  xmls_datatype_(DatatypeName, Datatype).
% The mapping between datatypes and datatype names is unique.
xmls_datatype(DatatypeName, Datatype):-
  once(xmls_datatype_(DatatypeName, Datatype)).

xmls_datatype_(boolean,  xsd:boolean ).
xmls_datatype_(date,     xsd:date    ).
xmls_datatype_(dateTime, xsd:dateTime).
xmls_datatype_(decimal,  xsd:decimal ).
xmls_datatype_(double,   xsd:double  ).
xmls_datatype_(float,    xsd:float   ).
xmls_datatype_(gDay,     xsd:gDay    ).
xmls_datatype_(gMonth,   xsd:gMonth  ).
xmls_datatype_(gYear,    xsd:gYear   ).
xmls_datatype_(int,      xsd:int     ).
xmls_datatype_(string,   xsd:string  ).
