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

# Datatype

A triple consisting of:
  * *|Value space|*
    A set of distinct values.
  * *|Lexical space|*
    A set of lexical representations.
  * *Facets*
    Characterizing properties of the value space, individual values, or the
    lexical space.

# Value space

The set of values for a given datatype.

Each value in the value space of a datatype is denoted by at least one
literal in the lexical space of the same datatype.

Value space definitions:
  * *Intensional*: axiomatically from fundamental notions.
  * *Extensional*: enumeration.
  * *Restriction* of the value space of an already defined datatype.
  * * Combination* of value from different value spaces, according to some
    construction procedure (XMLS list, XMLS union).

# Lexical space

The set of valid literals for a datatype.

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
