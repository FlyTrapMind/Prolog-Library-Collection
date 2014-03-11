:- module(
  xsd,
  [
    xsd_canonical_map/3, % +Datatype:iri
                         % +Value
                         % -LexicalExpression:list(code)
    xsd_datatype/1, % ?Datatype:iri
    xsd_datatype/2, % ?Name:atom
                    % ?Datatype:iri
    xsd_lexical_map/3, % +Datatype:iri
                       % +Lexical:or([atom,list(code)])
                       % ?Value
    xsd_order/2 % +Datatype:iri
                % :LEQ
  ]
).

/** <module> XML Schema 2: Datatypes

Support for XML Scheme 2 datatypes,
conforming to recommendation version 1.1.

@author Wouter Beek
@compat XML Schema 2: Datatypes (Second Edition)
@see Canonical map for double does not work (loops on log value =:= 0.
@see http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/
@see Turn the infinite datatype requirements into a unit test.
@tbd Implement =base64Binary=.
@tbd Implement =anyURI=.
@tbd Implement =QNAME=.
@tbd Implement =NOTATION=.
@tbd Implement the non-primitive built-in atomic and list datatypes.
@tbd Read section 4: Datatype components.
@tbd Read section 5: Conformance.
@tbd Read section E.3.3 on adding durations to dateTime.
@tbd Read section G on REs.
@tbd Read section H on implementation-defined datatypes.
@version 2013/08-2013/10, 2014/01, 2014/03
*/

:- use_module(generics(codes_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % RDF-meta assertions.
:- use_module(rdf(rdf_read)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_boolean)).
:- use_module(xsd(xsd_date)).
:- use_module(xsd(xsd_dateTime)).
:- use_module(xsd(xsd_decimal)).
:- use_module(xsd(xsd_duration)).
:- use_module(xsd(xsd_float)).
:- use_module(xsd(xsd_gDay)).
:- use_module(xsd(xsd_gMonth)).
:- use_module(xsd(xsd_gMonthDay)).
:- use_module(xsd(xsd_gYear)).
:- use_module(xsd(xsd_gYearMonth)).
:- use_module(xsd(xsd_hexBinary)).
:- use_module(xsd(xsd_integer)).
:- use_module(xsd(xsd_string)).
:- use_module(xsd(xsd_time)).

:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- rdf_meta(xsd_canonical_map(r,+,-)).
:- rdf_meta(xsd_datatype(r)).
:- rdf_meta(xsd_datatype(+,r)).
:- rdf_meta(xsd_lexical_map(r,+,?)).



%! xsd_canonical_map(
%!   +Datatype:iri,
%!   +Value,
%!   -LexicalExpression:list(code)
%! ) is det.

xsd_canonical_map(xsd:boolean, Boolean, Lexical):- !,
  xsd_boolean_canonical_map(Boolean, Lexical).
xsd_canonical_map(xsd:date, Date, Lexical):- !,
  xsd_date_canonical_map(Date, Lexical).
xsd_canonical_map(xsd:dateTime, DateTime, Lexical):- !,
  xsd_dateTime_canonical_map(DateTime, Lexical).
xsd_canonical_map(xsd:decimal, Decimal, Lexical):- !,
  decimalCanonicalMap(Decimal, Lexical).
xsd_canonical_map(xsd:double, Double, Lexical):- !,
  doubleCanonicalMap(Double, Lexical).
xsd_canonical_map(xsd:duration, Duration, Lexical):- !,
  xsd_duration_canonical_map(Duration, Lexical).
xsd_canonical_map(xsd:float, Float, Lexical):- !,
  floatCanonicalMap(Float, Lexical).
xsd_canonical_map(xsd:gDay, GregorianDay, Lexical):- !,
  gDayCanonicalMap(GregorianDay, Lexical).
xsd_canonical_map(xsd:gMonth, GregorianMonth, Lexical):- !,
  gMonthCanonicalMap(GregorianMonth, Lexical).
xsd_canonical_map(xsd:gMonthDay, GregorianMonthDay, Lexical):- !,
  gMonthDayCanonicalMap(GregorianMonthDay, Lexical).
xsd_canonical_map(xsd:gYear, GregorianYear, Lexical):- !,
  gYearCanonicalMap(GregorianYear, Lexical).
xsd_canonical_map(xsd:gYearMonth, GregorianYearMonth, Lexical):- !,
  gYearMonthCanonicalMap(GregorianYearMonth, Lexical).
xsd_canonical_map(xsd:hexBinary, HexBinary, Lexical):- !,
  hexBinaryCanonicalMap(HexBinary, Lexical).
xsd_canonical_map(xsd:integer, Integer, Lexical):- !,
  integerCanonicalMap(Integer, Lexical).
xsd_canonical_map(xsd:string, String, Lexical):- !,
  stringCanonicalMap(String, Lexical).
xsd_canonical_map(xsd:time, Time, Lexical):- !,
  timeCanonicalMap(Time, Lexical).


%! xsd_datatype(+Datatype:iri) is semidet.
%! xsd_datatype(-Datatype:iri) is nondet.
%! xsd_datatype(+Name:atom, +Datatype:iri) is semidet.
%! xsd_datatype(+Name:atom, -Datatype:iri) is det.
%! xsd_datatype(-Name:atom, +Datatype:iri) is det.
%! xsd_datatype(-Name:atom, -Datatype:iri) is nondet.

xsd_datatype(Datatype):-
  xsd_datatype(_, Datatype).

xsd_datatype(boolean,    xsd:boolean   ).
xsd_datatype(date,       xsd:date      ).
xsd_datatype(dateTime,   xsd:dateTime  ).
xsd_datatype(decimal,    xsd:decimal   ).
xsd_datatype(double,     xsd:double    ).
xsd_datatype(duration,   xsd:duration  ).
xsd_datatype(float,      xsd:float     ).
xsd_datatype(gDay,       xsd:gDay      ).
xsd_datatype(gMonth,     xsd:gMonth    ).
xsd_datatype(gMonthDay,  xsd:gMonthDay ).
xsd_datatype(gYear,      xsd:gYear     ).
xsd_datatype(gYearMonth, xsd:gYearMonth).
xsd_datatype(hexBinary,  xsd:hexBinary ).
xsd_datatype(integer,    xsd:integer   ).
xsd_datatype(string,     xsd:string    ).
xsd_datatype(time,       xsd:time      ).


%! xsd_lexical_map(
%!   +Datatype:iri,
%!   +Lexical:or([atom,list(code)]),
%!   +Value
%! ) is semidet.
%! xsd_lexical_map(
%!   +Datatype:iri,
%!   +Lexical:or([atom,list(code)]),
%!   -Value
%! ) is det.

xsd_lexical_map(Datatype, Lexical, Value):-
  atomic_codes_goal(xsd_lexical_map_codes(Datatype), Lexical, Value).

xsd_lexical_map_codes(xsd:boolean, Lexical, Boolean):- !,
  xsd_boolean_lexical_map(Lexical, Boolean).
xsd_lexical_map_codes(xsd:date, Lexical, Date):- !,
  xsd_date_lexical_map(Lexical, Date).
xsd_lexical_map_codes(xsd:dateTime, Lexical, DateTime):- !,
  xsd_dateTime_lexical_map(Lexical, DateTime).
xsd_lexical_map_codes(xsd:decimal, Lexical, Decimal):- !,
  decimalLexicalMap(Lexical, Decimal).
xsd_lexical_map_codes(xsd:double, Lexical, Double):- !,
  doubleLexicalMap(Lexical, Double).
xsd_lexical_map_codes(xsd:duration, Lexical, Duration):- !,
  xsd_duration_lexical_map(Lexical, Duration).
xsd_lexical_map_codes(xsd:float, Lexical, Float):- !,
  floatLexicalMap2(Lexical, Float).
xsd_lexical_map_codes(xsd:gDay, Lexical, GregorianDay):- !,
  gDayLexicalMap(Lexical, GregorianDay).
xsd_lexical_map_codes(xsd:gMonth, Lexical, GregorianMonth):- !,
  gMonthLexicalMap(Lexical, GregorianMonth).
xsd_lexical_map_codes(xsd:gMonthDay, Lexical, GregorianMonthDay):- !,
  gMonthDayLexicalMap(Lexical, GregorianMonthDay).
xsd_lexical_map_codes(xsd:gYear, Lexical, GregorianYear):- !,
  gYearLexicalMap(Lexical, GregorianYear).
xsd_lexical_map_codes(xsd:gYearMonth, Lexical, GregorianYearMonth):- !,
  gYearMonthLexicalMap(Lexical, GregorianYearMonth).
xsd_lexical_map_codes(xsd:hexBinary, Lexical, HexBinary):- !,
  hexBinaryLexicalMap(Lexical, HexBinary).
xsd_lexical_map_codes(xsd:integer, Lexical, Integer):- !,
  integerLexicalMap(Lexical, Integer).
xsd_lexical_map_codes(xsd:string, Lexical, String):- !,
  stringLexicalMap(Lexical, String).
xsd_lexical_map_codes(xsd:time, Lexical, Time):- !,
  timeLexicalMap(Lexical, Time).


%! xsd_order(+Datatype:iri, :LEQ) is det.

xsd_order(D1, xsd_dateTime_leq):-
  rdf_global_id(D1, D2),
  rdf_memberchk(
    D2,
    [
      xsd:date,
      xsd:dateTime,
      xsd:gDay,
      xsd:gMonth,
      xsd:gMonthDay,
      xsd:gYear,
      xsd:gYearMonth
    ]
  ), !.
xsd_order(D1, =<):-
  rdf_global_id(D1, D2),
  rdf_memberchk(D2, [xsd:decimal,xsd:double,xsd:float,xsd:integer]).

