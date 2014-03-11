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

:- use_module(dcg(dcg_generic)).
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
  phrase(xsd_boolean_canonical_map(Boolean), Lexical).
xsd_canonical_map(xsd:date, Date, Lexical):- !,
  phrase(xsd_date_canonical_map(Date), Lexical).
xsd_canonical_map(xsd:dateTime, DateTime, Lexical):- !,
  phrase(xsd_dateTime_canonical_map(DateTime), Lexical).
xsd_canonical_map(xsd:decimal, Decimal, Lexical):- !,
  phrase(xsd_decimal_canonical_map(Decimal), Lexical).
xsd_canonical_map(xsd:double, Double, Lexical):- !,
  phrase(xsd_double_canonical_map(Double), Lexical).
xsd_canonical_map(xsd:duration, Duration, Lexical):- !,
  phrase(xsd_duration_canonical_map(Duration), Lexical).
xsd_canonical_map(xsd:float, Float, Lexical):- !,
  phrase(xsd_float_canonical_map(Float), Lexical).
xsd_canonical_map(xsd:gDay, GregorianDay, Lexical):- !,
  phrase(xsd_gDay_canonical_map(GregorianDay), Lexical).
xsd_canonical_map(xsd:gMonth, GregorianMonth, Lexical):- !,
  phrase(xsd_gMonth_canonical_map(GregorianMonth), Lexical).
xsd_canonical_map(xsd:gMonthDay, GregorianMonthDay, Lexical):- !,
  phrase(xsd_gMonthDay_canonical_map(GregorianMonthDay), Lexical).
xsd_canonical_map(xsd:gYear, GregorianYear, Lexical):- !,
  phrase(xsd_gYear_canonical_map(GregorianYear), Lexical).
xsd_canonical_map(xsd:gYearMonth, GregorianYearMonth, Lexical):- !,
  phrase(xsd_gYearMonth_canonical_map(GregorianYearMonth), Lexical).
xsd_canonical_map(xsd:hexBinary, HexBinary, Lexical):- !,
  phrase(xsd_hexBinary_canonical_map(HexBinary), Lexical).
xsd_canonical_map(xsd:integer, Integer, Lexical):- !,
  phrase(xsd_integer_canonical_map(Integer), Lexical).
xsd_canonical_map(xsd:string, String, Lexical):- !,
  phrase(xsd_string_canonical_map(String), Lexical).
xsd_canonical_map(xsd:time, Time, Lexical):- !,
  phrase(xsd_time_canonical_map(Time), Lexical).


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
  dcg_phrase(xsd_boolean_lexical_map(Boolean), Lexical).
xsd_lexical_map_codes(xsd:date, Lexical, Date):- !,
  dcg_phrase(xsd_date_lexical_map(Date), Lexical).
xsd_lexical_map_codes(xsd:dateTime, Lexical, DateTime):- !,
  dcg_phrase(xsd_dateTime_lexical_map(DateTime), Lexical).
xsd_lexical_map_codes(xsd:decimal, Lexical, Decimal):- !,
  dcg_phrase(xsd_decimal_lexical_map(Decimal), Lexical).
xsd_lexical_map_codes(xsd:double, Lexical, Double):- !,
  dcg_phrase(xsd_double_lexical_map(Double), Lexical).
xsd_lexical_map_codes(xsd:duration, Lexical, Duration):- !,
  dcg_phrase(xsd_duration_lexical_map(Duration), Lexical).
xsd_lexical_map_codes(xsd:float, Lexical, Float):- !,
  dcg_phrase(xsd_float_lexical_map(Float), Lexical).
xsd_lexical_map_codes(xsd:gDay, Lexical, GregorianDay):- !,
  dcg_phrase(xsd_gDay_canonical_map(GregorianDay), Lexical).
xsd_lexical_map_codes(xsd:gMonth, Lexical, GregorianMonth):- !,
  dcg_phrase(xsd_gMonth_lexical_map(GregorianMonth), Lexical).
xsd_lexical_map_codes(xsd:gMonthDay, Lexical, GregorianMonthDay):- !,
  dcg_phrase(xsd_gMonthDay_lexical_map(GregorianMonthDay), Lexical).
xsd_lexical_map_codes(xsd:gYear, Lexical, GregorianYear):- !,
  dcg_phrase(xsd_gYear_lexical_map(GregorianYear), Lexical).
xsd_lexical_map_codes(xsd:gYearMonth, Lexical, GregorianYearMonth):- !,
  dcg_phrase(xsd_gYearMonth_lexical_map(GregorianYearMonth), Lexical).
xsd_lexical_map_codes(xsd:hexBinary, Lexical, HexBinary):- !,
  dcg_phrase(xsd_hexBinary_lexical_map(HexBinary), Lexical).
xsd_lexical_map_codes(xsd:integer, Lexical, Integer):- !,
  dcg_phrase(xsd_integer_lexical_map(Integer), Lexical).
xsd_lexical_map_codes(xsd:string, Lexical, String):- !,
  dcg_phrase(xsd_string_lexical_map(String), Lexical).
xsd_lexical_map_codes(xsd:time, Lexical, Time):- !,
  dcg_phrase(xsd_time_lexical_map(Time), Lexical).


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

