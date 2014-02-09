:- module(
  xsd,
  [
    xsd_canonicalMap/3, % +Datatype:iri
                        % +Value
                        % -LexicalExpression:list(code)
    xsd_canonize_graph/1, % +Graph:atom
    xsd_convert_datatype/4, % +FromDatatype:iri
                            % +FromLexicalExpression:list(code)
                            % +ToDatatype:iri
                            % -ToLexicalExpression:list(code)
    xsd_datatype/2, % ?Name:atom
                    % ?Datatype:iri
    xsd_lexicalCanonicalMap/3, % +Datatype:iri
                               % +LexicalExpression:list(code)
                               % -CanonicalLiteral:atom
    xsd_lexicalMap/3, % +Datatype:iri
                      % +LexicalExpression:list(code)
                      % ?Value
    xsd_order/2 % +Datatype:iri
                % :LEQ
  ]
).
:- reexport(
  xsd(xsd_dateTime),
  [
    dateTime_leq/2
  ]
).

/** <module> XML Schema 2: Datatypes

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
@version 2013/08-2013/10, 2014/01
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

:- rdf_meta(xsd_canonicalMap(r,+,-)).
:- rdf_meta(xsd_canonicalMap_(r,+,-)).
:- rdf_meta(xsd_convert_datatype(r,+,r,-)).
:- rdf_meta(xsd_datatype(+,r)).
:- rdf_meta(xsd_lexicalCanonicalMap(r,+,-)).
:- rdf_meta(xsd_lexicalMap(r,+,-)).
:- rdf_meta(xsd_lexicalMap_(r,+,-)).



%! xsd_canonicalMap(
%!   +Datatype:iri,
%!   +Value,
%!   -LexicalExpression:list(code)
%! ) is det.

xsd_canonicalMap(Datatype, Double, LEX):-
  rdf_global_id(xsd:double, Datatype), !,
  number_codes(Double, LEX).
xsd_canonicalMap(Datatype, Value, LEX):-
  once(xsd_canonicalMap_(Datatype, Value, LEX)).

xsd_canonicalMap_(xsd:boolean, Boolean, LEX):- !,
  booleanCanonicalMap(Boolean, LEX).
xsd_canonicalMap_(xsd:date, Date, LEX):- !,
  dateCanonicalMap(Date, LEX).
xsd_canonicalMap_(xsd:dateTime, DateTime, LEX):- !,
  dateTimeCanonicalMap(DateTime, LEX).
xsd_canonicalMap_(xsd:decimal, Decimal, LEX):- !,
  decimalCanonicalMap(Decimal, LEX).
xsd_canonicalMap_(xsd:double, Double, LEX):- !,
  doubleCanonicalMap(Double, LEX).
xsd_canonicalMap_(xsd:duration, Duration, LEX):- !,
  durationCanonicalMap(Duration, LEX).
xsd_canonicalMap_(xsd:float, Float, LEX):- !,
  floatCanonicalMap(Float, LEX).
xsd_canonicalMap_(xsd:gDay, GregorianDay, LEX):- !,
  gDayCanonicalMap(GregorianDay, LEX).
xsd_canonicalMap_(xsd:gMonth, GregorianMonth, LEX):- !,
  gMonthCanonicalMap(GregorianMonth, LEX).
xsd_canonicalMap_(xsd:gMonthDay, GregorianMonthDay, LEX):- !,
  gMonthDayCanonicalMap(GregorianMonthDay, LEX).
xsd_canonicalMap_(xsd:gYear, GregorianYear, LEX):- !,
  gYearCanonicalMap(GregorianYear, LEX).
xsd_canonicalMap_(xsd:gYearMonth, GregorianYearMonth, LEX):- !,
  gYearMonthCanonicalMap(GregorianYearMonth, LEX).
xsd_canonicalMap_(xsd:hexBinary, HexBinary, LEX):- !,
  hexBinaryCanonicalMap(HexBinary, LEX).
xsd_canonicalMap_(xsd:integer, Integer, LEX):- !,
  integerCanonicalMap(Integer, LEX).
xsd_canonicalMap_(xsd:string, String, LEX):- !,
  stringCanonicalMap(String, LEX).
xsd_canonicalMap_(xsd:time, Time, LEX):- !,
  timeCanonicalMap(Time, LEX).
xsd_canonicalMap_(Datatype, Value, _LEX):- !,
  debug(
    xsd,
    'There is no canonical map for value ~w of datatype ~w.',
    [Value,Datatype]
  ),
  fail.


%! xsd_canonize_graph(+Graph:atom) is det.
% Make sure all typed literals in the graph with the given name
% have a lexical value that is a canonical value for its datatype.
%
% @arg Graph The atomic name of an RDF graph.

xsd_canonize_graph(G):-
  forall(
    (
      % For every RDF triple that contains a typed literal ...
      rdf(S, P, literal(type(Datatype,Value)), G),

      % Check whether the datatypes that occur in the graph
      % are all covered by this module.
      (
        xsd_datatype(DatatypeName, Datatype), !
      ;
        debug(xsd, 'Unrecognized datatype: ~w.', [Datatype]),
        DatatypeName = unknown
      ),

      % Convert from lexical to value,
      % and then from value to canonical lexical.
      to_codes(Value, LexicalExpression),
      xsd_lexicalCanonicalMap(
        Datatype,
        LexicalExpression,
        CanonicalLexicalExpression
      ),

      % Only changes need to be written.
      \+ atom_codes(LexicalExpression, CanonicalLexicalExpression)
    ),
    (
      rdf_retractall(S, P, literal(type(Datatype,LexicalExpression)), G),
      rdf_assert(S, P, literal(type(Datatype,CanonicalLexicalExpression)), G),
      debug(
        xsd,
        'Canonized datatype ~w: "~w" -> "~w"',
        [DatatypeName,LexicalExpression,CanonicalLexicalExpression]
      )
    )
  ).


%! xsd_convert_datatype(
%!   +FromDatatype:uri,
%!   +FromLexicalExpression:list(code),
%!   +ToDatatype:uri,
%!   -ToLexicalExpression:list(code)
%! ) is det.

xsd_convert_datatype(FromDatatype, FromLEX, ToDatatype, ToLEX):-
  xsd_lexicalMap(FromDatatype, FromLEX, Value),
  xsd_canonicalMap(ToDatatype, Value, ToLEX).


%! xsd_datatype(?Name:atom, ?Datatype:iri) is nondet.

xsd_datatype(boolean, xsd:boolean).
xsd_datatype(date, xsd:date).
xsd_datatype(dateTime, xsd:dateTime).
xsd_datatype(decimal, xsd:decimal).
xsd_datatype(double, xsd:double).
xsd_datatype(duration, xsd:duration).
xsd_datatype(float, xsd:float).
xsd_datatype(gDay, xsd:gDay).
xsd_datatype(gMonth, xsd:gMonth).
xsd_datatype(gMonthDay, xsd:gMonthDay).
xsd_datatype(gYear, xsd:gYear).
xsd_datatype(gYearMonth, xsd:gYearMonth).
xsd_datatype(hexBinary, xsd:hexBinary).
xsd_datatype(integer, xsd:integer).
xsd_datatype(string, xsd:string).
xsd_datatype(time, xsd:time).


%! xsd_lexicalCanonicalMap(
%!   +Datatype:iri,
%!   +LexicalExpression:list(code),
%!   -CanonicalLexicalExpression:list(code)
%! ) is det.
% Reads an XSD 1.1 datatype value and writes it into its canonical form.

xsd_lexicalCanonicalMap(Datatype, LEX, CanonicalLEX):-
  xsd_convert_datatype(Datatype, LEX, Datatype, CanonicalLEX).


%! xsd_lexicalMap(
%!   +Datatype:iri,
%!   +LexicalExpression:list(code),
%!   -Value
%! ) is nondet.
%
% @tbd rdf_meta/1 directive does not work for the Datatype parameter!

xsd_lexicalMap(Datatype1, LEX, Value):-
  rdf_global_id(Datatype1, Datatype2),
  xsd_lexicalMap_(Datatype2, LEX, Value).

xsd_lexicalMap_(xsd:boolean, LEX, Boolean):-
  booleanLexicalMap(LEX, Boolean).
xsd_lexicalMap_(xsd:date, LEX, Date):-
  dateLexicalMap(LEX, Date).
xsd_lexicalMap_(xsd:dateTime, LEX, DateTime):-
  dateTimeLexicalMap(LEX, DateTime).
xsd_lexicalMap_(xsd:decimal, LEX, Decimal):-
  decimalLexicalMap(LEX, Decimal).
xsd_lexicalMap_(xsd:double, LEX, Double):-
  doubleLexicalMap(LEX, Double).
xsd_lexicalMap_(xsd:duration, LEX, Duration):-
  durationLexicalMap(LEX, Duration).
xsd_lexicalMap_(xsd:float, LEX, Float):-
  floatLexicalMap(LEX, Float).
xsd_lexicalMap_(xsd:gDay, LEX, GregorianDay):-
  gDayLexicalMap(LEX, GregorianDay).
xsd_lexicalMap_(xsd:gMonth, LEX, GregorianMonth):-
  gMonthLexicalMap(LEX, GregorianMonth).
xsd_lexicalMap_(xsd:gMonthDay, LEX, GregorianMonthDay):-
  gMonthDayLexicalMap(LEX, GregorianMonthDay).
xsd_lexicalMap_(xsd:gYear, LEX, GregorianYear):-
  gYearLexicalMap(LEX, GregorianYear).
xsd_lexicalMap_(xsd:gYearMonth, LEX, GregorianYearMonth):-
  gYearMonthLexicalMap(LEX, GregorianYearMonth).
xsd_lexicalMap_(xsd:hexBinary, LEX, HexBinary):-
  hexBinaryLexicalMap(LEX, HexBinary).
xsd_lexicalMap_(xsd:integer, LEX, Integer):-
  integerLexicalMap(LEX, Integer).
xsd_lexicalMap_(xsd:string, LEX, String):-
  stringLexicalMap(LEX, String).
xsd_lexicalMap_(xsd:time, LEX, Time):-
  timeLexicalMap(LEX, Time).
xsd_lexicalMap_(Datatype, _LEX, _Value):-
  debug(xsd, 'There is no lexical mapping for datatype ~w.', [Datatype]),
  fail.


%! xsd_order(+Datatype:iri, :LEQ) is det.

xsd_order(D1, dateTime_leq):-
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

