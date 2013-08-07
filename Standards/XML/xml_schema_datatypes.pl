:- module(
  xml_schema_datatypes,
  [
    xmls_datatype/2 % ?DatatypeName:atom
                    % ?Datatype:uri
  ]
).

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_datatypes)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xmls_datetime)).

:- rdf_meta(xmls_datatype(?,r)).
:- rdf_meta(xmls_datatype_(?,r)).



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
% @param DatatypeName The atomic name of an XML Schema datatype.
% @param Datatype The URI of an XML Schema datatype.

xmls_datatype(DatatypeName, Datatype):-
  var(DatatypeName), var(Datatype), !,
  xmls_datatype_(DatatypeName, Datatype).
% The mapping between datatypes and datatype names is unique.
xmls_datatype(DatatypeName, Datatype):-
  once(xmls_datatype_(DatatypeName, Datatype)).

