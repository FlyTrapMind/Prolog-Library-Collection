:- module(xml_std, []).

/** <module> XML_STD

Standards support for XML.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(std, 'http://www.example.org/standards/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').
% XML Scheme namespace for instances.
:- xml_register_namespace(xsi, 'http://www.w3.org/2001/XMLSchema-instance#').

:- initialitation(init_xml_std).



init_xml_std:-
  standards_graph(G),
  init_xml(G),
  init_xml_schema(G),
  init_xml_schema_datatypes(G),
  init_xml_scheme_primer(G).

init_xml(G):-
  rdfs_assert_class(w3c:'Recommendation', G),
  % Atoms that are used multiple times.
  rdf_global_id(w3c:'XML/Core/', XMLWG),
  rdf_global_id(w3c:'TR/2008/REC-xml-20081126/', This),

  % XML Working Group
  rdfs_assert_label(XMLWG, 'XML Core Working Group', G),

  % XML Recommendation
  rdf_assert_individual(This, w3c:'Recommendation', G),
  rdf_assert_datatype(This, w3c:year, gYear, 2008, G),
  rdf_assert_literal(
    This,
    w3c:title,
    'Extensible Markup Language (XML) 1.0 (Fifth Edition)',
    G
  ),
  rdf_assert(This, w3c:developed_by, XMLWG, G),
  rdf_assert_literal(This, w3c:author, 'Tim Bray', G),
  rdf_assert_literal(This, w3c:author, 'Jean Paoli', G),
  rdf_assert_literal(This, w3c:author, 'C. M. Sperberg-McQueen', G),
  rdf_assert_literal(This, w3c:author, 'Eve Maler', G),
  rdf_assert_literal(This, w3c:author, 'François Yergeau', G),
  rdf_assert(This, w3c:supercedes, w3c:'TR/2006/REC-xml-20060816/', G),
  % SGML
  rdf_assert(This, w3c:mentions, iso:'8879', G),
  % Characters
  rdf_assert(This, w3c:requires, iso:'10646', G),
  rdf_assert(This, w3c:requires, std:'BCP47', G),
  % Language identification tags
  rdf_assert(This, w3c:requires, std:'IANA-LANGCODES', G),
  % Unicode
  rdf_assert(This, w3c:requires, std:'Unicode', G).

init_xml_schema(G):-
  rdf_global_id(w3c:'TR/2012/REC-xmlschema11-1-20120405/', This),
  rdfs_assert_class(w3c:'Recommendation', G).
  rdf_assert_individual(This, w3c:'Recommendation', G),
  rdf_assert_datatype(This, w3c:year, gYear, 2012, G),
  rdf_assert_literal(
    This,
    std:title,
    'XML Schema Definition Language (XSD) 1.1 Part 1: Structures',
    G
  ),
  rdf_assert_literal(This, w3c:author, 'Shudi (Sandy) Gao 高殊镝', G),
  rdf_assert_literal(This, w3c:author, 'C. M. Sperberg-McQueen', G),
  rdf_assert_literal(This, w3c:author, 'Henry S. Thompson', G),
  % XML Schema Definition Language 1.1 Part 2: Datatypes
  rdf_assert(This, w3c:depends_on, w3c:'???', G).

init_xml_schema_datatypes(G):-
  rdf_global_id(w3c:'TR/2004/REC-xmlschema-2-20041028/', This),
  rdfs_assert_class(w3c:'Recommendation', G),
  rdf_assert_individual(This, w3c:'Recommendation', G),
  rdf_assert_datatype(This, w3c:year, gYear, 2004, G),
  rdf_assert(
    This,
    std:title,
    literal('XML Schema Part 2: Datatypes Second Edition'),
    G
  ),
  rdf_assert(This, w3c:author, literal('Paul V. Biron'), G),
  rdf_assert(This, w3c:author, literal('Ashok Malhotra'), G),
  % Language-independent datatypes.
  rdf_assert(This, w3c:mentions, iso:'11404', G),
  % SQL datatypes.
  rdf_assert(This, w3c:mentions, std:'SQL', G).

init_xml_scheme_primer(G):-
  rdf_global_id(w3c:'TR/2004/REC-xmlschema-0-20041028/', This),
  rdfs_assert_class(w3c:'Recommendation', G).
  rdf_assert_individual(This, w3c:'Recommendation', G),
  rdf_assert_datatype(This, w3c:year, gYear, 2004, G),
  rdf_assert_literal(
    This,
    std:title,
    'XML Schema Part 0: Primer Second Edition',
    G
  ),
  rdf_assert_literal(This, w3c:author, 'David C. Fallside', G),
  rdf_assert_literal(This, w3c:author, 'Priscilla Walmsley', G).

