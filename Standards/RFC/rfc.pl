:- module(
  rfc,
  [
    rfc_graph/1 % ?Graph:atom
  ]
).

/** <module> RFC

@author Wouter Beek
@version 2013/05, 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').

:- initialization(init_rfc).



init_rfc:-
  rfc_graph(RFC_G),
  rdfs_assert_class(rfc:'Standard', RFC_G),
  W3C_G = w3c,
  init_xml(W3C_G).

init_xml(G):-
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

rfc_graph(rfc).

