:- module(std_rdf, []).

/** <module> STD_RDF

Standards metadata.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').

:- initialization(init_standards).



init_standards:-
  standards_graph(G),
  init_draft_phillips_record_jar_02(G).
  
init_draft_phillips_record_jar_02(G):-
  rdf_global_id(rfc:'draft-phillips-record-jar-02', This),
  rdfs_assert_class(rfc:'Draft', G),
  rdf_assert_individual(This, rfc:'Draft', G),
  rdf_assert_datatype(This, rfc:year, gYear, 2008, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'The record-jar Format',
    G
  ),
  rdf_assert_literal(This, rfc:editor, en, 'A. Phillips', G),
  rdf_assert(This, foaf:homepage, rfc:'draft-phillips-record-jar-02.txt', G).

