:- module(rdf_axiom_test, []).

/** <module> RDF_AXIOM_TEST

@author Wouter Beek
@version 2013/08
*/

:- use_module(library(plunit)).

:- begin_tests(rdf_axiom).

:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(logic(rdf_axiom)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(test_triple(?,r,r,r)).

test(rdf_query, [forall(test_triple(_Lang, S, P, O))]):-
  query(S, P, O).

%! test_triple(
%!   ?Language:oneof([rdf,rdfs]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

test_triple(rdfs, rdfs:'Resource',    rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:'Class',       rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:'Literal',     rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'XMLLiteral',   rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:'Datatype',    rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'Seq',          rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'Bag',          rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'Alt',          rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:'Container',   rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'List',         rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:'ContainerMembershipProperty', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Property',     rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdf:'Statement',    rdf:type, rdfs:'Class'  ).
test_triple(rdfs, rdfs:domain,        rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:range,         rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:subPropertyOf, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:subClassOf,    rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:member,        rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:seeAlso,       rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:isDefinedBy,   rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:comment,       rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:label,         rdf:type, rdf:'Property').

:- end_tests(rdf_axiom).

