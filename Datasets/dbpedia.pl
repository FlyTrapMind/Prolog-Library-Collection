:- module(
  dbpedia,
  [
    assert_identity_resource/2, % +Resource:uri
                                % +Graph:atom
    assert_resource/2, % +Graph:atom
                       % +Resource:uri
    dbpedia_find_concept/2, % +Name:atom
                            % -ConceptName:uri
    find_dbpedia_agent/4 % +Name:atom
                         % +Birth:integer
                         % +Death:integer
                         % -DBpediaAgent:uri
  ]
).

/** <module> DBPEDIA

Query DBpedia using SPARQL.

### Examples

Query for a resource:
~~~{.sparql}
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX umbel: <http://umbel.org/umbel/rc/>
SELECT DISTINCT ?s
WHERE {
  ?s rdf:type umbel:Writer .
  ?s rdfs:label ?label .
  FILTER regex(?label, "Queneau", "i")
}
LIMIT 1
OFFSET 0
~~~

Retrieve all known facts about a query result:
~~~{.sparql}
PREFIX dbpedia: <http://dbpedia.org/resource/>
SELECT ?p ?o
WHERE
{
  dbpedia.org:Raymond_Queneau ?p ?o .
}
~~~

@author Wouter Beek
@version 2013/03-2013/05, 2013/08, 2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_term)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(sparql(sparql_sameas)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(ttl, turtle)).

% XML and SPARQL namespace prefixes that often occur in DBpedia.
:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').
:- sparql_add_prefix(dbo).

:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').
:- sparql_add_prefix(dbp).

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- sparql_add_prefix(dbpedia).
:- sparql_add_remote(dbpedia, 'dbpedia.org', default, '/sparql').

:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').
:- sparql_add_prefix(fb).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- sparql_add_prefix(foaf).

:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').
:- sparql_add_prefix('powder-s').

:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').
:- sparql_add_prefix(umbel).

:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').
:- sparql_add_prefix(yago).

:- rdf_meta(assert_resource(+,r)).
:- rdf_meta(find_dbpedia_agent(+,+,+,r)).

:- debug(dbpedia).



assert_identity_resource(IRI, G):-
  owl_identity_set(IRI, I_Set),
  maplist(assert_resource(G), I_Set).

assert_resource(G, IRI):-
  rdf_is_iri(IRI),
  rdf_graph(G), !,
  describe_sameas(dbpedia, IRI, PO_Rows),
  forall(
    member(row(P, O), PO_Rows),
    rdf_assert(IRI, P, O, G)
  ).

%! dbpedia_find_concept(+Name:atom, -ConceptName:uri) is det.

dbpedia_find_concept(Name, ConceptName):-
  Where1 = '?concept rdfs:label ?label .',
  format(atom(Where2), 'FILTER regex(?label, "~w", "i")', [Name]),
  Where = [Where1,Where2],
  formulate_sparql(
    _Graph,
    [],
    select([distinct(true)],[concept]),
    Where,
    limit([],10),
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(dbpedia, 'Could not find a resource for \'~w\'.', [Name])
  ;
    first(Resources, row(ConceptName))
  ).

%! find_person(
%!   +FullName:atom,
%!   +Birth:integer,
%!   +Death:integer,
%!   -DBpediaAuthor:uri
%! ) is semidet.

find_dbpedia_agent(Name, Birth, Death, DBpediaAuthor):-
  format(
    atom(Where),
    [
      '?writer rdf:type foaf:Person .',
      '?writer rdfs:label ?label .',
      'FILTER regex(?label, "~w", "i")',
      '?writer dbpprop:dateOfBirth ?birth .',
      'FILTER regex(?birth, "~w")',
      '?writer dbpprop:dateOfDeath ?death .',
      'FILTER regex(?death, "~w")'
    ],
    [Name, Birth, Death]
  ),
  formulate_sparql(
    _Graph,
    [dbp,foaf],
    select([distinct(true)],[writer]),
    Where,
    limit([],10),
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(dbpedia, 'Could not find a resource for \'~w\'.', [Name])
  ;
    first(Resources, row(DBpediaAuthor))
  ).

