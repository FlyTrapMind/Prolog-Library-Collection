:- module(
  dbpedia,
  [
    dbpedia_find_agent/4 % +Name:atom
                         % +Birth:integer
                         % +Death:integer
                         % -DBpediaAgent:iri
  ]
).

/** <module> DBpedia

Dedicated DBpedia queries using SPARQL.

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
@version 2013/03-2013/05, 2013/08, 2013/12-2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_term)).
:- use_module('SPARQL'('SPARQL_build')).
:- use_module('SPARQL'('SPARQL_db')).
:- use_module('SPARQL'('SPARQL_ext')).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(ttl, turtle)).

% XML and SPARQL namespace prefixes that often occur in DBpedia.
:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').

:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- 'SPARQL_register_remote'(dbpedia, 'dbpedia.org', default, '/sparql').

:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').

:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').

:- rdf_meta(dbpedia_find_agent(+,+,+,r)).



%! dbpedia_find_agent(
%!   +FullName:atom,
%!   +Birth:integer,
%!   +Death:integer,
%!   -DBpediaAuthor:uri
%! ) is semidet.

dbpedia_find_agent(Name, Birth, Death, DBpediaAuthor):-
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [dbp,foaf],
      select,
      true,
      [writer],
      [
        rdf(var(writer), rdf:type, foaf:'Person'),
        rdf(var(writer), rdfs:label, var(label)),
        filter(regex(var(label), string(Name), [case_insensitive])),
        rdf(var(writer), dbpprop:dateOfBirth, var(birth)),
        filter(regex(var(birth), string(Birth))),
        rdf(var(writer), dbpprop:dateOfDeath, var(death)),
        filter(regex(var(death), string(Death)))
      ],
      10,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(dbpedia, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(dbpedia, 'Could not find a resource for \'~w\'.', [Name])
  ;
    first(Resources, row(DBpediaAuthor))
  ).

