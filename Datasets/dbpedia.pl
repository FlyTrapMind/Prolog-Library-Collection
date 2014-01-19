:- module(
  dbpedia,
  [
    dbpedia_assert/3, % +Options:list(nvpair)
                      % +Resource:iri
                      % +Graph:atom
    dbpedia_describe/3, % +Options:list(nvpair)
                        % +Resource:iri
                        % -PO_Pairs:list(list(or([bnode,iri,literal])))
    dbpedia_find_agent/4, % +Name:atom
                          % +Birth:integer
                          % +Death:integer
                          % -DBpediaAgent:iri
    dbpedia_find/2 % +SearchTerm:atom
                   % -Resource:iri
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
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_term)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
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

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- sparql_add_prefix(rdfs).

:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').
:- sparql_add_prefix(umbel).

:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').
:- sparql_add_prefix(yago).

:- rdf_meta(dbpedia_assert(+,r)).
:- rdf_meta(dbpedia_find_agent(+,+,+,r)).



dbpedia_assert(O1, Resource, G):-
  'SPARQL_assert'(O1, dbpedia, Resource, G).



dbpedia_describe(O1, Resource, PO_Pairs):-
  'SPARQL_describe'(O1, dbpedia, Resource, PO_Pairs).



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
      [dbp,foaf],
      select,
      true,
      [writer],
      [
        rdf(var(writer), rdf:type, foaf:Person),
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



%! dbpedia_find(+SearchTerm:atom, -Resource:iri) is det.
% Returns the DBpedia concept that best fits the given search term.
%
% If the search term is itself a resource, then this is returned.
% Otherwise, DBpedia is searched for a resource that is labeled with
%  the given search term.
%
% @arg SearchTerm
% @arg Resource

dbpedia_find(SearchTerm, Resource):-
  'SPARQL_find'(dbpedia, SearchTerm, Resource).

