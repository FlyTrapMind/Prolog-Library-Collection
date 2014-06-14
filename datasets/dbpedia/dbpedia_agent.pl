:- module(
  dbpedia_agent,
  [
    dbpedia_find_agent/4 % +Name:atom
                         % +Birth:integer
                         % +Death:integer
                         % -DBpediaAgent:iri
  ]
).

/** <module> DBpedia agent

Search for agents (e.g. people) on DBpedia.

@author Wouter Beek
@version 2013/03-2013/05, 2013/08, 2013/12-2014/01, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(list_ext)).
:- use_module(generics(row_ext)).
:- use_module(generics(typecheck)).
:- use_module(owl(owl_read)).
:- use_module(sparql(sparql_api)).

:- use_module(plRdf_term(rdf_term)).

:- rdf_meta(dbpedia_find_agent(+,+,+,r)).

