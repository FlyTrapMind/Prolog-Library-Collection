:- module(
  'SPARQL_db',
  [
    'SPARQL_register_remote'/4, % +Remote:atom
                                % +Server:atom
                                % +Port:or([oneof([default]),nonneg])
                                % +Path:atom
    'SPARQL_current_remote'/4, % ?Remote:atom
                               % ?Server:atom
                               % ?Port:or([oneof([default]),nonneg])
                               % ?Path:atom
    'SPARQL_remove_remote'/1 % +Remote:atom
  ]
).

/** <module> SPARQL database

Persistency store for SPARQL-related information.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(persistency)).
:- use_module(library(semweb/rdf_db)).
:- use_module('LOD'('LOD_location')).
:- use_module(os(file_ext)).
:- use_module(xml(xml_namespace)).

%! 'SPARQL_remote'(
%!   ?Remote:atom,
%!   ?Server:atom,
%!   ?Port:atomic,
%!   ?Path:atom
%! ) is nondet.

:- persistent(sparql_remote(remote:atom,server:atom,port:atomic,path:atom)).

:-
  absolute_file_name(project('SPARQL.db'), File, [access(write)]),
  db_attach(File, []).



% SPARQL remote

'SPARQL_register_remote'(Remote, Server, Port, Path):-
  'SPARQL_current_remote'(Remote, Server, Port, Path), !,
  debug('SPARQL_db', 'SPARQL remote ~w is already set. No changes.', [Remote]).
'SPARQL_register_remote'(Remote, _Server1, _Port1, _Path1):-
  'SPARQL_current_remote'(Remote, _Server2, _Port2, _Path2), !,
  debug(
    'SPARQL_db',
    'SPARQL remote ~w is already set DIFFERENTLY. First remove.',
    [Remote]
  ).
'SPARQL_register_remote'(Remote, Server, Port, Path):-
  with_mutex('SPARQL_db', assert_sparql_remote(Remote, Server, Port, Path)).


'SPARQL_current_remote'(Remote, Server, Port, Path):-
  with_mutex('SPARQL_db', sparql_remote(Remote, Server, Port, Path)).


'SPARQL_remove_remote'(Remote):-
  with_mutex(
    'SPARQL_db',
    (
      once(sparql_remote(Remote, Server, Port, Path)), !,
      retractall_sparql_remote(Remote, Server, Port, Path)
    )
  ).
'SPARQL_remove_remote'(Remote):-
  existence_error('SPARQL remote', Remote).



% Registrations

% HTML
:- db_add_novel(user:prolog_file_type(htm,  html)).
:- db_add_novel(user:prolog_file_type(html, html)).

% Image
:- db_add_novel(user:prolog_file_type(bmp,  image)).
:- db_add_novel(user:prolog_file_type(gif,  image)).
:- db_add_novel(user:prolog_file_type(jpeg, image)).
:- db_add_novel(user:prolog_file_type(jpg,  image)).
:- db_add_novel(user:prolog_file_type(png,  image)).


% DBpedia
:- 'SPARQL_register_remote'(dbpedia, 'dbpedia.org', default, '/sparql').

% DBpedia ontology
:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').

% DBpedia property
:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').

% DBpedia resource
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia localizations
:- 'SPARQL_register_remote'('af.dbpedia', 'af.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'bg.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'br.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'bs.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'ca.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('cs.dbpedia', 'cs.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'ia.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('id.dbpedia', 'id.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'io.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'is.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('it.dbpedia', 'it.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('ja.dbpedia', 'ja.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'my.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'nn.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'no.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'nv.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'oc.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('pl.dbpedia', 'pl.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('pt.dbpedia', 'pt.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'(dbpedia, 'ro.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('ru.dbpedia', 'ru.dbpedia.org', default, '/sparql').

% Dublin Core elements
:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').

% Dublin Core terms
:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- 'LOD_register_location'(dcterms, 'http://dublincore.org/2012/06/14/dcterms.rdf').

% Dublin core ?
:- xml_register_namespace(eor, 'http://dublincore.org/2000/03/13/eor#').

% Freebase
:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').

% FOAF
:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

% OWL
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_set_predicate(owl:sameAs, symmetric(true)).
:- rdf_set_predicate(owl:sameAs, transitive(true)).

% ?
:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').

% RDF
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% RDFS
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_set_predicate(rdfs:subClassOf, transitive(true)).
:- rdf_set_predicate(rdfs:subPropertyOf, transitive(true)).

% SERQL
:- xml_register_namespace(serql, 'http://www.openrdf.org/schema/serql#').

% SKOS
:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').

% UMBEL
:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').

% XSD
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

% YAGO resource
:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').

