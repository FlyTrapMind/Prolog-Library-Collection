:- module(
  'SPARQL_db',
  [
    'SPARQL_register_remote'/4, % +SPARQL_Remote:atom
                                % +Server:atom
                                % +Port:or([oneof([default]),nonneg])
                                % +Path:atom
    'SPARQL_current_remote'/4, % ?SPARQL_Remote:atom
                               % ?Server:atom
                               % ?Port:or([oneof([default]),nonneg])
                               % ?Path:atom
    'SPARQL_remove_remote'/1, % +SPARQL_Remote:atom
    'SPARQL_current_remote_domain'/2, % ?SPARQL_Remote:atom
                                      % ?Domain:atom
    'SPARQL_register_remote_domain'/2 % +SPARQL_Remote:atom
                                      % +Domain:atom
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

%! sparql_remote(
%!   ?Remote:atom,
%!   ?Server:atom,
%!   ?Port:atomic,
%!   ?Path:atom
%! ) is nondet.

:- persistent(sparql_remote(remote:atom,server:atom,port:atomic,path:atom)).
:- persistent(sparql_remote_domain(sparql_remote:atom,domain:atom)).

:-
  absolute_file_name(project('SPARQL.db'), File, [access(write)]),
  db_attach(File, []).



% SPARQL remote

'SPARQL_register_remote'(Remote, Domain, Port, Path):-
  'SPARQL_current_remote'(Remote, Domain, Port, Path), !,
  debug('SPARQL_db', 'SPARQL remote ~w is already set. No changes.', [Remote]).
'SPARQL_register_remote'(Remote, _Server1, _Port1, _Path1):-
  'SPARQL_current_remote'(Remote, _Server2, _Port2, _Path2), !,
  debug(
    'SPARQL_db',
    'SPARQL remote ~w is already set DIFFERENTLY. First remove.',
    [Remote]
  ).
'SPARQL_register_remote'(Remote, Domain, Port, Path):-
  with_mutex('SPARQL_db', assert_sparql_remote(Remote, Domain, Port, Path)).


'SPARQL_current_remote'(Remote, Domain, Port, Path):-
  with_mutex('SPARQL_db', sparql_remote(Remote, Domain, Port, Path)).


'SPARQL_remove_remote'(Remote):-
  with_mutex(
    'SPARQL_db',
    (
      once(sparql_remote(Remote, Domain, Port, Path)), !,
      retractall_sparql_remote(Remote, Domain, Port, Path)
    )
  ).
'SPARQL_remove_remote'(Remote):-
  existence_error('SPARQL remote', Remote).

'SPARQL_current_remote_domain'(SPARQL_Remote, Domain):-
  'SPARQL_current_remote'(SPARQL_Remote, Domain, _, _).
'SPARQL_current_remote_domain'(SPARQL_Remote, Domain):-
  with_mutex('SPARQL_db', sparql_remote_domain(SPARQL_Remote, Domain)).

'SPARQL_register_remote_domain'(SPARQL_Remote, Domain):-
  'SPARQL_current_remote'(SPARQL_Remote, _, _, _),
  with_mutex('SPARQL_db', assert_sparql_remote_domain(SPARQL_Remote, Domain)).



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
:- xml_register_namespace('dbpedia-owl', 'http://dbpedia.org/ontology/').

% DBpedia property
:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').
:- xml_register_namespace(dbpprop, 'http://dbpedia.org/property/').

% DBpedia resource
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia localizations
:- 'SPARQL_register_remote_domain'(dbpedia, 'af.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ar.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'arc.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'arz.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'av.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ay.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'be.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'bjn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'bn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'bo.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'br.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'bs.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ca.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'cdo.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ce.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'chr.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'chy.dbpedia.org').
:- 'SPARQL_register_remote'('cs.dbpedia', 'cs.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'cy.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'da.dbpedia.org').
:- 'SPARQL_register_remote'('el.dbpedia', 'el.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'eo.dbpedia.org').
:- 'SPARQL_register_remote'('es.dbpedia', 'es.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'fa.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'fi.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ga.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'gd.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'gl.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'gn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'gu.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ha.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'hak.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'he.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'hi.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ht.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'hu.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'hy.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ia.dbpedia.org').
:- 'SPARQL_register_remote'('id.dbpedia', 'id.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'io.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'is.dbpedia.org').
:- 'SPARQL_register_remote'('it.dbpedia', 'it.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('ja.dbpedia', 'ja.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'jv.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'kk.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'kl.dbpedia.org').
:- 'SPARQL_register_remote'('ko.dbpedia', 'ko.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'la.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'lbe.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'lez.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'li.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'lt.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'lv.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'mhr.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'mk.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ml.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'mr.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'mrj.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ms.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'my.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'na.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'nah.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ne.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'new.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'nn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'no.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'nrm.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'nv.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'oc.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'pnb.dbpedia.org').
:- 'SPARQL_register_remote'('pl.dbpedia', 'pl.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote'('pt.dbpedia', 'pt.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'qu.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ro.dbpedia.org').
:- 'SPARQL_register_remote'('ru.dbpedia', 'ru.dbpedia.org', default, '/sparql').
:- 'SPARQL_register_remote_domain'(dbpedia, 'rw.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sco.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'se.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'simple.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sl.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sq.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sr.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'srn.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'su.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sv.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'sw.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ta.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'te.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'tg.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'th.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'tl.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'tum.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'udm.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'ug.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'uk.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'vi.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'wa.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'war.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'wo.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'xal.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'yi.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'yoh.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'zh.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'zh_min_nan.dbpedia.org').
:- 'SPARQL_register_remote_domain'(dbpedia, 'zh_yue.dbpedia.org').

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

