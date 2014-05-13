:- module(
  sparql_db,
  [
    sparql_register_remote/4, % +SparqlRemote:atom
                              % +Server:atom
                              % +Port:or([oneof([default]),nonneg])
                              % +Path:atom
    sparql_current_remote/4, % ?SparqlRemote:atom
                             % ?Server:atom
                             % ?Port:or([oneof([default]),nonneg])
                             % ?Path:atom
    sparql_remove_remote/1, % +SparqlRemote:atom
    sparql_current_remote_domain/2, % ?SparqlRemote:atom
                                    % ?Domain:atom
    sparql_register_remote_domain/2, % +SparqlRemote:atom
                                     % +Domain:atom
    sparql_remote_domain/2 % +SparqlRemote:atom
                           % +Domain:atom
  ]
).

/** <module> SPARQL database

Persistency store for SPARQL-related information.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01,
         2014/04
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(db_ext)).
:- use_module(lod(lod_location)).
:- use_module(os(file_ext)).
:- use_module(xml(xml_namespace)).

%! sparql_remote(
%!   ?Remote:atom,
%!   ?Server:atom,
%!   ?Port:atomic,
%!   ?Path:atom
%! ) is nondet.

:- dynamic(sparql_remote/4).
:- dynamic(sparql_remote_domain/2).



% SPARQL remote

sparql_register_remote(Remote, Domain, Port, Path):-
  sparql_current_remote(Remote, Domain, Port, Path), !,
  debug(sparql_db, 'SPARQL remote ~w is already set. No changes.', [Remote]).
sparql_register_remote(Remote, _, _, _):-
  sparql_current_remote(Remote, _, _, _), !,
  debug(
    sparql_db,
    'SPARQL remote ~w is already set DIFFERENTLY. First remove.',
    [Remote]
  ).
sparql_register_remote(Remote, Domain, Port, Path):-
  db_add_novel(sparql_remote(Remote, Domain, Port, Path)).


sparql_current_remote(Remote, Domain, Port, Path):-
  sparql_remote(Remote, Domain, Port, Path).


sparql_remove_remote(Remote):-
  once(sparql_remote(Remote, Domain, Port, Path)), !,
  retractall(sparql_remote(Remote, Domain, Port, Path)).
sparql_remove_remote(Remote):-
  existence_error('SPARQL remote', Remote).

sparql_current_remote_domain(SparqlRemote, Domain):-
  sparql_current_remote(SparqlRemote, Domain, _, _).
sparql_current_remote_domain(SparqlRemote, Domain):-
  sparql_remote_domain(SparqlRemote, Domain).

sparql_register_remote_domain(SparqlRemote, Domain):-
  sparql_current_remote(SparqlRemote, _, _, _),
  db_add_novel(sparql_remote_domain(SparqlRemote, Domain)).



% REGISTRATIONS

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
:- sparql_register_remote(dbpedia, 'dbpedia.org', default, '/sparql').
:- sparql_register_remote('live.dbpedia', 'live.dbpedia.org', default, '/sparql').
:- lod_register_header(dbpedia, 'Accept', 'application/rdf+xml').
:- lod_register_location(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia ontology
:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').

% DBpedia property
:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').

% DBpedia resource
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia localizations
:- initialization(dbpedia_localizations).
dbpedia_localizations:-
  forall(
    dbpedia_language_tag(LangTag),
    dbpedia_register(LangTag)
  ).

dbpedia_register(LangTag):-
  atomic_list_concat([LangTag,dbpedia,org], '.', Domain),
  sparql_register_remote_domain(dbpedia, Domain),
  
  atomic_list_concat([LangTag,dbpedia], '.', Remote),
  sparql_register_remote(Remote, Domain, default, '/sparql'),
  
  atomic_list_concat([LangTag,dbp], '.', ResourceNamespace),
  uri_components(
    ResourcePrefix,
    uri_components(http,Domain,'/resource/',_,_)
  ),
  xml_register_namespace(ResourceNamespace, ResourcePrefix),
  
  atomic_list_concat([LangTag,dbpprop], '.', PropertyNamespace),
  uri_components(
    PropertyPrefix,
    uri_components(http,Domain,'/property/',_,_)
  ),
  xml_register_namespace(PropertyNamespace, PropertyPrefix).

%! dbpedia_language_tag(+LanguageTag:atom) is semidet.
%! dbpedia_language_tag(-LanguageTag:atom) is multi.

dbpedia_language_tag(ace).
dbpedia_language_tag(af).
dbpedia_language_tag(als).
dbpedia_language_tag(am).
dbpedia_language_tag(an).
dbpedia_language_tag(ang).
dbpedia_language_tag(ar).
dbpedia_language_tag(arc).
dbpedia_language_tag(arz).
dbpedia_language_tag(as).
dbpedia_language_tag(ast).
dbpedia_language_tag(av).
dbpedia_language_tag(ay).
dbpedia_language_tag(az).
dbpedia_language_tag(ba).
dbpedia_language_tag(bar).
dbpedia_language_tag(bat_smg).
dbpedia_language_tag(bcl).
dbpedia_language_tag(bcl_smg).
dbpedia_language_tag(be).
dbpedia_language_tag('be-x-old').
dbpedia_language_tag(bg).
dbpedia_language_tag(bi).
dbpedia_language_tag(bjn).
dbpedia_language_tag(bm).
dbpedia_language_tag(bn).
dbpedia_language_tag(bo).
dbpedia_language_tag(bpy).
dbpedia_language_tag(br).
dbpedia_language_tag(bs).
dbpedia_language_tag(bxr).
dbpedia_language_tag(ca).
dbpedia_language_tag(cdo).
dbpedia_language_tag(ce).
dbpedia_language_tag(ceb).
dbpedia_language_tag(chr).
dbpedia_language_tag(chy).
dbpedia_language_tag(ckb).
dbpedia_language_tag(co).
dbpedia_language_tag(commons).
dbpedia_language_tag(cr).
dbpedia_language_tag(crh).
dbpedia_language_tag(cs).
dbpedia_language_tag(cy).
dbpedia_language_tag(da).
dbpedia_language_tag(diq).
dbpedia_language_tag(dv).
dbpedia_language_tag(el).
dbpedia_language_tag(eo).
dbpedia_language_tag(es).
dbpedia_language_tag(eu).
dbpedia_language_tag(fa).
dbpedia_language_tag(fi).
dbpedia_language_tag(fr).
dbpedia_language_tag(frp).
dbpedia_language_tag(fy).
dbpedia_language_tag(ga).
dbpedia_language_tag(gan).
dbpedia_language_tag(gd).
dbpedia_language_tag(gl).
dbpedia_language_tag(gn).
dbpedia_language_tag(got).
dbpedia_language_tag(gu).
dbpedia_language_tag(gv).
dbpedia_language_tag(ha).
dbpedia_language_tag(hak).
dbpedia_language_tag(he).
dbpedia_language_tag(hi).
dbpedia_language_tag(hif).
dbpedia_language_tag(hsb).
dbpedia_language_tag(ht).
dbpedia_language_tag(hu).
dbpedia_language_tag(hy).
dbpedia_language_tag(ia).
dbpedia_language_tag(id).
dbpedia_language_tag(ig).
dbpedia_language_tag(io).
dbpedia_language_tag(is).
dbpedia_language_tag(it).
dbpedia_language_tag(ja).
dbpedia_language_tag(jv).
dbpedia_language_tag(kaa).
dbpedia_language_tag(kab).
dbpedia_language_tag(kbd).
dbpedia_language_tag(ki).
dbpedia_language_tag(kk).
dbpedia_language_tag(kl).
dbpedia_language_tag(km).
dbpedia_language_tag(kn).
dbpedia_language_tag(ko).
dbpedia_language_tag(la).
dbpedia_language_tag(lbe).
dbpedia_language_tag(lez).
dbpedia_language_tag(li).
dbpedia_language_tag(ln).
dbpedia_language_tag(lt).
dbpedia_language_tag(lv).
dbpedia_language_tag(mg).
dbpedia_language_tag(mhr).
dbpedia_language_tag(mk).
dbpedia_language_tag(ml).
dbpedia_language_tag(mr).
dbpedia_language_tag(mrj).
dbpedia_language_tag(ms).
dbpedia_language_tag(my).
dbpedia_language_tag(na).
dbpedia_language_tag(nah).
dbpedia_language_tag(ne).
dbpedia_language_tag(new).
dbpedia_language_tag(nn).
dbpedia_language_tag(no).
dbpedia_language_tag(nrm).
dbpedia_language_tag(nv).
dbpedia_language_tag(oc).
dbpedia_language_tag(pnb).
dbpedia_language_tag(pl).
dbpedia_language_tag(pt).
dbpedia_language_tag(qu).
dbpedia_language_tag(ro).
dbpedia_language_tag(ru).
dbpedia_language_tag(rw).
dbpedia_language_tag(sco).
dbpedia_language_tag(se).
dbpedia_language_tag(simple).
dbpedia_language_tag(sl).
dbpedia_language_tag(sn).
dbpedia_language_tag(sq).
dbpedia_language_tag(sr).
dbpedia_language_tag(srn).
dbpedia_language_tag(su).
dbpedia_language_tag(sv).
dbpedia_language_tag(sw).
dbpedia_language_tag(szl).
dbpedia_language_tag(ta).
dbpedia_language_tag(te).
dbpedia_language_tag(tg).
dbpedia_language_tag(th).
dbpedia_language_tag(tl).
dbpedia_language_tag(tr).
dbpedia_language_tag(tt).
dbpedia_language_tag(tum).
dbpedia_language_tag(udm).
dbpedia_language_tag(ug).
dbpedia_language_tag(uk).
dbpedia_language_tag(vi).
dbpedia_language_tag(wa).
dbpedia_language_tag(war).
dbpedia_language_tag(wo).
dbpedia_language_tag(xal).
dbpedia_language_tag(yi).
dbpedia_language_tag(yo).
dbpedia_language_tag(yoh).
dbpedia_language_tag(zh).
dbpedia_language_tag(zh_min_nan).
dbpedia_language_tag(zh_yue).

% Dublin Core elements
:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- lod_register_location(dc, 'http://dublincore.org/2012/06/14/dcelements.rdf').

% Dublin Core terms
:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- lod_register_location(dcterms, 'http://dublincore.org/2012/06/14/dcterms.rdf').

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

% PROV
:- xml_register_namespace(prov, 'http://www.w3.org/ns/prov#').

% RDF
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% RDFS
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_set_predicate(rdfs:subClassOf, transitive(true)).
:- rdf_set_predicate(rdfs:subPropertyOf, transitive(true)).

% Schema
:- xml_register_namespace(schema, 'http://schema.org/').
:- lod_register_location(schema, 'http://schema.rdfs.org/all.ttl').

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

