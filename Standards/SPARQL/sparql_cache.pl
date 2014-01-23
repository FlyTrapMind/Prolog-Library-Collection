:- module(
  sparql_cache,
  [
    'SPARQL_cache_iterative'/3 % +Resources1:ordset(or([bnode,iri,literal]))
                               % -Resources2:ordset(or([bnode,iri,literal]))
                               % -Propositions:ordset(list)
  ]
).

/** <module> SPARQL Cache

Locally caches triples that are relevant for specific resources.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_web(rdf_table)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- debug(sparql_cache).

:- initialization(init_sparql_cache).



'SPARQL_cache'(Remote, Resource, Resources, Propositions):-
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      inf,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(Remote, Query, _VarNames, Rows),

  % Conversion
  rows_to_propositions([Resource], Rows, Propositions),
  ord_union(Propositions, Resources).
  %%%%rows_to_resources(Rows, Resources).


'SPARQL_cache_iterative'([], [], []):- !.
'SPARQL_cache_iterative'([H|T], Resources, Propositions):- !,
  'SPARQL_cache_iterative'([H|T], [H], Resources, [], Propositions),
  % DEB
  findall(
    [S,P,O,none],
    member([S,P,O], Propositions),
    Quadruples
  ),
  rdf_store_table(Quadruples).
'SPARQL_cache_iterative'(Resource, Resources, Propositions):-
  'SPARQL_cache_iterative'([Resource], Resources, Propositions).

'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  rdf_is_bnode(H), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).
'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  rdf_is_literal(H), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).
'SPARQL_cache_iterative'([H1|T], Vs, VSol, Props, PropsSol):-
  uri_components(H1, uri_components(Scheme,Domain,Path,_Fragment,_Search)),
  uri_components(H2, uri_components(Scheme,Domain,Path,_NoFragment,_NoSearch)),
  file_name_type(_, Type, H2),
  memberchk(Type, [html,image,pdf]), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).
'SPARQL_cache_iterative'([H1|T1], Vs1, VSol, Props1, PropsSol):-
  message('Resource ~w', [H1]),
  (
    uri_components(H1, uri_components(_, Domain, _, _, _)),
    sparql_current_remote(Remote, Domain, _, _)
  ->
    'SPARQL_cache'(Remote, H1, Neighbors, NeighborProps)
  ;
    rdf_global_id(Prefix:_, H1),
    rdf_current_location(Prefix, URL)
  ->
    local_cache(Prefix, URL, H1, Neighbors, NeighborProps)
  ;
    is_of_type(uri, H1)
  ->
    local_cache(_, H1, H1, Neighbors, NeighborProps)
  ), !,

  % Filter on propositions that are included in results.
  exclude(old_proposition, NeighborProps, NewProps),
  length(NewProps, NumberOfNewProps),
  message('~d propositions added', [NumberOfNewProps]),

  % Filter on resources that have to be visited.
  exclude(old_neighbor(Vs1, NewProps), Neighbors, NewNeighbors),
  length(NewNeighbors, NumberOfNewNeighbors),
  message('~d resources added', [NumberOfNewNeighbors]),

  % Update resources that have to be visiterd.
  append(T1, NewNeighbors, T2),

  % Update results.
  ord_union(Vs1, Neighbors, Vs2),
  ord_union(Props1, NewProps, Props2),

  % Recurse.
  'SPARQL_cache_iterative'(T2, Vs2, VSol, Props2, PropsSol).
% Thw show must go on!
'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  message('~w failed', [H]),
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).


old_neighbor(Vs1, _, Element):-
  memberchk(Element, Vs1), !.
old_neighbor(_, NewProps, Element):-
  member(
    [_,'http://dbpedia.org/ontology/wikiPageExternalLink',Element],
    NewProps
  ), !.


local_cache(G, URL, H1, Neighbors, NeighborProps):-
  uri_query_add(H1, format, rdf, URL),
  catch(download_to_file(URL, File), _, fail),
  (
    load_html(File, _, [])
  ->
    delete_file(File)
  ;
    % Make sure there is a graph name.
    (
      nonvar(G), !
    ;
      url_to_graph_name(URL, G)
    ),
    
    % Make sure the file is loaded in the graph.
    (
      rdf_graph(G), !
    ;
      rdf_load2(File, [graph(G)])
    ),
    
    setoff(
      [S,P,O],
      rdf(S, P, O, G),
      NeighborProps
    ),
    ord_union(NeighborProps, Neighbors)
  ).


message(Format, Args):-
  debug(sparql_cache, Format, Args),
  format(user_output, Format, Args),
  nl(user_output),
  flush_output(user_output).


init_sparql_cache:-
  rdf_set_predicate(owl:sameAs, symmetric(true)),
  rdf_set_predicate(owl:sameAs, transitive(true)),
  rdf_set_predicate(rdfs:subClassOf, transitive(true)),
  rdf_set_predicate(rdfs:subPropertyOf, transitive(true)).

old_proposition([S,P,O]):-
  rdf(S, P, O), !.
old_proposition([S,P,O]):-
  rdf_predicate_property(P, symmetric(true)),
  rdf(O, P, S), !.


row_to_proposition(Prefix, Row, L):-
  Row =.. [row|Suffix],
  append(Prefix,  Suffix, L).

rows_to_propositions(Prefix, Rows, Props):-
  rows_to_propositions(Prefix, Rows, [], Props).

rows_to_propositions(_, [], Sol, Sol):- !.
rows_to_propositions(Prefix, [H1|T], L1, Sol):-
  row_to_proposition(Prefix, H1, H2),
  ord_add_element(L1, H2, L2),
  rows_to_propositions(Prefix, T, L2, Sol).


rows_to_resources(Rows, Resources):-
  rows_to_resources(Rows, [], Resources).

rows_to_resources([], Resources, Resources).
rows_to_resources([Row|Rows], Resources1, Sol):-
  Row =.. [row|NewResources],
  ord_union(Resources1, NewResources, Resources2),
  rows_to_resources(Rows, Resources2, Sol).





:- db_add_novel(user:prolog_file_type(bmp,  image)).
:- db_add_novel(user:prolog_file_type(gif,  image)).
:- db_add_novel(user:prolog_file_type(jpeg, image)).
:- db_add_novel(user:prolog_file_type(jpg,  image)).
:- db_add_novel(user:prolog_file_type(png,  image)).

% Used in case the XML namespace does not denote
%  a machine-readable description of the vocabulary, e.g. Dublin Core.
:- dynamic(rdf_location/2).

rdf_register_location(Prefix, Location):-
  db_add_novel(rdf_location(Prefix, Location)).

rdf_current_location(Prefix, Location):-
  rdf_location(Prefix, Location), !.
rdf_current_location(Prefix, Location):-
  xml_current_namespace(Prefix, Location).


:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').
:- sparql_add_prefix(dbo).

:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').
:- sparql_add_prefix(dbp).

:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- sparql_add_prefix(dc).

:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- sparql_add_prefix(dcterms).
:- rdf_register_location(dcterms, 'http://dublincore.org/2012/06/14/dcterms.rdf').

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- sparql_add_prefix(dbpedia).
:- sparql_add_remote(dbpedia, 'dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'bg.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'br.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'bs.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'ca.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'cs.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'ia.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'id.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'io.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'is.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'it.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'ja.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'my.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'nn.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'no.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'nv.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'oc.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'pl.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'pt.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'ro.dbpedia.org', default, '/sparql').
:- sparql_add_remote(dbpedia, 'ru.dbpedia.org', default, '/sparql').

:- xml_register_namespace(eor, 'http://dublincore.org/2000/03/13/eor#').
:- sparql_add_prefix(eor).

:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').
:- sparql_add_prefix(fb).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- sparql_add_prefix(foaf).

:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- sparql_add_prefix(owl).

:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').
:- sparql_add_prefix('powder-s').

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- sparql_add_prefix(rdf).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- sparql_add_prefix(rdfs).

:- xml_register_namespace(serql, 'http://www.openrdf.org/schema/serql#').
:- sparql_add_prefix(serql).

:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').
:- sparql_add_prefix(skos).

:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').
:- sparql_add_prefix(umbel).

:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- sparql_add_prefix(xsd).

:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').
:- sparql_add_prefix(yago).
