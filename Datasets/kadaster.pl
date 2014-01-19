:- module(
  kadaster,
  [
    load_kadaster/0,
    query_kadaster/1, % +Resources:list(iri)
    query_kadaster/2, % +Subject
                      % +Resources:list(iri)
    scrape_kadaster/0
  ]
).

/** <module> Kadaster

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_serial)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).

:- sparql_add_remote(kadaster, 'brk.kadaster.nl', default, '/sparql').



load_kadaster:-
  absolute_file_name(kadaster, File, [access(read)]),
  rdf_load2(File, [format(turtle),graph(kadaster)]).

query_kadaster(Resources):-
  'SPARQL_formulate'(
    _Graph,
    [],
    select([distinct(true)],[s,p,o]),
    ['?s ?p ?o .'],
    limit([],10),
    Query
  ),
  'SPARQL_enqueue'(kadaster, Query, _VarNames, Resources).

query_kadaster(S, Resources):-
  phrase(
    'SPARQL_formulate'(
      _,
      [],
      select,
      true,
      [p,o]),
      [rdf(iri(S), var(p), var(o))],
      10,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(kadaster, Query, _VarNames, Resources).

scrape_kadaster:-
  phrase(
    'SPARQL_formulate'(
      _,
      [],
      select,
      true,
      [s,p,o],
      [rdf(var(s), var(p), var(o))],
      inf,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(kadaster, Query, _VarNames, Resources),
  forall(
    member(row(S,P,O), Resources),
    rdf_assert(S, P, O, kadaster)
  ),
  rdf_save2(kadaster, [format(turtle),graph(kadaster)]).
