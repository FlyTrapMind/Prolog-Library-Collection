:- module(
  kadaster,
  [
    load_kadaster/0,
    query_kadaster/1, % -Resources:list(iri)
    query_kadaster/2, % +Subject
                      % -Resources:list(iri)
    scrape_kadaster/0
  ]
).

/** <module> Kadaster

@author Wouter Beek
@version 2014/01, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(rdf_file(rdf_serial)).
:- use_module(sparql(sparql_api)).
:- use_module(sparql(sparql_db)).

:- initialization(init_kadaster).
init_kadaster:-
  uri_components(Url, uri_components(http,'brk.kadaster.nl','/sparql',_,_)),
  sparql_register_endpoint(kadaster, query, Url).



load_kadaster:-
  absolute_file_name(kadaster, File, [access(read)]),
  rdf_load_any([format(turtle),graph(kadaster)], File).

query_kadaster(Resources):-
  sparql_select(kadaster, _, [], true, [s,p,o],
      [rdf(var(s), var(p), var(o))], 10, _, _, Resources).

query_kadaster(S, Resources):-
  sparql_select(kadaster, _, [], true, [p,o],
      [rdf(iri(S), var(p), var(o))], 10, _, _, Resources).

scrape_kadaster:-
  sparql_select(kadaster, _, [], true, [s,p,o],
      [rdf(var(s), var(p), var(o))], inf, _, _, Rows),
  maplist(assert_row_as_triple, Rows),
  rdf_save([format(turtle)], kadaster, kadaster).

assert_row_as_triple(row(S,P,O)):-
  rdf_assert(S, P, O, kadaster).

