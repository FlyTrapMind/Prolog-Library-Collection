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
  sparql_register_endpoint(
    kadaster,
    query,
    uri_components(http,'brk.kadaster.nl','/sparql',_,_)
  ).



load_kadaster:-
  absolute_file_name(kadaster, File, [access(read)]),
  rdf_load_any(File, [format(turtle),graph(kadaster)]).

query_kadaster(Resources):-
  sparql_select(kadaster, [], [s,p,o],
      [rdf(var(s),var(p),var(o))], Resources, [distinct(true),limit(10)]).

query_kadaster(S, Resources):-
  sparql_select(kadaster, [], [p,o],
      [rdf(iri(S),var(p),var(o))], Resources, [distinct(true),limit(10)]).

scrape_kadaster:-
  sparql_select(kadaster, [], [s,p,o],
      [rdf(var(s),var(p),var(o))], Rows, [distinct(true)]),
  maplist(assert_row_as_triple, Rows),
  rdf_save_any(kadaster, [format(turtle),graph(kadaster)]).

assert_row_as_triple(row(S,P,O)):-
  rdf_assert(S, P, O, kadaster).

