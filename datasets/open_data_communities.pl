:- module(
  open_data_communities,
  [
    open_data_communities_urls/1 % -Urls:list(url)
  ]
).

/** <module> Open Data Communities

@author Wouter Beek
@version 2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ser(rdf_load_any)).



%! open_data_communities_urls(-Urls:ordset(url)) is det.
% @tbd Add pagination.

open_data_communities_urls(Urls):-
  setup_call_cleanup(
    rdf_load_any('http://opendatacommunities.org/data.ttl', [graph(Graph)]),
    aggregate_all(
      set(Url),
      rdf(_, void:dataDump, Url, Graph),
      Urls
    ),
    rdf_unload_graph(Graph)
  ).

