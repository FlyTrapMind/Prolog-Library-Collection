:- module(
  rdf_dataset,
  [
    rdf_create_dataset/3, % +DefaultGraph:atom
                          % +NamedGraphs:list(pair(atom))
                          % -Dataset:compound
    rdf_dataset/2, % ?DefaultGraph:atom
                   % ?NamedGraphs:list(kvpair)
    rdf_dataset_graph/2, % +RDF_Dataset:compound
                         % -Graph:atom
    rdf_default_graph/2, % +Dataset:compound
                         % -DefaultGraph:atom
    rdf_named_graph/3 % +Dataset:compound
                      % +Name:atom
                      % -NamedGraph:atom
  ]
).

/** <module> HIVES

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- dynamic(rdf_dataset/2).



%! rdf_create_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphs:list(pair(atom)),
%!   -Dataset:compound
%! ) is det.

rdf_create_dataset(DG, NGs, rdf_dataset(DG,NGs)):-
  pairs_keys_values(NGs, Ks, Vs),
  maplist(atom, Ks),
  maplist(rdf_graph, Vs), !,
  db_add_novel(rdf_dataset(DG, NGs)).

%! rdf_dataset_graph(+RDF_Dataset:compound, -Graph:atom) is nondet.

rdf_dataset_graph(DS, DG):-
  rdf_default_graph(DS, DG).
rdf_dataset_graph(DS, NG):-
  rdf_named_graph(DS, _N, NG).

rdf_default_graph(rdf_dataset(DG,_NGs), DG).

rdf_named_graph(rdf_dataset(_DG,NGs), N, NG):-
  member(N-NG, NGs).

