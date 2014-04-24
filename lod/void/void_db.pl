:- module(
  void_db,
  [
    void_dataset_location/3, % +VoidGraph:atom
                             % +VoidDataset:iri
                             % -DatadumpFile:atom
    void_dataset/2, % +VoidGraph:atom
                    % -VoidDataset:iri
    void_rdf_load_any/3 % +Options:list(nvpair)
                        % +Input
                        % -Pairs:list(pair(atom))
  ]
).

/** <module> VoID DB

Generic support for VoID, used by other VoID modules.

@author Wouter Beek
@version 2013/11, 2014/03-2014/04
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(typecheck)).
:- use_module(http(http_download)).
:- use_module(os(file_ext)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').

:- initialization(void_init(_)).

%! void_init(?RdfGraph:atom) is det.
% Loads the VoID vocabulary.

void_init(G):-
  void_url(Url),
  (
    nonvar(G)
  ->
    rdf_load_any([graph(G)], Url)
  ;
    rdf_load_any([], Url, [_-G])
  ).



%! void_graph_datasets(+VoidGraph:atom, -VoidDataset:iri) is semidet.
% Translates between VoID graphs and the RDF datasets described by them.

void_dataset(VoidGraph, VoidDataset):-
  rdfs_individual_of(VoidDataset, void:'Dataset'),
  once(rdf_term(VoidDataset, VoidGraph)).


void_dataset_location(VoidGraph, VoidDataset, DatadumpFile):-
  % Every dataset has exactly one datadump property.
  % @tbd Is this assumption correct?l
  rdf(VoidDataset, void:dataDump, DatadumpLocation, VoidGraph),
  (
    is_of_type(iri, DatadumpLocation)
  ->
    % Store locally.
    download_to_file([], DatadumpLocation, DatadumpFile)
  ;
    is_absolute_file_name(DatadumpLocation)
  ->
    DatadumpFile = DatadumpLocation
  ;
    rdf_graph_property(VoidGraph, source(VoidFile)),
    file_name(VoidFile, VoidDirectory, _, _),
    relative_file_path(DatadumpFile, VoidDirectory, DatadumpLocation)
  ).


void_rdf_load_any(O1, Input, Pairs):-
  void_rdf_load_any(O1, [Input], [], Pairs).


% Done!
void_rdf_load_any(_, [], Pairs, Pairs):- !.
% Add another URL to the TODO list.
void_rdf_load_any(O1, Todo, Pairs, Done):-
  member(_-Graph, Pairs),
  void_dataset(Graph, Dataset),
  void_dataset_location(Graph, Dataset, H),
  \+ member(H-_, Pairs), !,
gtrace,
  void_rdf_load_any(O1, [H|Todo], Pairs, Done).
void_rdf_load_any(O1, [H|Todo], Pairs1, Done):-
  rdf_load_any(O1, H, Pairs2),
  union(Pairs1, Pairs2, Pairs3),
  void_rdf_load_any(O1, Todo, Pairs3, Done).


void_url('http://vocab.deri.ie/void.ttl').

