:- module(
  void_file,
  [
    void_load/2, % +File:atom
                 % -RdfDataset:compound
    void_save/3 % +Options:list(nvpair)
                % +RdfDataset:compound
                % ?File:atom
  ]
).

/** <module> VoID file handling

Support for the Vocabulary of Interlinked Datasets (VoID).

VoID is an RDF Schema vocabulary for expressing metadata about RDF datasets.
It is intended as a bridge between the publishers and users of RDF data,
with applications ranging from data discovery to cataloging and archiving
of datasets.

VoID is a W3C Interest Group Note as of 2011/03/03.

VoiD covers four areas of metadata:
  * *|General metadata|* following the Dublin Core model.
  * *|Access metadata|* describes how RDF data can be accessed
  *  using various protocols.
  * *|Structural metadata|* describes the structure and schema of datasets
    and is useful for tasks such as querying and data integration.
  * *|Description of links between datasets|* are helpful
    for understanding how multiple datasets are related.

@author WouterBeek
@compat http://www.w3.org/TR/void/
@version 2013/03-2013/05, 2013/09-2013/11, 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).
:- use_module(void(void_db)). % XML namespace.
:- use_module(void(void_stat)).

:- initialization(void_init).

%! void_init is det.
% Loads the VoID vocabulary.

void_init:-
  rdf_new_graph(void_schema, Graph),
  absolute_file_name(void('VoID'), File, [access(read),file_type(turtle)]),
  rdf_load([file_type(turtle)], Graph, File).



%! void_graph_rdf_dataset(+VoidGraph:atom, -RdfDataset:compound) is nondet.
% Translates between VoID graphs and the RDF datasets described by them.

void_graph_rdf_dataset(VoidGraph, rdf_dataset(VoidGraph, VoidDatasets)):-
  % Typecheck.
  rdf_graph(VoidGraph),

  setoff(
    VoidDataset,
    rdf(VoidDataset, rdf:type, void:'Dataset', VoidGraph),
    VoidDatasets
  ).


%! void_load(+File:atom, -RdfDataset:compound) is det.
% Loads a VoID file and all the datasets defined in it.
%
% Also calculates VoID statistics for all datasets and asserts those
%  in the VoID file.
%
% @arg File The atomic name of the absolute file path of a VoID file.

% The RDF graph already exists.
void_load(File, _):-
  rdf_graph_property(_, source(File)), !,
  print_message(warning, 'Cannot load VoID file. File already loaded.').
void_load(File, RdfDataset):-
  rdf_load([], VoidGraph, File),
  file_to_directory(File, Directory),
  void_graph_rdf_dataset(VoidGraph, RdfDataset),
  RdfDataset = rdf_dataset(VoidGraph, VoidDatasets),
  
  % Each dataset is loaded in a separate thread.
  forall_thread(
    (
      member(VoidDataset, VoidDatasets),
      format(atom(Msg), 'Loading dataset ~w.', [VoidDataset])
    ),
    void_load_dataset(Directory, VoidDataset),
    void_file,
    Msg
  ).


%! void_load_dataset(+Directory:atom, +VoidDataset:iri) is det.

void_load_dataset(Directory, VoidDataset):-
  % Every dataset has exactly one datadump property.
  % @tbd Is this assumption correct?l
  once(rdf(VoidDataset, void:dataDump, DatadumpLocation)),
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
    relative_file_path(DatadumpFile, Directory, DatadumpLocation)
  ),

  rdf_load([], VoidDataset, DatadumpFile).


%! void_save(+Options:list(nvpair), +RdfDataset:compound, ?File:atom) is det.

void_save(O1, RdfDataset, File):-
  % First save all datasets that are described in the given VoID graph.
  forall_thread(
    (
      rdf_named_graph(RdfDataset, VoidDataset),
      format(atom(Msg), 'Saving VoID dataset ~w.', [VoidDataset])
    ),
    rdf_save(O1, VoidDataset, File),
    void_file,
    Msg
  ),
  % Then save the VoID graph itself.
  rdf_default_graph(RdfDataset, DefaultGraph),
  rdf_save([format(turtle)], DefaultGraph, File).


void_update_library(G):-
  forall_thread(
    (
      void_dataset(G, DS, _DS_F, DS_G),
      format(atom(Msg), 'Updating dataset ~w', [DS])
    ),
    (
      void_assert_modified(G, DS),
      void_assert_statistics(G, DS, DS_G)
    ),
    void_file,
    Msg
  ).

