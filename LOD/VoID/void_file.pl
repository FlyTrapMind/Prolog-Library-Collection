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

:- use_module(generics(thread_ext)).
:- use_module(library(semweb/rdf_db)).
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



%! void_load(+File:atom, ?VoidGraph:atom) is det.
% Loads a VoID file and all the datasets defined in it.
%
% Also calculates VoID statistics for all datasets and asserts those
%  in the VoID file.
%
% @arg File The atomic name of the absolute file path of a VoID file.
% @VoidGraph

% The RDF graph already exists.
void_load(File, VoidGraph):-
  rdf_graph_property(VoidGraph, source(File)), !,
  print_message(warning, 'Cannot load VoID file. File already loaded.').
void_load(File, VoidGraph):-
  rdf_load([], VoidGraph, File),

  % NO THREADS:
  forall(
    void_dataset(VoidGraph, VoidDataset),
    void_load_dataset(VoidGraph, VoidDataset)
  ).
/*
  % THREADS:
  forall_thread(
    (
      void_dataset(VoidGraph, VoidDataset),
      format(atom(Msg), 'Loading dataset ~w.', [VoidDataset])
    ),
    void_load_dataset(VoidGraph, VoidDataset),
    void_file,
    Msg
  ).
*/


%! void_load_dataset(+VoidGraph:atom, +VoidDataset:iri) is det.

void_load_dataset(VoidGraph, VoidDataset):-
  void_dataset_location(VoidGraph, VoidDataset, DatadumpFile),
  rdf_load([], VoidDataset, DatadumpFile).


%! void_save(+Options:list(nvpair), +VoidGraph:atom, ?File:atom) is det.

void_save(O1, VoidGraph, File):-
  % Update VoID statistics.
  void_update(VoidGraph),

  % NO THREADS
  forall(
    void_dataset(VoidGraph, VoidDataset),
    void_save_dataset(O1, VoidGraph, VoidDataset)
  ),
/*
  % THREADS
  forall_thread(
    (
      void_dataset(VoidGraph, VoidDataset),
      format(atom(Msg), 'Saving VoID dataset ~w.', [VoidDataset])
    ),
    void_save_dataset(O1, VoidGraph, VoidDataset),
    void_file,
    Msg
  ),
*/
  % Then save the VoID graph itself.
  rdf_save(O1, VoidGraph, File).


%! void_save_dataset(
%!   +Options:list(nvpair),
%!   +VoidGraph:atom,
%!   +VoidDataset:iri
%! ) is det.

void_save_dataset(O1, VoidGraph, VoidDataset):-
  void_dataset_location(VoidGraph, VoidDataset, DatadumpFile),
  rdf_save(O1, VoidDataset, DatadumpFile).

