:- module(
  void_file,
  [
    void_load_library/2, % +File:atom
                         % +Graph:atom
    void_save_library/2 % +Graph:atom
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
@version 2013/03-2013/05, 2013/09-2013/11
*/

:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).
:- use_module(void(void_db)).
:- use_module(void(void_stat)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').

:- initialization(void_init).

%! void_init is det.
% Loads the VoID vocabulary.

void_init:-
  rdf_new_graph(void_schema, Graph),
  absolute_file_name(void('VoID'), File, [access(read),file_type(turtle)]),
  rdf_load([file_type(turtle)], Graph, File).



%! void_load_dataset(+File:atom, +Graph:atom, +Dataset:iri) is det.
% @arg File The atomic name of the file that contains the
%      dataset description.
% @arg Graph The atomic name of the RDF graph that contains
%      the description.
% @arg Dataset An IRI denoting a dataset.

void_load_dataset(File, Graph, Dataset):-
  % Every dataset has exactly one datadump property.
  % @tbd Is this assumption correct?
  once(rdf(DS, void:dataDump, Dump)),

  % @tbd Extend this to other cases: absolute files, generic relative files,
  %      Web locations.
  file_directory_name(DD_F, DD_Dir),
  absolute_file_name(Dump, DS_F, [access(read),relative_to(DD_Dir)]),

  % The RDF graph name into which the sataset is loaded is derived from
  % its IRI.
  rdf_global_id(_Ns:DS_G, DS),

  rdf_load([], DS_G, DS_F),

  % Update the internally stored relations between VoID graphs and
  %  dataset graphs.
  void_dataset_add(DD_G, DS, DS_F, DS_G).


%! void_load_library(+File:atom) is det.
% Loads a VoID file and all the datasets defined in it.
%
% Also calculates VoID statistics for all datasets and asserts those
%  in the VoID file.
%
% @arg File The atomic name of the absolute file path of a VoID file.

% The RDF graph already exists.
void_load_library(File):-
  rdf_graph_property(Graph, source(File(G))), !,
  print_message(warning, 'Cannot load VoID file. File already loaded.').
void_load_library(File):-
  rdf_load([], Graph, File),
  
  % Each dataset is loaded in a separate thread.
  forall_thread(
    (
      % This includes VoID linksets, according to the VoID vocabulary.
      % @tbd Add graph argument.
      rdfs_individual_of(Dataset, void:'Dataset'),
      format(atom(Msg), 'Loading dataset ~w.', [Dataset])
    ),
    void_load_dataset(File, Graph, Dataset),
    void_file,
    Msg
  ).


%! void_save_library(+Graph:atom, ?File:atom) is det.
% @tbd Add meta-data updates.

void_save_library(G, F):-
  % First save all datasets that are described in the given VoID graph.
  forall_thread(
    (
      void_dataset(G, _DS, DS_F, DS_G),
      format(atom(Msg), 'Saving graph ~w.', [DS_G])
    ),
    rdf_save([format(turtle)], DS_G, DS_F),
    void_file,
    Msg
  ),
  % Then save the VoID graph itself.
  rdf_save([format(turtle)], G, F).

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

