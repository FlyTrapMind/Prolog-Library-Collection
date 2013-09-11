:- module(
  void,
  [
    rdf_link/5, % ?Subject:oneof([bnode,uri])
                % ?Predicate:uri
                % ?Object:oneof([bnode,literal,uri])
                % ?FromGraph:atom
                % ?ToGraph:atom
    rdf_linkset/3, % ?Triples:list(rdf_triples)
                   % ?FromGraph:atom
                   % ?ToGraph:atom
    void_load_library/3, % +VoID_File:atom
                         % ?VoID_GraphPreference:atom
                         % -VoID_Graph:atom
    void_save_library/2 % +VoID_Graph:atom
                        % ?VoID_File:atom
  ]
).

/** <module> VoID

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
@tbd Use forall_thread/2 for the threaded parts.
@version 2013/03-2013/05, 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_statistics)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').

:- dynamic(dataset/4).

:- debug(void).



load_dataset(Dataset):-
  once(rdf(Dataset, void:dataDump, FileName)),
  absolute_file_name(data(FileName), File, [access(read)]),
  rdf_global_id(_Namespace:LocalName, Dataset),
  rdf_load(File, [graph(LocalName)]).

%! rdf_link(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?FromGraph:atom,
%!   ?ToGraph:atom
%! ) is nondet.
% An RDF link is an RDF triple whose subject and object are described in
% different datasets.

rdf_link(Subject, Predicate, Object, FromGraph, ToGraph):-
  rdf(Subject, Predicate1, Object1, FromGraph),
  \+ ((
    Predicate1 == Predicate,
    Object1 == Object
  )),
  rdf(Object, Predicate2, Object2, ToGraph),
  \+ ((
    Predicate2 == Predicate,
    Object2 == Object
  )),
  FromGraph \== ToGraph.

%! rdf_linkset(+Triples:list(compound), ?FromGraph:atom, ?ToGraph:atom) is semidet.
%! rdf_linkset(-Triples:list(compound), +FromGraph:atom, +ToGraph:atom) is det
% An RDF linkset is a collection of RDF links between the same two datasets.

rdf_linkset(Triples, FromGraph, ToGraph):-
  nonvar(Triples), !,
  forall(
    member(row(Subject, Predicate, Object), Triples),
    rdf_link(Subject, Predicate, Object, FromGraph, ToGraph)
  ).
rdf_linkset(Triples, FromGraph, ToGraph):-
  findall(
    row(Subject, Predicate, Object),
    rdf_link(Subject, Predicate, Object, FromGraph, ToGraph),
    Triples
  ).

%! void_assert_modified(
%!   +Dataset:atom,
%!   +DatasetPath:atom,
%!   +VoID_Graph:atom
%! ) is det.
% Asserts a new modification date.

void_assert_modified(Dataset, DatasetPath, VoID_Graph):-
  time_file(DatasetPath, LastModified),
  rdf_overwrite_datatype(
    Dataset,
    dc:modified,
    dateTime,
    LastModified,
    VoID_Graph
  ).

void_assert_statistics(Dataset, DatasetGraph, VoID_Graph):-
  % void:classes
  count_classes(DatasetGraph, NumberOfClasses),
  rdf_overwrite_datatype(
    Dataset,
    void:classes,
    integer,
    NumberOfClasses,
    VoID_Graph
  ),

  % void:distinctObjects
  count_objects(_, _, DatasetGraph, NumberOfObjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctObjects,
    integer,
    NumberOfObjects,
    VoID_Graph
  ),

  % void:distinctSubjects
  count_subjects(_, _, DatasetGraph, NumberOfSubjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctSubjects,
    integer,
    NumberOfSubjects,
    VoID_Graph
  ),

  % void:properties
  count_properties(_, _, DatasetGraph, NumberOfProperties),
  rdf_overwrite_datatype(
    Dataset,
    void:properties,
    integer,
    NumberOfProperties,
    VoID_Graph
  ),

  % void:triples
  rdf_statistics(triples_by_graph(DatasetGraph, NumberOfTriples)),
  rdf_overwrite_datatype(
    Dataset,
    void:triples,
    integer,
    NumberOfTriples,
    VoID_Graph
  ).

%! void_load_library(
%!   +VoID_File:atom,
%!   +VoID_GraphPref:atom,
%!   -VoID_Graph:atom
%! ) is det.
% Loads a VoID file and all the datasets defined in it.
% Also calculates VoID statistics for all datasets and asserts those to
% the VoID file.
%
% @param VoID_File The atomic name of an absolute file path of a VoID file.
% @param VoID_GraphPref The preferred atomic name of the VoID graph.
% @param VoID_Graph The actual atomic name of the VoID graph.

void_load_library(VoID_File, VoID_G1, VoID_G2):-
  % Type checks,
  nonvar(VoID_File),
  exists_file(VoID_File),
  access_file(VoID_File, read), !,

  % Clear the internal database.
  retractall(dataset(_, _, _, _)),

  % Make sure the graph name does not already exist,
  % and is close to the preferred grah name (when given).
  (
    var(VoID_G1)
  ->
    file_to_graph_name(VoID_File, VoID_G2)
  ;
    rdf_new_graph(VoID_G1, VoID_G2)
  ),

  % Load the VoID file in a new graph (see above).
  rdf_load2(VoID_File, [graph(VoID_G2)]),
  debug(void, 'VoID file ~w loaded into graph ~w.', [VoID_File,VoID_G2]),

  % Load the VoID vocabulary into the same graph.
  rdf_new_graph(void_schema, VoID_SchemaG),
  absolute_file_name(
    vocabularies('VoID'),
    VoID_SchemaFile,
    [access(read),file_type(turtle)]
  ),
  rdf_load2(VoID_SchemaFile, [file_type(turtle),graph(VoID_SchemaG)]),

  % We assume that the datasets that are mentioned in the VoID file
  % are reachable from that VoID file's directory.
  file_directory_name(VoID_File, VoID_Dir),

  % All datasets are loaded in multiple threads.
  forall_thread(
    (
      % This includes VoID linksets, accordiung to the VoID vocabulary.
      rdfs_individual_of(Dataset, void:'Dataset'),
      rdf(Dataset, void:dataDump, DatasetRelFile, VoID_G2),
      % Possibly relative file paths are made absolute.
      directory_file_path(VoID_Dir, DatasetRelFile, DatasetAbsFile),
      % The graph name is derived from the file name.
      file_to_graph_name(DatasetAbsFile, DatasetG),
      format(atom(Msg), 'Loading graph ~w', [DatasetG])
    ),
    (
      rdf_load2(DatasetAbsFile, [graph(DatasetG)]),
      assert(dataset(VoID_G2, Dataset, DatasetAbsFile, DatasetG))
    ),
    void,
    Msg
  ),
  
  % The VoID graph itself is updated.
  void_update_library(VoID_G2),

  % The updated VoID graph is stored to the same file
  % that it was loaded from.
  rdf_save2(VoID_File, [graph(VoID_G2)]).

%! void_save_library(+VoID_Graph:atom, ?VoID_File:atom) is det.

void_save_library(VoID_G, VoID_File):-
  forall_thread(
    (
      dataset(VoID_G, _Dataset, DatasetPath, DatasetG),
      format(atom(Msg), 'Saving graph ~w', [DatasetG])
    ),
    rdf_save2(DatasetPath, [format(turtle),graph(DatasetG)]),
    void,
    Msg
  ),
  rdf_save2(VoID_File, [format(turtle),graph(VoID_G)]).

void_update_library(VoID_Graph):-
  forall_thread(
    (
      dataset(VoID_Graph, Dataset, DatasetPath, DatasetGraph),
      format(atom(Msg), 'Updating dataset ~w', [Dataset])
    ),
    (
      void_assert_modified(Dataset, DatasetPath, VoID_Graph),
      void_assert_statistics(Dataset, DatasetGraph, VoID_Graph)
    ),
    void,
    Msg
  ).

