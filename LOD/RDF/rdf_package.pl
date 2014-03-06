:- module(
  rdf_package,
  [
    rdf_package_build/3, % +Options:list(nvpair)
                         % +RdfDataset:compound
                         % +Archive:atom
    void_package_build/3 % +Options:list(nvpair)
                         % +VoidGraph:atom
                         % +Archive:atom
  ]
).

/** <module> RDF Package

Packaging of RDF datasets

@author Wouter Beek
@version 2014/03
*/

:- use_module(os(archive_ext)).
:- use_module(rdf(rdf_dataset)).
:- use_module(void(void_db)).



rdf_package_build(O1, RdfDataset, Archive):-
  rdf_assert_dataset(RdfDataset),
  rdf_default_graph(RdfDataset, VoidGraph),
  void_package_build(O1, VoidGraph, Archive).


void_package_build(O1, VoidGraph, Archive):-
  void_save(O1, VoidGraph, VoidFile),
  findall(
    VoidDatasetFile,
    (
      void_dataset(VoidGraph, VoidDataset),
      void_dataset_location(VoidGraph, VoidDataset, VoidDatasetFile)
    ),
    VoidDatasetFiles
  ),
  create_archive([VoidFile|VoidDatasetFiles], Archive).

