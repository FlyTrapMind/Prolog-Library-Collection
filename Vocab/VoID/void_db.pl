:- module(
  void_db,
  [
    void_dataset/4, % ?DatasetDescriptionGraph:atom
                    % ?Dataset:iri
                    % ?DatasetFile:atom
                    % ?DatasetGraph:atom
    void_dataset_add/4, % +DatasetDescriptionGraph:atom
                        % +Dataset:iri
                        % +DatasetFile:atom
                        % +DatasetGraph:atom
    void_dataset_remove/1 % +DatasetDescriptionGraph:atom
  ]
).

/** <module> VoID DB

Persistent store for VoID datasets.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(error)).
:- use_module(library(persistency)).

:- persistent(void_dataset_(
  dataset_description_graph:atom,
  dataset:atom,
  dataset_file:atom,
  dataset_graph:atom
)).



%! void_dataset(
%!   ?DatasetDescriptionGraph:atom,
%!   ?Dataset:iri,
%!   ?DatasetFile:atom,
%!   ?DatasetGraph:atom
%! ) is nondet.
% These assertions make it easy to read/write from/to the description of
% a dataset in the VoID graph based on information that is in the dataset
% graph.
%
% Keeping the file names makes it easy to store the VoID graph to
% a package that includes all the relevant files.
%
% @arg DatasetDescriptionGraph The atomic name of a dataset description.
% @arg Dataset The IRI denoting an RDF dataset.
% @arg DatasetFile The atomic name of a file dump of an RDF dataset.
% @arg DatasetGraph The atomic name of an RDF dataset.
%
% @tbd What is an RDF graph and what is an RDF dataset?

void_dataset(DD_G, DS, DS_F, DS_G):-
  void_dataset_(DD_G, DS, DS_F, DS_G).

%! void_dataset_add(
%!   +DatasetDescriptionGraph:atom,
%!   +Dataset:iri,
%!   +DatasetFile:atom,
%!   +DatasetGraph:atom
%! ) is nondet.
% Adds a VoID dataset to the VoID database.
%
% @arg DatasetDescriptionGraph The atomic name of a dataset description.
% @arg Dataset The IRI denoting an RDF dataset.
% @arg DatasetFile The atomic name of a file dump of an RDF dataset.
% @arg DatasetGraph The atomic name of an RDF dataset.
%
% @tbd What is an RDF graph and what is an RDF dataset?

void_dataset_add(DD_G, DS, DS_F, DS_G):-
  with_mutex(void_db, assert_void_dataset_(DD_G, DS, DS_F, DS_G)).

void_dataset_remove(DD_G):-
  with_mutex(
    void_db,
    (
      void_dataset(DD_G, _, _, _), !,
      retractall_void_dataset_(DD_G, _, _, _)
    )
  ).
void_dataset_remove(DD_G):-
  existence_error(void_db, DD_G).

