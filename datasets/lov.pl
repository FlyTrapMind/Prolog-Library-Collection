:- module(
  lov,
  [
    lov_resources/1 % -Iris:ordset(iri)
  ]
).

/** <module> Linked Open Vocabularies

Support for the OKF-managed list of open vocabularies.

@author Wouter Beek
@see http://lov.okfn.org/dataset/lov/
@version 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).


:- rdf_register_prefix(voaf, 'http://purl.org/vocommons/voaf#').



lov_resources(Iris):-
  lov_url(Url),
  rdf_transaction(
    (
      rdf_load(Url, []),
      aggregate_all(
        set(Iri),
        rdfs_individual_of(Iri, voaf:'Vocabulary'),
        Iris
      )
    ),
    _,
    [snapshot(true)]
  ).


lov_url(Url):-
  uri_components(
    Url,
    uri_components(http,'lov.okfn.org','/dataset/lov/lov.rdf',_,_)
  ).

