:- module(
  lov,
  [
    lov_resources/1 % -Pairs:ordset(pair(atom))
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

:- use_module(xml(xml_namespace)).

:- xml_register_namespace(voaf, 'http://purl.org/vocommons/voaf#').



lov_resources(Pairs):-
  lov_url(Url),
  rdf_transaction(
    (
      rdf_load(Url, []),
      aggregate_all(
        set(Resource-Resource),
        rdfs_individual_of(Resource, voaf:'Vocabulary'),
        Pairs
      )
    ),
    _,
    [snapshot(true)]
  ).


lov_url('http://lov.okfn.org/dataset/lov/').

