:- module(
  void_build,
  [
     void_assert_datadump/3, % +Subject:or([bnode,iri])
                             % +DataDump:url
                             % +Graph:atom
     void_assert_dataset/2 % +Subject:or([bnode,iri])
                           % +Graph:atom
  ]
).

/** <module> VoID build

Building VoID descriptions.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(vocabularies(dc)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').

:- rdf_meta(void_assert_datadump(r,+,+)).
:- rdf_meta(void_assert_dataset(r,+)).



%! void_assert_datadump(
%!   +Subject:or([bnode,iri]),
%!   +DataDump:url,
%!   +Graph:atom
%! ) is det.

void_assert_datadump(R, Dump, G):-
  rdf_assert(R, void:dataDump, Dump, G).

%! void_assert_dataset(+Subject:or([bnode,iri]), +Graph:atom) is det.

void_assert_dataset(R, G):-
  rdf_assert_individual(R, void:'Dataset', G).

