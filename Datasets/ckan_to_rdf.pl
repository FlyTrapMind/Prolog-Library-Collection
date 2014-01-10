:- module(
  ckan_to_rdf,
  [
    ckan_to_rdf/1 % +Options:list(nvpair)
  ]
).

/** <module> CKAN to RDF conversion

Automated CKAN to RDF conversion.

@author Wouter Beek
@version 2014/01
*/

:- use_module(datasets(ckan)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(prolog_to_rdf)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(json_ext)).



ckan_to_rdf(O1):-
  merge_options([output(rdf)], O1, O2),
  
  % DEB
  flag(aap, _, 0),
  
  % Make sure the CKAN site is online.
  site_read(O1), !,
  
  % Licenses.
  license_list(O2, _),
  
  % Packages.
  package_list(O2, _, _, PackageNames),
  forall(
    member(PackageName, PackageNames),
    package_show(O2, PackageName, _)
  ),
  
  true.



/*
  % Groups.
  group_list(O1, true, _, _, _, Groups1),
  json_to_rdf(Graph, ckan, Groups1),
  
  % Group members.
  group_list(O1, false, _, _, _, Groups2),
  forall(
    member(Group, Groups2),
    (
      member_list(O1, _, Group, _, Triples),
      write(Triples)
    )
  ),
  
  % Organizations.
  organization_list(O1, true, _, _, _, Organizations),
  json_to_rdf(Graph, ckan, Organizations),
  
  % Packages.
  package_list([], _, _, Packages),
  rdf_global_id(ckan:'Package', PackageClass),
  rdfs_assert_class(PackageClass, Graph),
  forall(
    member(Package1, Packages),
    (
      rdf_global_id(ckan:Package1, Package2),
      rdf_assert_individual(Package2, PackageClass, Graph)
    )
  ),
  
  % Package related.
  forall(
    member(Package, Packages),
    (
      related_list(O1, _, _, Package, _, _, Related),
      json_to_rdf(Graph, ckan, Related)
    )
  ),
  
  % Package revisions.
  forall(
    member(Package, Packages),
    (
      package_revision_list(O1, Package, Revisions),
      json_to_rdf(Graph, ckan, Revisions)
    )
  ),
  
  % Licenses.
  license_list(O1, Licenses),
  json_to_rdf(Graph, ckan, Licenses).
*/

