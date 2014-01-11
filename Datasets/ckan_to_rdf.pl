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
  format(current_output, 'Begin CKAN-2-RDF conversion.\n', []),
  
  merge_options([output(rdf)], O1, O2),
  
  % Make sure the CKAN site is online.
  site_read(O1), !,
  
  % Groups
  group_list(O2, _, _, _, _, GroupNames),
  forall(
    member(GroupName, GroupNames),
    (
      group_show(O2, GroupName, _),
      group_package_show(O2, GroupName, _, _)
    )
  ),
  
  % Licenses.
  license_list(O2, _),
  
  % Organizations.
  organization_list(O2, _, _, _, _, Organizations),
  forall(
    member(Organization, Organizations),
    organization_show(O2, Organization, _)
  ),
  
  % Packages.
  package_list(O2, _, _, Packages),
  forall(
    member(Package, Packages),
    package_show(O2, Package, _)
  ),
  
  % Tags.
  tag_list(O2, _, _, _, Tags),
  forall(
    member(Tag, Tags),
    tag_show(O2, Tag, _)
  ),
  
  % Users.
  user_list(O2, _, _, Users),
  forall(
    member(User, Users),
    user_show(O2, _, User, _)
  ),
  
  format(current_output, 'End CKAN-2-RDF conversion.\n', []).

