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

:- use_module(ckan(ckan)).
:- use_module(library(debug)).
:- use_module(library(lists)).



%! ckan_to_rdf(+Options:list(nvpair)) is det.
% @see Options are passed to the predicates in module [ckan].

ckan_to_rdf(O1):-
  format(current_output, 'Begin CKAN-2-RDF conversion.\n', []),

  % Make sure the CKAN site is online.
  site_read(O1), !,

  % Groups
  group_list(O1, _, _, _, _, Groups),
  length(Groups, LG),
  forall(
    nth1(I, Groups, Group),
    (
      debug(ckan, 'Groups ~w/~w', [I,LG]),
      group_show(O1, Group, _),
      group_package_show(O1, Group, _, _)
    )
  ),

  % Licenses.
  license_list(O1, _),

  % Organizations.
  organization_list(O1, _, _, _, _, Organizations),
  length(Organizations, LO),
  forall(
    nth1(I, Organizations, Organization),
    (
      debug(ckan, 'Organizations ~d/~d', [I,LO]),
      organization_show(O1, Organization, _)
    )
  ),

  % Packages.
  package_list(O1, _, _, Packages),
  length(Packages, LP),
  forall(
    nth1(I, Packages, Package),
    (
      debug(ckan, 'Packages ~d/~d', [I,LP]),
      package_show(O1, Package, _)
    )
  ),

  % Revisions.
  revision_list(O1, Revisions),
  length(Revisions, LR),
  forall(
    nth1(I, Revisions, Revision),
    (
      debug(ckan, 'Revisions ~d/~d', [I,LR]),
      revision_show(O1, Revision, _)
    )
  ),

  % Tags.
  tag_list(O1, _, _, _, Tags),
  length(Tags, LT),
  forall(
    nth1(I, Tags, Tag),
    (
      debug(ckan, 'Tags ~d/~d', [I,LT]),
      tag_show(O1, Tag, _)
    )
  ),

  % Users.
  user_list(O1, _, _, Users),
  length(Users, LU),
  forall(
    nth1(I, Users, User),
    (
      debug(ckan, 'Users ~d/~d', [I,LU]),
      user_show(O1, _, User, _)
    )
  ),

  format(current_output, 'End CKAN-2-RDF conversion.\n', []).

