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

:- use_module(ckan(ckan_api)).
:- use_module(generics(list_script)).
:- use_module(library(debug)).



%! ckan_to_rdf(+Options:list(nvpair)) is det.
% @see Options are passed to the predicates in module [ckan].
ckan_to_rdf(O_RDF):-
  % Conversion to RDF requires presence of graph option.
  % Conversion to PL requires absence of graph option.
  select_option(graph(_), O_RDF, O_PL),
  debug(ckan, 'Begin CKAN-to-RDF conversion.\n', []),

  % Make sure the CKAN site is online.
  site_read(O_PL), !,

/*
  % Groups
  group_list(O_PL, _, _, _, _, Groups),
  list_script(group_show(O_RDF), 'Groups', Groups, GroupsRemaining),
  debug(ckan, 'Remaining groups: ~w', [GroupsRemaining]),
*/

  % Licenses.
  license_list(O_RDF, _),

  % Organizations.
  organization_list(O_PL, _, _, _, _, Organizations),
  list_script(
    organization_show(O_RDF),
    'Organizations',
    Organizations,
    RemainingOrganizations
  ),
  debug(ckan, 'Remaining organizations: ~w', [RemainingOrganizations]),

  % Packages.
  package_list(O_PL, _, _, Packages),
  list_script(package_show(O_RDF), 'Packages', Packages, RemainingPackages),
  debug(ckan, 'Remaining packages: ~w', [RemainingPackages]),

/*
  % Revisions.
  % @tbd The request for revisions crashes the site.
  revision_list(O_PL, Revisions),
  list_script(revision_show(O_RDF), 'Revisions', Revisions, RemainingRevisions),
  debug(ckan, 'Remaining revisions: ~w', [RemainingRevisions]),
*/

/*
  % Tags.
  tag_list(O_PL, _, _, _, Tags),
  list_script(tag_show(O_RDF), 'Tags', Tags, RemainingTags),
  debug(ckan, 'Remaining tags: ~w', [RemainingTags]),
*/

/*
  % Users.
  % @tbd Why does this now work?
  user_list(O_PL, _, _, Users),
  list_script(user_show(O_RDF, _), 'Users', Users, RemainingUsers),
  debug(ckan, 'Remaining users: ~w', [RemainingUsers]),
*/

  format(current_output, 'End CKAN-to-RDF conversion.\n', []).

