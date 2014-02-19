:- module(
  ckan_to_rdf,
  [
    ckan_to_rdf/1 % +Options:list(nvpair)
  ]
).

/** <module> CKAN to RDF conversion

Automated CKAN to RDF conversion.

# Licenses

Based on the datahub.io JSON scrape we end up with 18 (out of 33) licenses
 that are underdefined (i.e., with no semantic description),
 impacting 207 datasets (5%).
Using case-insensitive matching of the license string with
 the repository of OpenDefinition/OKF license descriptions
 we are able to add descriptions for 14 underdefined licenses
 and additional properties for 4 licenses that were already defined.
We manually assert three additional identities in order
 to identify 1 underdefined license as a typographic variant of
 a defined license (this is `cc-by`) and to identify the remaining
 2 underdefined licenses as `ckan:None`.
After these operations all 4053 datasets have fully described
 licensing conditions.

| *License* | *Number of RDF datasets* | *Number of triples from `datahub.io`* | *Additional number of triples from OpenDefinition* | *`owl:sameAs` assertions |
| ckan:License/6                                | 1   | 1  |   | ckan:License/None  |
| ckan:License/65                               | 1   | 1  |   | ckan:License/None  |
| ckan:License/CreativeCommonsAttributionCCBY25 | 1   | 1  |   | ckan:License/cc-by |
| ckan:License/None                             | 12  | 1  |   |                    |
| ckan:License/W3C                              | 2   | 1  | 9 |                    |
| ckan:License/apache                           | 1   | 1  | 8 |                    |
| ckan:License/bsd-license                      | 3   | 1  | 9 |                    |
| ckan:License/canada-crown                     | 6   | 1  | 8 |                    |
| ckan:License/cc-by                            | 364 | 11 | 1 |                    |
| ckan:License/cc-by-sa                         | 233 | 11 |   |                    |
| ckan:License/cc-nc                            | 187 | 11 |   |                    |
| ckan:License/cc-zero                          | 203 | 11 | 1 |                    |
| ckan:License/geogratis                        | 100 | 1  | 9 |                    |
| ckan:License/gfdl                             | 41  | 11 |   |                    |
| ckan:License/gpl-2.0                          | 22  | 1  | 9 |                    |
| ckan:License/gpl-3.0                          | 3   | 1  | 9 |                    |
| ckan:License/lgpl-2.1                         | 2   | 1  | 9 |                    |
| ckan:License/mit-license                      | 4   | 1  | 9 |                    |
| ckan:License/notspecified                     | 985 | 10 |   |                    |
| ckan:License/odc-by                           | 68  | 11 |   |                    |
| ckan:License/odc-odbl                         | 118 | 11 |   |                    |
| ckan:License/odc-pddl                         | 170 | 11 | 1 |                    |
| ckan:License/other-at                         | 85  | 10 |   |                    |
| ckan:License/other-closed                     | 189 | 10 |   |                    |
| ckan:License/other-nc                         | 69  | 10 |   |                    |
| ckan:License/other-open                       | 855 | 10 |   |                    |
| ckan:License/other-pd                         | 221 | 10 |   |                    |
| ckan:License/real                             | 1   | 1  | 9 |                    |
| ckan:License/sunpublic                        | 1   | 1  | 9 |                    |
| ckan:License/uk-ogl                           | 68  | 11 | 3 |                    |
| ckan:License/ukclickusepsi                    | 22  | 1  | 8 |                    |
| ckan:License/ukcrown                          | 11  | 1  | 8 |                    |
| ckan:License/ukcrown-withrights               | 14  | 1  | 8 |                    |

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ckan(ckan_api)).
:- use_module(ckan(opendefinition_licenses)).
:- use_module(generics(list_script)).
:- use_module(library(debug)).
:- use_module(owl(owl_build)).



%! ckan_to_rdf(+Options:list(nvpair)) is det.
% @see Options are passed to the predicates in module [ckan].
ckan_to_rdf(O_RDF):-
  % Conversion to RDF requires presence of graph option.
  % Conversion to PL requires absence of graph option.
  select_option(graph(Graph), O_RDF, O_PL),
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
  % Enrich the licenses with information from OpenDefinition/OKF.
  enrich_licenses(Graph),
  owl_assert_resource_identity(ckan:'License/6', ckan:'License/None', Graph),
  owl_assert_resource_identity(ckan:'License/65', ckan:'License/None', Graph),
  owl_assert_resource_identity(
    ckan:'License/CreativeCommonsAttributionCCBY25',
    ckan:'License/cc-by',
    Graph
  ),
  
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
  
  debug(ckan, 'End CKAN-to-RDF conversion.\n', []).

