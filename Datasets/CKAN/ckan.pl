:- module(
  ckan,
  [
    current_package_list_with_resources/4, % +Options:list(nvpair)
                                           % +Limit:positive_integer
                                           % +Offset:positive_integer
                                           % -Resources:list
    format_autocomplete/4, % +Options:list(nvpair)
                           % +Limit:integer
                           % +Q:atom
                           % -Formats:list(atom)
    group_list/6, % +Options:list(nvpair)
                  % +AllFields:boolean
                  % +Field:oneof([name,packages])
                  % +GroupNames:list(atom)
                  % +Order:atom
                  % -Groups:or([list(atom),list(compound)])
    group_list_authz/4, % +Options:list(nvpair)
                        % +AmMember:boolean
                        % +AvailableOnly:boolean
                        % -Groups:list(compound)
    group_package_show/4, % +Options:list(nvpair)
                          % +IdOrName:atom
                          % +Limit:integer
                          % -Packages:list(compound)
    group_revision_list/3, % +Options:list(nvpair),
                           % +NameOrId:atom,
                           % -Revisions:list(compound)
    group_show/3, % +Options:list(nvpair)
                  % +IdOrName:atom
                  % -Group:compound
    license_list/2, % +Options:list(nvpair)
                    % -Licenses:list(compound)
    member_list/5, % +Options:list(nvpair)
                   % +Capacity:atom
                   % +IdOrName:atom
                   % +ObjectType:atom
                   % -Triples:list(triple(atom,atom,atom))
    organization_list/6, % +Options:list(nvpair)
                         % +AllFields:boolean
                         % +Field:oneof([name,packages])
                         % +Order:atom
                         % +OrganizationsFilter:list(atom)
                         % -Organizations:list(atom)
    organization_list_for_user/3, % +Options:list(nvpair),
                                  % +Permission:atom,
                                  % -Organizations:list(compound)
    organization_show/3, % +Options:list(nvpair)
                         % +IdOrName:atom
                         % -Organization:compound
    package_autocomplete/4, % +Options:list(nvpair)
                            % +Limit:integer
                            % +Q:atom
                            % -Packages:list(compound)
    package_list/4, % +Options:list(nvpair)
                    % +Limit:integer
                    % +Offset:integer
                    % -Packages:list(atom)
    package_relationships_list/5, % +Options:list(nvpair)
                                  % +Id:atom
                                  % +Id2:atom
                                  % +Rel:atom
                                  % -Relationships:list(compound)
    package_revision_list/3, % +Options:list(nvpair)
                             % +Package:atom
                             % -Revisions:list(compound)
    package_show/3, % +Options:list(nvpair)
                    % +IdOrName:atom
                    % -Package:compound
    related_list/7, % +Options:list(nvpair)
                    % ?Dataset:compound
                    % ?Featured:boolean
                    % ?IdOrName:atom
                    % ?Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc])
                    % ?TypeFilter:atom
                    % -Related:list(compound)
    related_show/3, % +Options:list(nvpair)
                    % +Id:atom
                    % -Out:compound
    resource_show/3, % +Options:list(nvpair)
                     % +Id:atom
                     % -Resource:compound
    resource_status_show/3, % +Options:list(nvpair)
                            % +Id:atom
                            % -Statuses:list(list)
    revision_list/2, % +Options:list(nvpair)
                     % -Revisions:list(atom)
    revision_show/3, % +Options:list(nvpair)
                     % +Id:atom
                     % -Revision:compound
    site_read/1, % +Options:list(nvpair)
    tag_list/5, % +Options:list(nvpair)
                % +AllFields:boolean
                % +Query:atom
                % +VocabularyId:atom
                % -Tags:or([list(atom),list(compound)])
    tag_show/3, % +Options:list(nvpair)
                % +IdOrName:atom
                % -Tag:compound
    user_autocomplete/4, % +Options:list(nvpair)
                         % +Limit:integer
                         % +Q:atom
                         % -Users:list(compound)
    user_list/4, % +Options:list(nvpair)
                 % +OrderBy:atom
                 % +Q:atom
                 % -Users:list(compound)
    user_show/4 % +Options:list(nvpair),
                % +IdOrName:atom,
                % +UserObject:compound,
                % -User:compound
  ]
).

/** <module> CKAN

Querying the CKAN API.

The following options are API-wide supported:
  * =|deprecated(Deprecated:boolean)|=
    Use the deprecated API.
  * =|output(Format:oneof([prolog,rdf])|=
  * =|paginated(Paginated:boolean)|=
    Use pagination in order to retrieve all results.

The following depretations (v.2.0.3) are supported:
  * For: current_package_list_with_resources/4
    Use parameter name `page` i.o. `offset`.
  * For: license_list/2
    Use licence_list/2 instead.

--

@author Wouter Beek
@see http://docs.ckan.org/en/latest/api.html
@tbd The CKAN API uses `True` and `False` for boolean values.
@tbd The JSON `null` value is not replaced with a given default value.
@version 2013/11-2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(rdf_conv(json_to_rdf)).
:- use_module(standards(json_ext)).

% `Predicate:atom-Type:atom-Optional:boolean`
legend(
  '__extras',
  _,
  [
    group_id-atom-false,
    object_package_id-atom-false,
    package_id-atom-false,
    revision_id-atom-false,
    revision_timestamp-atom-false,
    subject_package_id-atom-false
  ]
).
legend(
  '_group',
  _,
  [
    capacity-atom-false,
    name-atom-false
  ]
).
legend(
  error,
  _,
  [
    '__type'-atom-false,
    message-atom-false
  ]
).
legend(
  extra,
  _,
  [
    '__extras'-'__extras'/_-false,
    id-atom-false,
    group_id-atom-true,
    key-atom-false,
    object_package_id-atom-false,
    package_id-atom-false,
    revision_id-atom-false,
    revision_timestamp-atom-false,
    state-atom-false,
    subject_package_id-atom-false,
    value-atom-false
  ]
).
legend(
  group,
  id,
  [
    abbreviation-atom-true,
    approval_status-atom-true,
    category-atom-true,
    'contact-email'-atom-true,
    'contact-name'-atom-true,
    'contact-phone'-atom-true,
    description-atom-true,
    extras-list(extra/_)-false,
    'foi-email'-atom-true,
    'foi-name'-atom-true,
    'foi-phone'-atom-true,
    'foi-web'-atom-true,
    groups-list('_group'/_)-true,
    id-atom-false,
    image_url-atom-true,
    is_organization-boolean-true,
    name-atom-true,
    packages-list(package/_)-false,
    tags-list(tag/_)-false,
    title-atom-true,
    type-atom-true,
    users-list(user/_)-false
  ]
).
legend(
  license,
  id,
  [
    domain_content-boolean-true,
    domain_data-boolean-true,
    domain_software-boolean-true,
    family-atom-true,
    id-atom-false,
    is_okd_compliant-boolean-true,
    is_generic-boolean-true,
    is_osi_compliant-boolean-true,
    maintainer-atom-true,
    status-atom-false,
    title-atom-false,
    url-atom-true
  ]
).
legend(
  organization,
  id,
  [
    approval_status-atom-false,
    created-atom-false,
    description-atom-true,
    display_name-atom-false,
    extras-list(extra/_)-true,
    groups-list(group/_)-true,
    id-atom-false,
    image_url-atom-true,
    is_organization-boolean-false,
    name-atom-false,
    num_followers-integer-true,
    package_count-integer-true,
    packages-list(package/_)-false,
    revision_id-atom-false,
    revision_timestamp-atom-false,
    state-atom-false,
    tags-list(tag/_)-false,
    title-atom-false,
    type-atom-false,
    users-list(user/_)-false
  ]
).
legend(
  package,
  id,
  [
    additional_resources-list(resource/_)-true,
    author-atom-true,
    author_email-atom-true,
    capacity-atom-true,
    'core-dataset'-boolean-true,
    'contact-email'-atom-true,
    'contact-name'-atom-true,
    'contact-phone'-atom-true,
    data_dict-skip-true,
    date_released-atom-true,
    date_update_future-atom-true,
    date_updated-atom-true,
    extras-list(extra/_)-false,
    'foi-email'-atom-true,
    'foi-name'-atom-true,
    'foi-phone'-atom-true,
    'foi-web'-atom-true,
    geographic_coverage-list(atom)-true,
    geographic_granularity-atom-true,
    'geographic_granularity-other'-atom-true,
    groups-list(atom)-false,
    id-atom-false,
    individual_resources-list(resource/_)-true,
    isopen-boolean-false,
    last_major_modification-atom-true,
    license_id-atom-true,
    license_title-atom-true,
    license_url-atom-true,
    maintainer-atom-true,
    maintainer_email-atom-true,
    mandate-atom-true,
    metadata_created-atom-false,
    metadata_modified-atom-false,
    name-atom-false,
    national_statistic-atom-true,
    notes-atom-true,
    num_resources-integer-false,
    num_tags-integer-false,
    organization-organization/_-true,
    owner_org-atom-true,
    %owner_org-iri(organization)-false,
    precision-atom-true,
    private-boolean-false,
    'publish-date'-atom-true,
    'publish-restricted'-boolean-true,
    published_via-atom-true,
    relationships_as_object-list(relationship/_)-false,
    relationships_as_subject-list(relationship/_)-false,
    'release-notes'-atom-true,
    resources-list(resource/_)-false,
    revision_id-atom-false,
    revision_timestamp-atom-false,
    state-atom-false,
    tags-list(tag/_)-false,
    taxonomy_url-atom-true,
    'temporal_coverage-from'-atom-true,
    'temporal_coverage-to'-atom-true,
    temporal_granularity-atom-true,
    'temporal_granularity-other'-atom-true,
    'theme-primary'-atom-true,
    'theme-secondary'-atom-true,
    timeseries_resources-list(resource/_)-true,
    title-atom-true,
    tracking_summary-tracking_summary/_-false,
    type-atom-false,
    unpublished-boolean-true,
    update_frequency-atom-true,
    'update_frequency-other'-atom-true,
    url-atom-true,
    version-atom-true
  ]
).
legend(
  reply,
  _,
  [
    error-error/_-true,
    help-atom-false,
    result-or([_/_,list(_/_),list(atom)])-true,
    success-boolean-false
  ]
).
legend(
  relationship,
  id,
  [
    '__extras'-'__extras'/_-false,
    comment-atom-true,
    extras-list(extra/_)-false,
    id-atom-false,
    type-atom-false
  ]
).
legend(
  resource,
  id,
  [
    'Author'-atom-false,
    cache_filepath-atom-true,
    cache_last_updated-atom-true,
    cache_url-atom-true,
    cache_url_updated-atom-true,
    ckan_recommended_wms_preview-boolean-true,
    content_length-atom-true,
    content_type-atom-true,
    created-atom-true,
    datastore_active-boolean-true,
    date-atom-true,
    description-atom-false,
    format-atom-false,
    'HGNC'-atom-false,
    hash-atom-false,
    'hub-id'-atom-true,
    id-atom-false,
    last_modified-atom-true,
    maintainer-atom-false,
    mimetype-atom-true,
    mimetype_inner-atom-true,
    name-atom-true,
    openness_score-integer-true,
    openness_score_failure_count-integer-true,
    openness_score_reason-atom-true,
    openspending_hint-atom-false,
    owner-atom-true,
    position-integer-false,
    'publish-date'-atom-true,
    resource_group_id-atom-false,
    resource_locator_function-atom-true,
    resource_locator_protocol-atom-true,
    resource_type-atom-true,
    revision_id-atom-false,
    revision_timestamp-atom-false,
    scraped-atom-true,
    scraper_source-atom-true,
    scraper_url-atom-true,
    size-integer-true,
    sparql_graph_name-atom-false,
    state-atom-false,
    tracking_summary-tracking_summary/_-false,
    url-atom-false,
    url_error-atom-false,
    verified-boolean-true,
    verified_date-atom-true,
    webstore_last_updated-atom-true,
    webstore_url-atom-true
  ]
).
legend(
  revision,
  id,
  [
    approved_timestamp-atom-true,
    author-atom-true,
    id-atom-false,
    message-atom-true,
    timestamp-atom-false
  ]
).
legend(
  tag,
  id,
  [
    display_name-atom-false,
    id-atom-false,
    name-atom-false,
    packages-list(package/_)-false,
    revision_timestamp-atom-false,
    state-atom-false,
    vocabulary_id-atom-true
  ]
).
legend(
  tracking_summary,
  _,
  [
    total-integer-false,
    recent-integer-false
  ]
).
legend(
  user,
  name,
  [
    about-atom-true,
    activity_streams_email_notifications-boolean-false,
    capacity-atom-false,
    created-atom-false,
    display_name-atom-false,
    id-atom-false,
    email_hash-atom-false,
    fullname-atom-true,
    name-atom-false,
    number_administered_packages-integer-false,
    number_of_administered_packages-integer-false,
    number_of_edits-integer-false,
    openid-atom-true,
    sysadmin-boolean-false
  ]
).



%! current_package_list_with_resources(
%!   +Options:list(nvpair),
%!   +Limit:positive_integer,
%!   +Offset:positive_integer,
%!   -Packages:list(atom)
%! ) is det.
% Return a list of the site's datasets (packages) and their resources.
%
% @arg Options
% @arg Limit If given, the list of datasets will be broken into
%      pages of at most `limit` datasets per page and only one page
%      will be returned at a time.
% @arg Offset If `limit` is given, the offset to start returning packages
%      from.
% @arg PackagesAndResources

current_package_list_with_resources(O1, Limit1, Offset1, PackagesAndResources):-
  select_option(paginated(true), O1, O2), !,
  default(Limit1, 10, Limit2),
  default(Offset1, 1, Offset2),
  paginated_current_package_list_with_resources(
    O2,
    Limit2,
    Offset2,
    PackagesAndResources
  ).
current_package_list_with_resources(O1, Limit, Offset, PackagesAndResources):-
  process_limit_offset(O1, Limit, Offset, P1),
  ckan(O1, current_package_list_with_resources, P1, PackagesAndResources).

paginated_current_package_list_with_resources(O1, Limit, Offset1, L3):-
  current_package_list_with_resources(O1, Limit, Offset1, L1), !,
  debug(ckan, 'Offset: ~d\n~w\n\n\n', [Offset1,L1]),
  Offset2 is Offset1 + Limit,
  paginated_current_package_list_with_resources(O1, Limit, Offset2, L2),
  append(L1, L2, L3).
paginated_current_package_list_with_resources(_, _, _, []).


%! format_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Formats:list(compound)
%! ) is det.
% Returns a list of resource formats whose names contain a string.
%
% @arg Options
% @arg Q The string to search for.
% @arg Limit The maximum number of resource formats to return
%      Default: 5.
% @arg Formats A list of format strings.

format_autocomplete(O1, Limit1, Q, Formats):-
  default(Limit1, 5, Limit2),
  ckan(O1, format_autocomplete, [limit=Limit2,q=Q], Formats).


%! group_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +GroupNames:list(atom),
%!   +Order:atom,
%!   -Groups:list(atom)
%! ) is det.
% Return a list of the names of the site's groups.
%
% @arg Options
% @arg AllFields Whether full group dictionaries should be returned
%      instead of just names.
%      Default: `false`.
% @arg Field Sorting of the search results based on this field.
%      The allowed fields are `name` and `packages`.
%      Default: `name`.
% @arg GroupNames A list of names of the groups to return, if given
%      only groups whose names are in this list will be returned.
%      Optional.
% @arg Order The sort-order used.
%      Default: `asc`
% @arg Groups A list of the atomic names of the site's groups.

group_list(O1, AllFields1, Field, GroupNames, Order, Groups):-
  default(AllFields1, false, AllFields2),
  json_boolean(AllFields2, AllFields3),
  process_field_order_sort(Field, Order, Sort),
  add_option([all_fields=AllFields3,sort=Sort], groups, GroupNames, P1),
  ckan(O1, group_list, P1, Groups).


%! group_list_authz(
%!   +Options:list(nvpair),
%!   +AmMember:boolean,
%!   +AvailableOnly:boolean,
%!   -Groups:list(compound)
%! ) is det.
% Return the list of groups that the user is authorized to edit.
%
% @arg Options
% @arg AmMember If `true` return only the groups the logged-in user
%      is a member of, otherwise return all groups that the user
%      is authorized to edit (for example, sysadmin users
%      are authorized to edit all groups) (optional, default: `false`).
% @arg AvailableOnly Remove the existing groups in the package
%      (optional, default: `false`).
% @arg Groups List of dictized groups that the user is authorized to edit.

group_list_authz(O1, AmMember1, AvailableOnly1, Groups):-
  default(AvailableOnly1, false, AvailableOnly2),
  json_boolean(AvailableOnly2, AvailableOnly3),
  default(AmMember1, false, AmMember2),
  json_boolean(AmMember2, AmMember3),
  ckan(
    O1,
    group_list_authz,
    [am_member=AmMember3,available_only=AvailableOnly3],
    Groups
  ).


%! group_package_show(
%!   +Options:list(nvpair)
%!   +IdOrName:atom
%!   +Limit:integer
%!   -Packages:list(compound)
%! ) is det.
% Returns the datasets (packages) of a group.
%
% @arg Options
% @arg IdOrName The id or name of the group.
% @arg Limit The maximum number of datasets to return (optional).
% @arg Packages A list of packages.

group_package_show(O1, IdOrName, Limit, Packages):-
  add_option([id=IdOrName], limit, Limit, P1),
  ckan(O1, group_package_show, P1, Packages).


%! group_revision_list(
%!   +Options:list(nvpair),
%!   +NameOrId:atom,
%!   -Revisions:list(compound)
%! ) is det.
% Return a group's revisions.
%
% @arg Options
% @arg NameOrId The name or id of the group.
% @arg Revisions List of dictionaries.

group_revision_list(O1, NameOrId, Revisions):-
  ckan(O1, group_revision_list, [id=NameOrId], Revisions).


%! group_show(+Options:list(nvpair), +IdOrName:atom, -Group:compound) is det.
% Returns the details of a group.
%
% @arg Options
% @arg IdOrName The id or name of the group.
% @arg Group A group.

group_show(O1, Id, Group):-
  ckan(O1, group_show, [id=Id], Group).


%! license_list(+Options:list(nvpair), -Licenses:list(compound)) is det.
% Return the list of licenses available for datasets on the site.
%
% @arg Options
% @arg Licenses List of dictionaries.

license_list(O1, Licenses):-
  (
    option(deprecated(true), O1)
  ->
    FunctionName = licence_list
  ;
    FunctionName = license_list
  ),

  ckan(O1, FunctionName, [], Licenses).


%! member_list(
%!   +Options:list(nvpair),
%!   +Capacity:atom,
%!   +IdOrName:atom,
%!   +ObjectType:atom,
%!   -Triples:list(triple(atom,atom,atom))
%! ) is det.
% Return the members of a group.
%
% The user must have permission to ‘get’ the group.
%
% @arg Capacity Restrict the members returned to those with a given capacity,
%      e.g. `member`, `editor`, `admin`, `public`, `private`
%      (optional, default: `None`)
% @arg IdOrName The id or name of the group.
% @arg ObjectType Restrict the members returned to those of a given type,
%      e.g. `user` or `package` (optional, default: `None`).
% @arg Triples A list of <id,type,capacity>-triples.
%
% @throw ckan.logic.NotFound If the group does not exist.

member_list(O1, Capacity, IdOrName, ObjectType, Triples):-
  P1 = [id=IdOrName],
  add_option(P1, capacity, Capacity, P2),
  add_option(P2, object_type, ObjectType, P3),
  ckan(O1, member_list, P3, Triples).


%! organization_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +Order:atom,
%!   +OrganizationsFilter:list(atom),
%!   -Organizations:list(atom)
%! ) is det.
% Return a list of the names of the site’s organizations.
%
% @arg AllFields Return full group dictionaries instead of just names
%      (optional, default: `false`).
% @arg Field Sorting of the search results based on this field.
%      The allowed fields are `name` and `packages`.
%      Default: `name`.
% @arg Order The sort-order used.
%      Default: `asc`
% @arg OrganizationsFilter A list of names of the groups to return,
%      if given only groups whose names are in this list
%      will be returned (optional).
% @arg Organizations A list of organization names.

organization_list(
  O1,
  AllFields1,
  Field,
  Order,
  OrganizationsFilter,
  Organizations
):-
  default(AllFields1, false, AllFields2),
  json_boolean(AllFields2, AllFields3),
  process_field_order_sort(Field, Order, Sort),
  add_option(
    [all_fields=AllFields3,sort=Sort],
    organizations,
    OrganizationsFilter,
    P1
  ),
  ckan(O1, organization_list, P1, Organizations).


%! organization_list_for_user(
%!   +Options:list(nvpair),
%!   +Permission:atom,
%!   -Organizations:list(compound)
%! ) is det.
% Return the list of organizations that the user is a member of.
%
% @arg Options
% @arg Permission The permission the user has against
%      the returned organizations (optional, default: `edit_group`).
% @arg Organizations List of dictized organizations
%      that the user is authorized to edit.

organization_list_for_user(O1, Permission1, Organizations):-
  default(Permission1, edit_group, Permission2),
  ckan(
    O1,
    organization_list_for_user,
    [permission=Permission2],
    Organizations
  ).


%! organization_show(
%!   +Options:list(nvpair),
%!   +IdOrName:atom,
%!   -Organization:compound
%! ) is det.
% Returns the details of an organization.
%
% @arg Options
% @arg IdOrName The id or name of the organization.
% @arg Organization An organization.

organization_show(O1, IdOrName, Organization):-
  ckan(O1, organization_show, [id=IdOrName], Organization).


%! package_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Packages:list(compound)
%! ) is det.
% Returns a list of datasets (packages) that match a string.
%
% Datasets with names or titles that contain the query string
%  will be returned.
%
% @arg Options
% @arg Limit The maximum number of resource formats to return
%      Default: 10.
% @arg Q The string to search for.
% @arg Package A list of packages.

package_autocomplete(O1, Limit1, Q, Packages):-
  default(Limit1, 10, Limit2),
  ckan(O1, package_autocomplete, [limit=Limit2,q=Q], Packages).


%! package_list(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Offset:integer,
%!   -Packages:list(atom)
%! ) is det.
% Return a list of the names of the site's datasets (packages).
%
% @arg Options
% @arg Limit If given, the list of datasets will be broken into pages
%      of at most `Limit` datasets per page and only one page
%      will be returned at a time (optional).
% @arg Offset If `limit` is given, the offset to start returning packages
%      from.
% @arg Packages A list of atomic package/dataset names.
%      The list is sorted most-recently-modified first.

package_list(O1, Limit, Offset, Packages):-
  process_limit_offset(O1, Limit, Offset, P2),
  ckan(O1, package_list, P2, Packages).


%! package_relationships_list(
%!   +Options:list(nvpair),
%!   +Id:atom,
%!   +Id2:atom,
%!   +Rel:atom,
%!   -Relationships:list(compound)
%! ) is det.
% Returns a dataset (package)'s relationships.
%
% @arg Options
% @arg Id The id or name of the first package.
% @arg Id2 The id or name of the second package.
% @arg Rel Relationship as string,
%      see package_relationship_create/4 (optional).
%      [Is this a filter by relation type?]
% @arg Relationships A list of relationships.
%
% @see package_relationship_create/4 for the relationship types.

package_relationships_list(O1, Id, Id2, Rel, Relationships):-
  add_option([id=Id,id2=Id2], rel, Rel, P1),
  ckan(O1, package_relationships_list, P1, Relationships).


%! package_revision_list(
%!   +Options:list(nvpair),
%!   +Package:atom,
%!   -Revisions:list(compound)
%! ) is det.
% Return a dataset (package)'s revisions as a list of dictionaries.
%
% @arg Options
% @arg Package The id or name of the dataset.
% @arg Revisions A list of revision terms.

package_revision_list(O1, Package, Revisions):-
  ckan(O1, package_revision_list, [id(Package)], Revisions).


%! package_show(
%!   +Options:list(nvpair),
%!   +IdOrName:atom,
%!   -Package:compound
%! ) is det.

package_show(O1, IdOrName, Package):-
  ckan(O1, package_show, [id(IdOrName)], Package).


%! related_list(
%!   +Options:list(nvpair),
%!   +Dataset:compound,
%!   +Featured:boolean,
%!   +IdOrName:atom,
%!   +Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc]),
%!   +TypeFilter:atom,
%!   -Related:list(compound)
%! ) is det.
% Return a dataset's related items.
%
% Either the `IdOrName` or the `Dataset` parameter must be instantiated.
%
% @arg Options
% @arg Dataset Dataset dictionary of the dataset (optional).
% @arg Featured Whether or not to restrict the results
%      to only featured related items (optional, default: `false`)
% @arg IdOrName Id or name of the dataset (optional).
% @arg TypeFilter The type of related item to show
%      (optional, default: None, showing all items).
% @arg Sort The order to sort the related items in.
%      Possible values are `view_count_asc`, `view_count_desc`,
%      `created_asc` or `created_desc` (optional).
% @arg Related A list of dictionaries

related_list(O1, Dataset, Featured1, IdOrName, TypeFilter, Sort, Related):-
  default(Featured1, false, Featured2),
  json_boolean(Featured2, Featured3),

  % Either `dataset` or `id` must be instantiated.
  \+ maplist(nonvar, [Dataset,IdOrName]),
  \+ maplist(var, [Dataset,IdOrName]),

  add_option([featured=Featured3], dataset, Dataset, P2),
  add_option(P2, id, IdOrName, P3),

  % Optional `type_filter`
  add_option(P3, type_filter, TypeFilter, P4),

  % Optional `sort`
  add_option(P4, sort, Sort, P5),

  ckan(O1, related_list, P5, Related).


%! related_show(+Options:list(nvpair), +Id:atom, -Out:compound) is det.
% Return a single related item.
%
% @arg Options
% @arg Id the id of the related item to show
% @arg Out

related_show(O1, Id, Out):-
  ckan(O1, related_show, [id(Id)], Out).


%! resource_show(+Options:list(nvpair), +Id:atom, -Resource:compound) is det.
% Returns the metadata of a resource.
%
% @arg Options
% @arg Id The id of the resource.
% @arg Resource A resource.

resource_show(O1, Id, Resource):-
  ckan(O1, resource_show, [id=Id], Resource).


%! resource_status_show(
%!   +Options:list(nvpair),
%!   +Id:atom,
%!   -Statuses:list(list)
%! ) is det.
% Returns the statuses of a resource's tasks.
%
% @arg Options
% @arg Id The id of the resource.
% @arg Statuses A list of
%      =|<status,date_done,traceback,task_status>|=-dictionaries.

resource_status_show(O1, Id, Statuses):-
  ckan(O1, resource_status_show, [id=Id], Statuses).


%! revision_list(+Options:list(nvpair), -Revisions:list(atom)) is det.
% Return a list of the IDs of the site’s revisions.
%
% @arg Options
% @arg Revisions A list of IDs of the site's revisions.

revision_list(O1, Revisions):-
  ckan(O1, revision_list, [], Revisions).


%! revision_show(+Options:list(nvpair), +Id:atom, -Revision:compound) is det.
% Returns the details of a revision.
%
% @arg Options
% @arg Id The id of the revision.
% @arg Revision A revision.

revision_show(O1, Id, Revision):-
  ckan(O1, revision_show, [id=Id], Revision).


%! site_read(+Options:list(nvpair)) is semidet.
% Suceeds if the CKAN site is readable?

site_read(O1):-
  ckan(O1, site_read, [], true).


%! tag_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Query:atom,
%!   +VocabularyId:atom,
%!   -Tags:or([list(atom),list(compound)])
%! ) is det.
% Returns a list of the site's tags.
%
% By default only free tags (tags that don't belong to a vocabulary)
%  are returned.
% If the `VocabularyId` argument is given then only tags belonging to
%  that vocabulary will be returned.
%
% @arg Options
% @arg AllFields Return full tag dictionaries instead of just names
%      (optional, default: `false`).
% @arg Query A tag name query to search for, if given only tags whose
%      names contain this string will be returned.
% @arg VocabularyId The id or name of a vocabulary, if give only tags
%      that belong to this vocabulary will be returned.
% @arg Tags A list of tags.

tag_list(O1, AllFields1, Query, VocabularyId, Tags):-
  default(AllFields1, false, AllFields2),
  json_boolean(AllFields2, AllFields3),
  add_option([all_fields=AllFields3], query, Query, P1),
  add_option(P1, vocabulary_id, VocabularyId, P2),
  ckan(O1, tag_list, P2, Tags).


%! tag_show(+Options:list(nvpair), +IdOrName:atom, -Tag:compound) is det.
% Returns the details of a tag and all its datasets.
%
% @arg Options
% @arg IdOrName The name or id of the tag.
% @arg Tag The details of the tag, including a list of
%      all the tag's datasets and their details

tag_show(O1, IdOrName, Tag):-
  ckan(O1, tag_show, [id=IdOrName], Tag).


%! user_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Users:list(compound)
%! ) is det.
% Returns a list of user names that contain a string.
%
% @arg Options
% @arg Q The string to search for.
% @arg Limit The maximum number of user names to return.
%      Default: 20.
% @arg Users A list of user dictionaries each with keys
%      `name`, `fullname`, and `id`.

user_autocomplete(O1, Limit1, Q, Users):-
  default(Limit1, 20, Limit2),
  ckan(O1, user_autocomplete, [limit=Limit2,q=Q], Users).


%! user_list(
%!   +Options:list(nvpair),
%!   +OrderBy:atom,
%!   +Q:atom,
%!   -Users:list(compound)
%! ) is det.
% Returns a list of the site's user accounts.
%
% @arg Options
% @arg OrderBy Which field to sort the list by.
%      Default: `name`.
% @arg Q Restrict the users returned to those whose names contain
%      a[=this?] string (optional).
% @arg Users A list of users.

user_list(O1, OrderBy1, Q, Users):-
  default(OrderBy1, name, OrderBy2),
  add_option([order_by=OrderBy2], q, Q, P1),
  ckan(O1, user_list, P1, Users).

%! user_show(
%!   +Options:list(nvpair),
%!   +IdOrName:atom,
%!   +UserObject:compound,
%!   -User:compound
%! ) is det.
% Returns a user account.
%
% Either `IdOrName` or `UserObject` must be instantiated.
%
% @arg Options
% @arg IdOrName The id or name of the user (optional).
% @arg UserObject The user dictionary of the user (optional).
% @arg User A user.

user_show(O1, IdOrName, UserObject, User):-
  % Either `IdOrName` or `UserObject` must be instantiated.
  \+ maplist(nonvar, [IdOrName,UserObject]),
  \+ maplist(var, [IdOrName,UserObject]),

  add_option([], id, IdOrName, P1),
  add_option(P1, user_obj, UserObject, P2),

  ckan(O1, user_show, P2, User).


% HELPER PREDICATES %

%! ckan(
%!   +Options:list(nvpair),
%!   +Action:atom,
%!   +Parameters:list(nvpair),
%!   -JSON:compound
%! ) is det.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     An atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default: uninstantiated, using the server-side default.
%   * =|authority(+Authority:atom)|=
%     Default: =|datahub.io|=
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.
%
% @arg Options A list of name-value pairs.
% @arg Action The atomic name of a CKAN action.
% @arg Parameters A list of name-value pairs.
% @arg JSON A JSON-term.

ckan(O1, Action, Parameters, Result):-
  % URL
  option(scheme(Scheme), O1),
  option(authority(Authority), O1),
  option(api_version(Version), O1, 3),
  uri_path([api,Version,action,Action], Path),
  uri_components(URL, uri_components(Scheme, Authority, Path, _, _)),

  % API key
  (
    option(api_key(Key), O1)
  ->
    HTTP_O1 = [request_header('Authorization'=Key)]
  ;
    HTTP_O1 = []
  ),

  JSON_In = json(Parameters),
  merge_options(
    [
      method(post),
      post(json(JSON_In)),
      request_header('Content-Type'='application/json'),
      status_code(Status)
    ],
    HTTP_O1,
    HTTP_O2
  ),
  option(output(Format), O1, rdf),
  setup_call_cleanup(
    http_open(URL, Out, HTTP_O2),
    process_http(Status, Out, Format, Result),
    close(Out)
  ).

process_http(200, Out, Format, Return):- !,
  json_read(Out, json(Reply)),
  memberchk(help=Help, Reply),
  (
    memberchk(error=Error, Reply)
  ->
    memberchk('__type'=Type, Error),
    memberchk(message=Message, Error),
    throw(error(Type, context(Help, Message)))
  ;
    memberchk(result=Result, Reply),

    % To: Prolog
    json_to_prolog(ckan, Result, Return),

    % To: RDF
    (Format == rdf -> json_to_rdf(ckan, ckan, Result, _)),

    debug(ckan, 'Successful reply:\n~w', [Help])
  ).
process_http(Status, _, _, _):-
  debug(ckan, 'HTTP status code ~w', [Status]).


%! process_field_order_sort(
%!   +Field:oneof([name,packages]),
%!   +Order:atom,
%!   -Sort:atom
%! ) is det.

process_field_order_sort(Field1, Order1, Sort):-
  default(Field1, name, Field2),
  default(Order1, asc, Order2),
  atomic_list_concat([Field2,Order2], ' ', Sort).

%! process_limit_offset(
%!   +Options:list(nvpair),
%!   ?Limit:integer,
%!   ?Offset:integer,
%!   -Parameters:list(nvpair)
%! ) is det.
% The `offset` option is meaningless if there is no `limit` option.

process_limit_offset(_, Limit, Offset, []):-
  nonvar(Offset),
  var(Limit), !.
process_limit_offset(O1, Limit, Offset, P2):-
  % Parameter `limit`.
  add_option([], limit, Limit, P1),

  % Parameter `offset`.
  (
    option(deprecated(true), O1, false)
  ->
    ParameterName = page
  ;
    ParameterName = offset
  ),
  add_option(P1, ParameterName, Offset, P2).

