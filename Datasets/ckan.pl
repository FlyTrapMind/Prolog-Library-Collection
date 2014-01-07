:- module(
  ckan,
  [
    current_package_list_with_resources/4, % +Options:list(nvpair)
                                           % +Limit:integer
                                           % +Offset:integer
                                           % -Resources:list
    group_list/6, % +Options:list(nvpair),
                  % +AllFields:boolean,
                  % +Field:oneof([name,packages]),
                  % +Groups:list(atom),
                  % +Order:atom,
                  % -Groups:list(atom)
    member_list/5, % +Options:list(nvpair),
                   % +Capacity:atom,
                   % +IdOrName:atom,
                   % +ObjectType:atom,
                   % -Triples:list(triple(atom,atom,atom))
    package_list/4, % +Options:list(nvpair)
                    % +Limit:integer
                    % +Offset:integer
                    % -Packages:list(atom)
    package_revision_list/3, % +Options:list(nvpair)
                             % +Package:atom
                             % -Revisions:list(compound)
    related_list/7, % +Options:list(nvpair),
                    % ?Dataset:compound,
                    % ?Featured:boolean,
                    % ?IdOrName:atom,
                    % ?Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc]),
                    % ?TypeFilter:atom,
                    % -Related:list(compound)
    related_show/3, % +Options:list(nvpair)
                    % +Id:atom
                    % -Out:compound
    revision_list/2, % +Options:list(nvpair)
                     % -Revisions:list(atom)
    site_read/1 % +Options:list(nvpair)
  ]
).

/** <module> CKAN

Querying the CKAN API.

@author Wouter Beek
@version 2013/11-2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json_convert)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(server(api_keys)).

:- json_object revision(
  approved_timestamp, % atom or null
  author, % atom or null
  id:atom,
  message, % atom or null
  timestamp:atom
).



%! current_package_list_with_resources(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Offset:integer,
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

current_package_list_with_resources(O1, Limit, Offset, PackagesAndResources):-
  process_limit_offset(Limit, Offset, P1),
  ckan(O1, current_package_list_with_resources, P1, PackagesAndResources).


%! group_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +Groups:list(atom),
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
% @arg Groups A list of names of the groups to return, if given
%      only groups whose names are in this list will be returned.
%      Optional.
% @arg Order The sort-order used.
%      Default: `asc`
% @arg Groups A list of the atomic names of the site's groups.

group_list(O1, AllFields1, Field1, Groups, Order1, Groups):-
  default(AllFields1, false, AllFields2),
  default(Field1, name, Field2),
  default(Order1, asc, Order2),
  atomic_list_concat([Field2,Order2], ' ', Sort),
  add_option([all_fields=AllFields2,sort=Sort], groups, Groups, P1),
  ckan(O1, group_list, P1, JSON),
  JSON = json(Groups).


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
  process_limit_offset(Limit, Offset, P2),
  ckan(O1, package_list, P2, Packages).


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


%! related_list(
%!   +Options:list(nvpair),
%!   ?Dataset:compound,
%!   ?Featured:boolean,
%!   ?IdOrName:atom,
%!   ?Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc]),
%!   ?TypeFilter:atom,
%!   -Related:list(compound)
%! ) is det.
% Return a dataset's related items.
%
% Either the `IdOrName` or the `Dataset` parameter must be given.
%
% @arg Options
% @arg Dataset Dataset dictionary of the dataset (optional).
% @arg IdOrName Id or name of the dataset (optional).
% @arg TypeFilter The type of related item to show
%      (optional, default: None, showing all items).
% @arg Sort The order to sort the related items in.
%      Possible values are `view_count_asc`, `view_count_desc`,
%      `created_asc` or `created_desc` (optional).
% @arg Featured Whether or not to restrict the results
%      to only featured related items (optional, default: `false`)
% @arg Related A list of dictionaries

related_list(O1, Dataset, IdOrName, TypeFilter, Sort, Featured1, Related):-
  default(Featured1, false, Featured2),

  % Either `dataset` or `id`.
  \+ maplist(nonvar, [Dataset,IdOrName]),
  \+ maplist(var, [Dataset,IdOrName]),
  P1 = [featured=Featured2],
  add_option(P1, dataset, Dataset, P2),
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


%! revision_list(+Options:list(nvpair), -Revisions:list(atom)) is det.
% Return a list of the IDs of the site’s revisions.
%
% @arg Options
% @arg Revisions A list of IDs of the site's revisions.

revision_list(O1, Revisions):-
  ckan(O1, revision_list, [], Revisions).


%! site_read(+Options:list(nvpair)) is semidet.
% Suceeds if the CKAN site is readable?

site_read(O1):-
  ckan(O1, site_read, [], JSON),
  JSON == @(true).



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
% The following actions are supported:
%   * =dashboard_activity_list=
%     Returns a list of activities from the user dashboard (API key required).
%   * =package_create=
%     Create a new dataset (API key required).
%   * =package_list=
%     Returns a list of the CKAN datasets.
%   * =package_search=
%     Search for CKAN datasets by name.
%     Requires the search option `q` to be set to the search term.
%     Optionally set the maximum number of returned search requests
%     with search option `rows` (with a positive integer value).
%   * =site_read=
%     Whether the CKAN site gives the user read access?
%
% @arg Options A list of name-value pairs.
% @arg Action The atomic name of a CKAN action.
% @arg Parameters A list of name-value pairs.
% @arg JSON A JSON-term.
%
% @see http://docs.ckan.org/en/latest/api.html

ckan(O1, Action, Parameters, Result):-
  % URL
  option(scheme(Scheme), O1, http),
  option(authority(Authority), O1, 'datahub.io'),
  option(api_version(API_Version), O1, _VAR),
  uri_path([api,API_Version,action,Action], Path),
  uri_components(URL, uri_components(Scheme, Authority, Path, _, _)),

  % API key
  current_api_key('datahub.io', ckan, Default_API_Key),
  option(api_key(API_Key), O1, Default_API_Key),

  % HTTP POST
  HTTP_Options = [
    request_header('Authorization'=API_Key),
    request_header('Content-Type'='application/json')
  ],
  JSON_In = json(Parameters),
  http_post(URL, json(JSON_In), json(JSON_Out), HTTP_Options),

  % Process result.
  memberchk(success=Success, JSON_Out),
  (
    Success == @(true)
  ->
    memberchk(result=Result, JSON_Out)
  ;
    Success == @(false)
  ->
    ckan_error(JSON_Out)
  ).


ckan_error(JSON):-
  memberchk(help=Help, JSON),
  memberchk(error=Error, JSON),
  memberchk(message=Message, Error),
  memberchk('__type'=Type, Error),
  throw(error(Type, context(Help, Message))).


%! filter_offset(
%!   ?Limit:integer,
%!   ?Offset:integer,
%!   -Parameters:list(nvpair)
%! ) is det.
% The `offset` option is meaningless if there is no `limit` option.

process_limit_offset(Limit, Offset, []):-
  nonvar(Offset),
  var(Limit), !.
process_limit_offset(Limit, Offset, P2):-
  add_option([], limit, Limit, P1),
  add_option(P1, offset, Offset, P2).

