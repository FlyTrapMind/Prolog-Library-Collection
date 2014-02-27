:- module(
  ckan,
  [
    ckan/4 % +Options:list(nvpair)
           % +Action:atom
           % +Parameters:list(nvpair)
           % -Result:compound
  ]
).

/** <module> CKAN

@author Wouter Beek
@see http://docs.ckan.org/en/latest/api.html
@tbd The CKAN API uses `True` and `False` for boolean values.
@tbd The JSON `null` value is not replaced with a given default value.
@tbd Email addresses cannot all be parsed.
@tbd URLs cannot all be parsed.
@version 2013/11-2014/01
*/

:- use_module(generics(uri_ext)).
:- use_module(http(http)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(rdf_conv(json_to_rdf)).
:- use_module(standards(json_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ckan, 'http://www.wouterbeek.com/ckan#').

:- meta_predicate(ckan_http(+,+,+,1)).

% `Predicate:atom-Type:atom-Optional:boolean`
legend(
  '__extras',
  _,
  [
    group_id-string-false,
    object_package_id-string-false,
    package_id-string-false,
    revision_id-string-false,
    revision_timestamp-dateTime-false,
    subject_package_id-string-false
  ]
).
legend(
  '_group',
  _,
  [
    capacity-string-false,
    name-string-false
  ]
).
legend(
  error,
  _,
  [
    '__type'-string-false,
    message-string-false
  ]
).
legend(
  extra,
  _,
  [
    '__extras'-'__extras'/_-false,
    id-string-false,
    group_id-string-true,
    key-string-false,
    object_package_id-string-false,
    package_id-string-false,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    state-string-false,
    subject_package_id-string-false,
    value-string-true
  ]
).
% `name` does not seem to be a very reliable unique identifier,
% but `data.gov.uk` contains the following:
% =|GROUP.groups=[json([capacity= (public),name='national-health-service'])]|=
% i.e., without `id`.
legend(
  group,
  name,
  [
    abbreviation-string-true,
    approval_status-string-false,
    capacity-string-false,
    category-string-true,
    'contact-email'-string-true, % Should be `email`.
    'contact-name'-string-true,
    'contact-phone'-string-true,
    created-dateTime-false,
    description-string-true,
    display_name-string-false,
    extras-list(extra/_)-true,
    'foi-email'-string-true, % Should be `email`.
    'foi-name'-string-true,
    'foi-phone'-string-true,
    'foi-web'-string-true,
    groups-list(group/_)-true,
    id-string-false,
    image_display_url-string-true, % Should be `url`.
    image_url-string-true, % Should be `url`.
    is_organization-boolean-false,
    name-string-false,
    num_followers-integer-true,
    package_count-integer-true,
    packages-list(package/_)-false,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    state-string-false,
    tags-list(tag/_)-false,
    title-string-false,
    type-string-false,
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
    family-string-true,
    id-string-false,
    is_okd_compliant-boolean-true,
    is_generic-boolean-true,
    is_osi_compliant-boolean-true,
    maintainer-string-true,
    status-string-false,
    title-string-false,
    url-string-true  % Should be `url`.
  ]
).
legend(
  package,
  id,
  [
    additional_resources-list(resource/_)-true,
    author-string-true,
    author_email-string-true, % Should be `email`.
    capacity-string-true,
    'core-dataset'-boolean-true,
    'contact-email'-string-true, % Should be `email`.
    'contact-name'-string-true,
    'contact-phone'-string-true,
    creator_user_id-string-true,
    data_dict-skip-true,
    date_released-string-true,
    date_update_future-string-true,
    date_updated-string-true,
    extras-list(extra/_)-false,
    'foi-email'-string-true, % Should be `email`.
    'foi-name'-string-true,
    'foi-phone'-string-true,
    'foi-web'-string-true,
    geographic_coverage-list(string)-true,
    geographic_granularity-string-true,
    'geographic_granularity-other'-string-true,
    groups-list(group/_)-false,
    id-string-false,
    individual_resources-list(resource/_)-true,
    isopen-boolean-false,
    last_major_modification-string-true,
    license_id-license/_-true,
    license_title-string-true,
    license_url-string-true,
    maintainer-string-true,
    maintainer_email-string-true, % Should be `email`.
    mandate-string-true,
    metadata_created-string-false,
    metadata_modified-string-false,
    name-string-false,
    national_statistic-string-true,
    notes-string-true,
    num_resources-integer-false,
    num_tags-integer-false,
    organization-group/_-true,
    owner_org-group/_-true,
    precision-string-true,
    private-boolean-false,
    'publish-date'-string-true,
    'publish-restricted'-boolean-true,
    published_via-string-true,
    relationships_as_object-list(relationship/_)-false,
    relationships_as_subject-list(relationship/_)-false,
    'release-notes'-string-true,
    resources-list(resource/_)-false,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    state-string-false,
    tags-list(tag/_)-false,
    taxonomy_url-string-true,
    'temporal_coverage-from'-string-true,
    'temporal_coverage-to'-string-true,
    temporal_granularity-string-true,
    'temporal_granularity-other'-string-true,
    'theme-primary'-string-true,
    'theme-secondary'-string-true,
    timeseries_resources-list(resource/_)-true,
    title-string-true,
    tracking_summary-tracking_summary/_-false,
    type-string-false,
    unpublished-boolean-true,
    update_frequency-string-true,
    'update_frequency-other'-string-true,
    url-string-true, % Should be `url`.
    version-string-true
  ]
).
legend(
  reply,
  _,
  [
    error-error/_-true,
    help-string-false,
    result-or([_/_,list(_/_),list(string)])-true,
    success-boolean-false
  ]
).
legend(
  relationship,
  id,
  [
    '__extras'-'__extras'/_-false,
    comment-string-true,
    extras-list(extra/_)-false,
    id-string-false,
    object_package_id-string-false,
    revision_id-string-false,
    revision_timestamp-dateTime-false,
    state-string-false,
    subject_package_id-string-false,
    type-string-false
  ]
).
legend(
  resource,
  id,
  [
    'API'-string-false,
    'Author'-string-false,
    beach_type-string-true,
    cache_filepath-string-true,
    cache_last_updated-string-true,
    cache_url-string-true,
    cache_url_updated-string-true,
    ckan_recommended_wms_preview-boolean-true,
    'ClementLevallois'-string-false,
    'Contact'-string-false,
    content_length-string-true,
    content_type-string-true,
    created-dateTime-true,
    datastore_active-boolean-true,
    date-string-true,
    description-string-true,
    format-string-true,
    'GranularidadeEspacial'-string-false,
    hash-string-true,
    'HGNC'-string-false,
    'HotCasino'-string-false,
    'http://xmlns.com/foaf/0.1/primaryTopic'-string-false,
    'hub-id'-string-true,
    id-string-false,
    'JumpStartGeorgia'-string-false,
    lang-string-false,
    last_modified-dateTime-true,
    license-string-false, % Does this ever occur?
    'links:dbpedia'-integer-false,
    'links:dnb-gemeinsame-normdatei'-string-false,
    'links:lemon'-integer-false,
    'links:lexinfo'-integer-false,
    maintainer-string-false,
    mimetype-string-true,
    'mimetype-inner'-string-false,
    mimetype_inner-string-true,
    name-string-true,
    namespace-string-false,
    notes-string-true,
    opendatavenezia-string-true,
    openness_score-integer-true,
    openness_score_failure_count-integer-true,
    openness_score_reason-string-true,
    openspending_hint-string-true,
    owner-string-true,
    owner_org-group/_-false,
    'Periodicidade'-string-false,
    position-integer-false,
    'publish-date'-string-true,
    'record-count'-string-false,
    records-integer-false,
    resource_group_id-string-false,
    resource_locator_function-string-true,
    resource_locator_protocol-string-true,
    resource_type-string-true,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    scraped-string-true,
    scraper_source-string-true,
    scraper_url-string-true,
    sender-string-false,
    size-string-true,
    sparql_graph_name-string-false,
    'Sparqlendpoint'-string-true,
    'SpirosAlexiou'-string-false,
    state-string-false,
    'State'-string-false,
    'SvenVlaeminck'-string-false,
    tracking_summary-tracking_summary/_-false,
    translator-string-false,
    triples-integer-false,
    type-string-false,
    url-string-false, % Should be `url`.
    url_error-string-false,
    verified-boolean-true,
    verified_date-string-true,
    webstore_last_updated-string-true,
    webstore_url-string-true,
    wms_base_urls-string-true
  ]
).
legend(
  revision,
  id,
  [
    approved_timestamp-string-true,
    author-string-true,
    groups-list(group/_)-false,
    id-string-false,
    message-string-true,
    packages-list(package/_)-false,
    timestamp-string-false
  ]
).
legend(
  tag,
  id,
  [
    display_name-string-false,
    id-string-false,
    name-string-false,
    packages-list(package/_)-false,
    revision_timestamp-dateTime-false,
    state-string-false,
    vocabulary_id-string-true
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
  id,
  [
    about-string-true,
    activity_streams_email_notifications-boolean-false,
    capacity-string-false,
    created-dateTime-false,
    display_name-string-false,
    id-string-false,
    email_hash-string-false,
    fullname-string-true,
    name-string-false,
    number_administered_packages-integer-false,
    number_of_administered_packages-integer-false,
    number_of_edits-integer-false,
    openid-string-true,
    sysadmin-boolean-false
  ]
).



%! ckan(
%!   +Options:list(nvpair),
%!   +Action:atom,
%!   +Parameters:list(nvpair),
%!   -Result:compound
%! ) is det.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     An atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default: uninstantiated, using the server-side default.
%   * =|authority(+Authority:atom)|=
%     REQUIRED.
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.
%
% @arg Options A list of name-value pairs.
% @arg Action The atomic name of a CKAN action.
% @arg Parameters A list of name-value pairs.
% @arg Result A Prolog compound term.

ckan(O1, Action, Parameters, rdf):-
  option(graph(Graph), O1), !,
  ckan_http(O1, Action, Parameters, ckan_rdf(Graph)).
ckan(O1, Action, Parameters, Result):-
  ckan_http(O1, Action, Parameters, ckan_pl(Result)).

ckan_http(O1, Action, Parameters, Goal):-
  % URL
  option(scheme(Scheme), O1),
  option(authority(Authority), O1),
  option(api_version(Version), O1, _VAR),
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
      never_give_up(true),
      post(json(JSON_In)),
      request_header('Accept'='application/json')
    ],
    HTTP_O1,
    HTTP_O2
  ),
  http_goal(URL, HTTP_O2, Goal).

% To: Prolog
ckan_pl(Return, Stream):-
  ckan_stream_to_result(Stream, Result),
  json_to_prolog(ckan, Result, Return).

% To: RDF
ckan_rdf(Graph, Stream):-
  ckan_stream_to_result(Stream, Result),
  json_to_rdf(Graph, ckan, ckan, Result, _).

ckan_stream_to_result(Stream, Result):-
  json_read(Stream, json(Reply)),
  memberchk(help=Help, Reply),
  (
    memberchk(error=Error, Reply)
  ->
    memberchk('__type'=Type, Error),
    memberchk(message=Message, Error),
    throw(error(Type, context(Help, Message)))
  ;
    memberchk(result=Result, Reply)
  ).

