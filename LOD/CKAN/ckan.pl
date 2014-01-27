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
    group_id-atom-false,
    object_package_id-atom-false,
    package_id-atom-false,
    revision_id-atom-false,
    revision_timestamp-dateTime-false,
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
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    state-atom-false,
    subject_package_id-atom-false,
    value-atom-true
  ]
).
legend(
  group,
  id,
  [
    abbreviation-atom-true,
    approval_status-atom-false,
    capacity-atom-false,
    category-atom-true,
    'contact-email'-atom-true, % Should be `email`.
    'contact-name'-atom-true,
    'contact-phone'-atom-true,
    created-dateTime-false,
    description-atom-true,
    display_name-atom-false,
    extras-list(extra/_)-true,
    'foi-email'-atom-true, % Should be `email`.
    'foi-name'-atom-true,
    'foi-phone'-atom-true,
    'foi-web'-atom-true,
    groups-list(group/_)-true,
    id-atom-false,
    image_url-atom-true, % Should be `url`.
    is_organization-boolean-false,
    name-atom-false,
    num_followers-integer-true,
    package_count-integer-true,
    packages-list(package/_)-false,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    state-atom-false,
    tags-list(tag/_)-false,
    title-atom-false,
    type-atom-false,
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
    url-atom-true  % Should be `url`.
  ]
).
legend(
  package,
  id,
  [
    additional_resources-list(resource/_)-true,
    author-atom-true,
    author_email-atom-true, % Should be `email`.
    capacity-atom-true,
    'core-dataset'-boolean-true,
    'contact-email'-atom-true, % Should be `email`.
    'contact-name'-atom-true,
    'contact-phone'-atom-true,
    data_dict-skip-true,
    date_released-atom-true,
    date_update_future-atom-true,
    date_updated-atom-true,
    extras-list(extra/_)-false,
    'foi-email'-atom-true, % Should be `email`.
    'foi-name'-atom-true,
    'foi-phone'-atom-true,
    'foi-web'-atom-true,
    geographic_coverage-list(atom)-true,
    geographic_granularity-atom-true,
    'geographic_granularity-other'-atom-true,
    groups-list(group)-false,
    id-atom-false,
    individual_resources-list(resource/_)-true,
    isopen-boolean-false,
    last_major_modification-atom-true,
    license_id-license/_-true,
    license_title-atom-true,
    license_url-atom-true,
    maintainer-atom-true,
    maintainer_email-atom-true, % Should be `email`.
    mandate-atom-true,
    metadata_created-atom-false,
    metadata_modified-atom-false,
    name-atom-false,
    national_statistic-atom-true,
    notes-atom-true,
    num_resources-integer-false,
    num_tags-integer-false,
    organization-group/_-true,
    owner_org-group/_-true,
    precision-atom-true,
    private-boolean-false,
    'publish-date'-atom-true,
    'publish-restricted'-boolean-true,
    published_via-atom-true,
    relationships_as_object-list(relationship/_)-false,
    relationships_as_subject-list(relationship/_)-false,
    'release-notes'-atom-true,
    resources-list(resource/_)-false,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
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
    url-atom-true, % Should be `url`.
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
    object_package_id-atom-false,
    revision_id-atom-false,
    revision_timestamp-dateTime-false,
    state-atom-false,
    subject_package_id-atom-false,
    type-atom-false
  ]
).
legend(
  resource,
  id,
  [
    'API'-atom-false,
    'Author'-atom-false,
    beach_type-atom-true,
    cache_filepath-atom-true,
    cache_last_updated-atom-true,
    cache_url-atom-true,
    cache_url_updated-atom-true,
    ckan_recommended_wms_preview-boolean-true,
    'ClementLevallois'-atom-false,
    'Contact'-atom-false,
    content_length-atom-true,
    content_type-atom-true,
    created-dateTime-true,
    datastore_active-boolean-true,
    date-atom-true,
    description-atom-true,
    format-atom-true,
    'GranularidadeEspacial'-atom-false,
    hash-atom-true,
    'HGNC'-atom-false,
    'HotCasino'-atom-false,
    'http://xmlns.com/foaf/0.1/primaryTopic'-atom-false,
    'hub-id'-atom-true,
    id-atom-false,
    'JumpStartGeorgia'-atom-false,
    lang-atom-false,
    last_modified-dateTime-true,
    license-atom-false, % Does this ever occur?
    'links:dbpedia'-integer-false,
    'links:dnb-gemeinsame-normdatei'-atom-false,
    'links:lemon'-integer-false,
    'links:lexinfo'-integer-false,
    maintainer-atom-false,
    mimetype-atom-true,
    'mimetype-inner'-atom-false,
    mimetype_inner-atom-true,
    name-atom-true,
    namespace-atom-false,
    notes-atom-true,
    opendatavenezia-atom-true,
    openness_score-integer-true,
    openness_score_failure_count-integer-true,
    openness_score_reason-atom-true,
    openspending_hint-atom-true,
    owner-atom-true,
    owner_org-group/_-false,
    'Periodicidade'-atom-false,
    position-integer-false,
    'publish-date'-atom-true,
    'record-count'-atom-false,
    records-integer-false,
    resource_group_id-atom-false,
    resource_locator_function-atom-true,
    resource_locator_protocol-atom-true,
    resource_type-atom-true,
    revision_id-revision/_-false,
    revision_timestamp-dateTime-false,
    scraped-atom-true,
    scraper_source-atom-true,
    scraper_url-atom-true,
    sender-atom-false,
    size-atom-true,
    sparql_graph_name-atom-false,
    'Sparqlendpoint'-atom-true,
    'SpirosAlexiou'-atom-false,
    state-atom-false,
    'State'-atom-false,
    'SvenVlaeminck'-atom-false,
    tracking_summary-tracking_summary/_-false,
    translator-atom-false,
    triples-integer-false,
    type-atom-false,
    url-atom-false, % Should be `url`.
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
    groups-list(group/_)-false,
    id-atom-false,
    message-atom-true,
    packages-list(package/_)-false,
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
    revision_timestamp-dateTime-false,
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
  id,
  [
    about-atom-true,
    activity_streams_email_notifications-boolean-false,
    capacity-atom-false,
    created-dateTime-false,
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
    HTTP_O1 = [never_give_up(true),request_header('Authorization'=Key)]
  ;
    HTTP_O1 = []
  ),

  JSON_In = json(Parameters),
  merge_options(
    [
      method(post),
      post(json(JSON_In)),
      request_header('Content-Type'='application/json')
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
  json_to_rdf(Graph, ckan, Result, _).

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

