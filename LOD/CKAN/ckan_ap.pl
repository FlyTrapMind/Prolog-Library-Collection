:- module(
  ckan_ap,
  [
    ckan_ap/1 % +AP_Stages:list(compound)
  ]
).

/** <module> CKAN AP

Automated processes for CKAN data.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap)).
:- use_module(ckan(ckan_scrape)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_lit_read)).



ckan_ap(AP_Stages):-
  ckan_scrape(Site),
  ckan_ap(Site, AP_Stages).

ckan_ap(Site, AP_Stages):-
  % Collect datasets.
  % Note that sorting by size makes no sense,
  % since the semantics of the values of `ckan:size` is unknown.
  setoff(
    Resource,
    (
      rdfs_individual_of(Resource, ckan:'Resource'),
      (
        rdf_literal(Resource, ckan:format, Format, Site),
        rdf_format(Format)
      ;
        rdf_literal(Resource, ckan:mimetype, Mimetype, Site),
        rdf_mimetype(Mimetype)
      )
    ),
    Resources
  ),
  maplist(ckan_ap(Site, AP_Stages), Resources).

ckan_ap(Site1, AP_Stages1, Resource):-
  once(rdf_literal(Resource, ckan:url, URL, Site1)),
  once(rdf_literal(Resource, ckan:id, ResourceId, Site1)),
  once(rdf_literal(Resource, ckan:format, ResourceFormat, Site1)),
  (
    once(rdf_literal(Resource, ckan:resource_type, ResourceType, Site1))
  ->
    atomic_list_concat([ResourceId,ResourceFormat,ResourceType,URL], '\n', X1)
  ;
    atomic_list_concat([ResourceId,ResourceFormat,URL], '\n', X1)
  ),
  debug(semuri, 'Starting:\n~w', [X1]),

  once(rdf(Package, ckan:resources, Resource, Site1)),
  once(rdf_literal(Package, ckan:name, PackageName, Site1)),
  once(rdf_literal(Package, ckan:title, PackageTitle, Site1)),
  atomic_list_concat([PackageName,PackageTitle], '\n', X2),

  once(rdf(Package, ckan:organization, Organization, Site1)),
  once(rdf_literal(Organization, ckan:display_name, OrganizationName, Site1)),

  setoff(
    UserName,
    (
      rdf(Organization, ckan:users, User, Site1),
      rdf_literal(User, ckan:fullname, UserName, Site1)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', UserName),

  setoff(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site1),
      rdf_literal(Tag, ckan:name, TagName, Site1)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', TagName),

  % DEB
  flag(datasets, Id, Id + 1),
  format(user_output, '~w\n', [Id]),

  atomic_list_concat([Id,PackageName], '-', Name),
  Spec =.. [Site1,Name],
  create_nested_directory(ckan_data(Spec)),
  db_add_novel(user:file_search_path(Name, Spec)),
  
  atomic_list_concat([Site1,semuri], '_', Site2),
  maplist(add_arguments([Resource,Site2]), AP_Stages1, AP_Stages2),
  ap(
    [graph(Site2),reset(true)],
    Name,
    [
      ap_stage([], download_to_directory(URL)),
      ap_stage([], extract_archives),
      ap_stage([], mime_dir),
      ap_stage([], rdf_convert_directory)
    | AP_Stages2],
    Rows
  ),
  assert(semuri:row([X1,X2,OrganizationName,UserName,TagName|Rows])).

add_arguments(_, [], []).
add_arguments(Args1, [ap_stage(O1,Pred)|T1], [ap_stage(O3,Pred)|T2]):-
  select_option(args(Args2), O1, O2, []),
  append(Args1, Args2, Args3),
  merge_options([args(Args3)], O2, O3),
  add_arguments(Args1, T1, T2).

rdf_format('RDF').
rdf_format('XML').
rdf_format('application/n-triples').
rdf_format('application/rdf+xml').
rdf_format('application/x-nquads').
rdf_format('application/x-ntriples').
rdf_format('example/n3').
rdf_format('example/ntriples').
rdf_format('example/rdf xml').
rdf_format('example/rdf+json').
rdf_format('example/rdf+json').
rdf_format('example/rdf+ttl').
rdf_format('example/rdf+xml').
rdf_format('example/rdfa').
rdf_format('example/turtle').
rdf_format('example/x-turtle').
rdf_format('html+rdfa').
rdf_format('linked data').
rdf_format('mapping/owl').
rdf_format('meta/owl').
rdf_format('meta/rdf-schema').
rdf_format('meta/void').
rdf_format(owl).
rdf_format('rdf-n3').
rdf_format('rdf-turtle').
rdf_format('rdf-xml').
rdf_format('rdf/n3').
rdf_format('rdf/turtle').
rdf_format('rdf/xml, html, json').
rdf_format('text/n3').
rdf_format('text/turtle').

rdf_mimetype('application/rdf+n3').
rdf_mimetype('application/rdf+xml').
rdf_mimetype('application/turtle').
rdf_mimetype('text/n3').
rdf_mimetype('text/json').
rdf_mimetype('text/rdf+n3').
rdf_mimetype('text/turtle').
rdf_mimetype('text/xml').

