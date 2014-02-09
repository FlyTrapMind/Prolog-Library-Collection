:- module(
  ckan_ap,
  [
    ckan_ap/0,
    ckan_ap/1 % +Extra_AP_Stages:list(compound)
  ]
).

/** <module> CKAN AP

Automated processes for CKAN data.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_archive_ext)).
:- use_module(ap(ap_db)).
:- use_module(ap(ap_file_mime)).
:- use_module(ap(ap_void_stat)).
:- use_module(ckan(ckan_scrape)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(archive_ext)). % Used in AP stage.
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)). % Used in AP stage.
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).
:- use_module(os(file_mime)). % Used in AP stage.
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_name)). % Used in meta-DCG.
:- use_module(rdf(rdf_serial)). % Used in AP stage.



%! ckan_ap is det.
% Run the default stages of the CKAN AP process.

ckan_ap:-
  ckan_ap([]).

%! ckan_ap(+Extra_AP_Stages:list(compound)) is det.
% Add extra stages to the CKAN AP process.

ckan_ap(Extra_AP_Stages):-
  % Scrape a CKAN site.
  ckan_scrape(Site),

  % Run AP processes for CKAN site.
  thread_create(ckan_ap_site(Site, Extra_AP_Stages), _, []).


%! ckan_ap_site(+Site:atom, +Extra_AP_Stages:list(compound)) is det.
% @arg Site The atomic name of a CKAN site.
% @arg AP_Stages A list of compound terms that describe AP stages.

ckan_ap_site(Site, Extra_AP_Stages):-
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
  maplist(ckan_ap_site(Site, Extra_AP_Stages), Resources).

%! ckan_ap_site(
%!   +Site:atom,
%!   +Extra_AP_Stages:list(compound),
%!   +Resource:iri
%! ) is det.
% @arg Site The atomic name of a CKAN site.
% @arg AP_Stages A list of compound terms that describe AP stages.
% @arg Resource An IRI denoting a CKAN resource.

ckan_ap_site(Site1, Extra_AP_Stages1, Resource):-
  dcg_with_output_to(atom(Name), rdf_term_name(Resource)),
  Spec =.. [Site1,Name],
  create_nested_directory(ckan_data(Spec)),
  db_add_novel(user:file_search_path(Name, Spec)),
  atomic_list_concat([Site1,ckan], '_', Site2),
  maplist(
    add_arguments([Resource,Site2]),
    Extra_AP_Stages1,
    Extra_AP_Stages2
  ),

  create_ap(_, AP),
  rdf_assert_datatype(AP, ap:alias, xsd:string, Name, ap),
  rdf_assert(AP, ap:resource, Resource, ap),
  rdf_assert_datatype(AP, ap:graph, xsd:string, Site1, ap),
  ap(
    [graph(Site2),reset(true)],
    AP,
    [
      ap_stage([name('Download')], ckan_download_to_directory),
      ap_stage([name('Arch')], extract_archives),
      ap_stage([name('MIME')], mime_dir),
      ap_stage([name('toTurtle')], rdf_convert_directory),
      ap_stage([name('VoID')], void_statistics)
    | Extra_AP_Stages2]
  ).

ckan_download_to_directory(_, ToDir, AP_Stage):-
  rdf_collection_member(AP_Stage, AP, _),
  rdf(AP, ap:resource, Resource),
  rdf_literal(Resource, ckan:url, URL, _),
  download_to_directory(URL, ToDir, _).

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

