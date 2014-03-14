:- module(
  ckan_ap,
  [
    ckan_ap/0,
    ckan_ap/1, % +ExtraStages:list(compound)
    ckan_ap/2 % +File:atom
              % +ExtraStages:list(compound)
  ]
).

/** <module> CKAN AP

Automated processes for CKAN data.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_archive_ext)). % Used in AP stage.
:- use_module(ap(ap_download)).
:- use_module(ap(ap_db)).
:- use_module(ap(ap_file_mime)). % Used in AP stage.
:- use_module(ap(ap_file_size)). % Used in AP stage.
:- use_module(ap(ap_rdf_serial)). % Used in AP stage.
:- use_module(ap(ap_void_fetch)). % Used in AP stage.
:- use_module(ap(ap_void_stat)). % Used in AP stage.
:- use_module(ckan(ckan_scrape)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)). % Used in AP stage.
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % MD5
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_name)). % Used in meta-DCG.
:- use_module(rdfs(rdfs_label_build)).



%! ckan_ap is det.
% Run the default stages of the CKAN AP process.

ckan_ap:-
  ckan_ap(_, []).

%! ckan_ap(+ExtraStages:list(compound)) is det.
% Add extra stages to the CKAN AP process.

ckan_ap(ExtraStages):-
  ckan_ap(_, ExtraStages).

%! ckan_ap(+File:atom, +ExtraStages:list(compound)) is det.
% Load the CKAN data from the given file.

ckan_ap(File, ExtraStages):-
  (
    access_file(File, read)
  ->
    file_name(File, _, Graph, _),
    rdf_load([], Graph, File)
  ;
    % Scrape a CKAN site.
    ckan_scrape(Graph)
  ),

  % Load the results of resources that were already processed.
  % Do not fail if the file is not there.
  %(
  %  absolute_file_name(
  %    data(Graph),
  %    TmpFile,
  %    [access(read),extensions([tmp]),file_errors(fail)]
  %  )
  %->
  %  rdf_load([format(turtle)], ap, TmpFile)
  %;
  %  true
  %),

  % Run AP processes for CKAN site.
  thread_create(ckan_ap_site(Graph, ExtraStages), _, []).


%! ckan_ap_site(+Graph:atom, +ExtraStages:list(compound)) is det.
% @arg Graph The atomic name of a CKAN site.
% @arg AP_Stages A list of compound terms that describe AP stages.

ckan_ap_site(Graph, ExtraStages):-
  % Collect datasets.
  % Note that sorting by size makes no sense,
  % since the semantics of the values of `ckan:size` is unknown.
  take_lod_sample(Graph, Resources1),

  % Filter resources that have already been processed previously.
  exclude(already_processed, Resources1, Resources2),

  % DEB
  length(Resources2, NumberOfResources),
  debug(ckan, 'About to process ~:d resources.', [NumberOfResources]),

  create_ap_collection(AP_Collection),
  rdfs_assert_label(AP_Collection, Graph, ap),
  maplist(ckan_ap_site(AP_Collection, ExtraStages), Resources2).


take_lod_sample(Graph, Resources):-
  setoff(
    Resource,
    (
      rdfs_individual_of(Resource, ckan:'Resource'),
      (
        rdf_datatype(Resource, ckan:format, xsd:string, Format, Graph),
        rdf_format(Format)
      ;
        rdf_datatype(Resource, ckan:mimetype, xsd:string, Mimetype, Graph),
        rdf_mimetype(Mimetype)
      )
    ),
    Resources
  ).

already_processed(Resource):-
  once(ap_resource(_, Resource, _)).


%! ckan_ap_site(
%!   +AP_Collection:iri,
%!   +ExtraStages:list(compound),
%!   +Resource:iri
%! ) is det.
% @arg AP_Collection The atomic name of a CKAN site as its `rdfs:label`.
% @arg AP_Stages A list of compound terms that describe AP stages.
% @arg Resource An IRI denoting a CKAN resource.

ckan_ap_site(AP_Collection, ExtraStages, Resource):-
  once(rdfs_label(AP_Collection, Graph)),
  once(rdf_datatype(Resource, ckan:url, xsd:string, URL, _)),

  % The directory name is based on the URL.
  rdf_atom_md5(URL, 1, Hash),
  create_nested_directory(data(Hash), Dir),
  db_add_novel(user:file_search_path(Hash, Dir)),

  create_ap(AP_Collection, AP),
  rdf_assert(AP, ap:resource, Resource, ap),
  rdf_assert_datatype(AP, ap:alias, xsd:string, Hash, ap),
  rdf_assert_datatype(AP, ap:graph, xsd:string, Graph, ap),
  ap(
    [leave_trail(false),reset(true)],
    AP,
    [
      ckan_ap:ap_stage([name('Download')], ckan_download_to_directory),
      ckan_ap:ap_stage([name('Arch')], extract_archives),
      %ckan_ap:ap_stage([name('FetchVoID')], void_fetch),
      ckan_ap:ap_stage(
        [name('toNTriples'),args(['application/n-triples'])],
        ap_rdf_merge_directory
      ),
      ckan_ap:ap_stage([name('FileSize')], file_size),
      ckan_ap:ap_stage([name('VoID')], void_statistics)
    | ExtraStages]
  ).


ckan_download_to_directory(_, ToDir, AP_Stage):-
  ap_stage_resource(AP_Stage, Resource, _),
  rdf_datatype(Resource, ckan:url, xsd:string, URL, _),
  (
    rdf_datatype(Resource, ckan:mimetype, xsd:string, MIME, _)
  ->
    format(atom(Accept), '~w; q=0.9', [MIME])
  ;
    Accept = ''
  ),
  ap_download_to_directory(AP_Stage, ToDir, URL, Accept).


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
%rdf_mimetype('text/xml').

