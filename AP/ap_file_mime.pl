:- module(
  ap_file_mime,
  [
    mime_dir/3 % +FromDirectory:atom
               % +ToDirectory:atom
               % +AP_Stage:iri
  ]
).

/** <module> AP file MIME

File MIME type identification for the AP architecture.

@author Wouter Beek
@version 2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ckan, 'http://www.wouterbeek.com/ckan#').



%! mime_dir(+FromDirectory:atom, +ToDirectory:atom, +AP_Stage:iri) is det.

mime_dir(FromDir, ToDir, AP_Stage):-
  directory_files([], FromDir, FromFiles),
  (
    FromFiles == []
  ->
    existence_error('File', 'No files')
  ;
    maplist(mime_file(AP_Stage), FromFiles)
  ),
  link_directory_contents(FromDir, ToDir).


mime_file(AP_Stage, File):-
  ap_stage_resource(AP_Stage, Resource, Graph),
  rdf_datatype(Resource, ckan:mimetype, xsd:string, MIME1, Graph),
  (
    file_mime(File, MIME2)
  ->
    (MIME1 \== MIME2 -> debug(ap, '[MIME] ~w -> ~w', [MIME1,MIME2]) ; true)
  ;
    MIME2 = MIME1
  ),
  add_properties_of_file(AP_Stage, File, ['MIME'-MIME2]),
  rdf_assert_datatype(Resource, ap:mime, xsd:string, MIME2, Graph).

