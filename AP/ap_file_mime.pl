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



%! mime_dir(+FromDirectory:atom, +ToDirectory:atom, +AP_Stage:iri) is det.

mime_dir(FromDir, ToDir, AP_Stage):-
  directory_files([], FromDir, FromFiles),
  forall(
    member(FromFile, FromFiles),
    ((
      file_mime(FromFile, MIME)
    ->
      add_properties_of_file(AP_Stage, FromFile, ['MIME'-MIME]),
      ap_stage_resource(AP_Stage, Resource, Graph),
      rdf_assert_datatype(Resource, ap:mime, xsd:string, MIME, Graph)
    ;
      syntax_error('Unrecognized MIME')
    ))
  ),
  link_directory_contents(FromDir, ToDir).

