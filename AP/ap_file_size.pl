:- module(
  ap_file_size,
  [
    file_size/3 % +FromDirectory:atom
                % +ToDirectory:atom
                % +AP_Stage:iri
  ]
).

/** <module> AP file size

File size identification for the AP architecture.

@author Wouter Beek
@version 2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_datatype)).



%! file_size(+FromDirectory:atom, +ToDirectory:atom, +AP_Stage:iri) is det.

file_size(FromDir, ToDir, AP_Stage):-
  directory_files([], FromDir, Files),
  forall(
    member(File, Files),
    (
      size_file(File, Size),
      add_properties_of_file(AP_Stage, File, [file_size-Size]),
      ap_stage_resource(AP_Stage, Resource, Graph),
      rdf_assert_datatype(Resource, ap:file_size, xsd:integer, Size, Graph)
    )
  ),
  link_directory_contents(FromDir, ToDir).

