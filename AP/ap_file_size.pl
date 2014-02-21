:- module(
  ap_file_size,
  [
    file_size/3, % +FromDirectory:atom
                 % +ToDirectory:atom
                 % +AP_Stage:iri
    file_size_filter/4 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % +AP_Stage:iri
                       % +MAX_Size_MB:between(0.0,inf)
  ]
).

/** <module> AP file size

File size identification and filtering for the AP architecture.

@author Wouter Beek
@version 2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).



%! file_size(+FromDirectory:atom, +ToDirectory:atom, +AP_Stage:iri) is det.

file_size(FromDir, ToDir, AP_Stage):-
  directory_files([], FromDir, Files),
  (
    Files == []
  ->
    existence_error('File', 'No files')
  ;
    forall(
      (
        member(File, Files),
        access_file(File, read)
      ),
      (
        size_file(File, Size),
        add_properties_of_file(AP_Stage, File, [file_size-Size]),
        ap_stage_resource(AP_Stage, Resource, Graph),
        rdf_assert_datatype(Resource, ap:file_size, xsd:integer, Size, Graph)
      )
    ),
    link_directory_contents(FromDir, ToDir)
  ).


file_size_filter(FromDir, ToDir, AP_Stage, MAX_Size_MB):-
  rdf_assert_individual(AP_Stage, ap:'Filter', ap),
  directory_files([], FromDir, FromFiles),
  MAX_Size is MAX_Size_MB * 1024 * 1024,
  maplist(file_size_is_smaller_than(MAX_Size), FromFiles),
  link_directory_contents(FromDir, ToDir).

file_size_is_smaller_than(MAX_Size, File):-
  size_file(File, Size),
  Size =< MAX_Size, !.
file_size_is_smaller_than(_, File):-
  permission_error(open, 'BIG-file', File).

