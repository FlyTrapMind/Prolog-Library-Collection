:- module(
  ap_archive_ext,
  [
    extract_archives/3 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % +AP_Stage:iri
  ]
).

/** <module> AP: extract archive

Archive extraction process for the AP architecture.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(generics(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(library(apply)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).



%! extact_archives(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   +AP_Stage:iri
%! ) is det.

extract_archives(FromDir, ToDir, AP_Stage):-
  directory_files([recursive(false)], FromDir, FromFiles),
  include(extract_archive0(AP_Stage), FromFiles, ConvertedFiles),
  link_directory_contents(FromDir, ToDir),
  (
    ConvertedFiles == []
  ->
    rdf_assert_individual(AP_Stage, ap:'Skip', ap),
    rdf_assert_datatype(AP_Stage, ap:status, xsd:string, skip, ap)
  ;
    true
  ).

extract_archive0(AP_Stage, FromFile):-
  extract_archive(FromFile, Conversions),
  add_operation_on_file(
    AP_Stage,
    FromFile,
    'archive extraction',
    Conversions
  ).

