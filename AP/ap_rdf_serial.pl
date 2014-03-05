:- module(
  ap_rdf_serial,
  [
    ap_rdf_convert_directory/4 % +FromDirectory:atom
                               % +ToDirectory:atom
                               % -AP_Status:compound
                               % ?MIME:atom
  ]
).

/** <module> AP RDF serial

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(generics(error_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(rdf(rdf_serial)).



ap_rdf_convert_directory(FromDir, ToDir, AP_Stage, MIME1):-
  default(MIME1, 'application/x-turtle', MIME2),
  rdf_convert_directory(FromDir, ToDir, MIME2, ToFiles),
  (
    ToFiles == []
  ->
    existence_error('RDF file', 'No RDF files')
  ;
    maplist(ap_rdf_convert_directory_assertion(AP_Stage), ToFiles)
  ).

ap_rdf_convert_directory_assertion(AP_Stage, File):-
  add_operation_on_file(AP_Stage, File, 'RDF conversion', []).

