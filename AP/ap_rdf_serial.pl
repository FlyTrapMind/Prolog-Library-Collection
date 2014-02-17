:- module(
  ap_rdf_serial,
  [
    rdf_convert_directory/3 % +FromDirectory:atom
                            % +ToDirectory:atom
                            % -AP_Status:compound
  ]
).

/** <module> AP RDF serial

@author Wouter Beek
@version 2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(generics(error_ext)).
:- use_module(library(apply)).
:- use_module(rdf(rdf_serial)).



rdf_convert_directory(FromDir, ToDir, AP_Stage):-
gtrace,
  rdf_convert_directory(FromDir, ToDir, 'application/x-turtle', ToFiles),
  (
    ToFiles == []
  ->
    existence_error('RDF file', 'No RDF files')
  ;
    maplist(rdf_convert_directory_assertion(AP_Stage), ToFiles)
  ).

rdf_convert_directory_assertion(AP_Stage, File):-
  add_operation_on_file(AP_Stage, File, 'RDF conversion', []).

