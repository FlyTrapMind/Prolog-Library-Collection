:- module(
  rdf_serial_conv,
  [
    rdf_convert_directory/3, % +FromDirectory:atom
                             % +ToFormat:oneof([ntriples,triples,turtle,xml])
                             % +ToDirectory:atom
    rdf_convert_directory/4, % +FromDirectory:atom
                             % +FromFormat:list(oneof([ntriples,triples,turtle,xml]))
                             % +ToFormat:oneof([ntriples,triples,turtle,xml])
                             % +ToDirectory:atom
    rdf_convert_file/3 % +FromFile:atom
                       % +ToFile:atom
                       % +ToFormat:oneof([ntriples,triples,turtle,xml])
  ]
).

/** <module> RDF serialization conversion

Easily converts between different RDF serializations.

@author Wouter Beek
@version 2013/11-2013/12
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).



rdf_convert_directory(FromDir, ToFormat, ToDir):-
  % Notice that we even include the 'to' format,
  % since for instance different versions of the same library
  % may export the same format differently.
  rdf_convert_directory(FromDir, [ntriples,triples,turtle,xml], ToFormat, ToDir).

%! rdf_convert_directory(
%!   +FromDirectory:atom,
%!   +FromFormats:list(oneof([ntriples,triples,turtle,xml])),
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToDirectory:atom
%! ) is det.

rdf_convert_directory(FromDir, FromFormats, ToFormat, ToDir):-
  rdf_process_directory_files(
    FromDir,
    FromFormats,
    ToDir,
    ToFormat,
    rdf_convert_file
  ).

%! rdf_convert_file(
%!   +FromFile:atom,
%!   +ToFile:atom,
%!   +ToFormat:oneof([ntriples,triples,turtle,xml])
%! ) is det.
% Converts a given file in one format to a new file in a different format.
% The original file is not removed.

rdf_convert_file(FromFile, ToFile, ToFormat):-
  rdf_setup_call_cleanup(
    [format(ToFormat),to(ToFile)],
    % Idle wheel, i.e. a predicate which we know succeeds
    % for every graph argument.
    rdf_graph,
    [FromFile]
  ).

