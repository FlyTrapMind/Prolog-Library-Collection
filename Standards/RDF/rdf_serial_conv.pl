:- module(
  rdf_serial_conv,
  [
    rdf_convert_directory/4, % +FromDirectory:atom
                             % +FromFormat:list(oneof([ntriples,triples,turtle,xml]))
                             % +ToFormat:oneof([ntriples,triples,turtle,xml])
                             % +ToDirectory:atom
    rdf_convert_file/3 % +FromFile:atom
                       % +ToFormat:atom
                       % +ToFile:atom
  ]
).

/** <module> RDF serialization conversion

Easily converts between different RDF serializations.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).



%! rdf_convert_directory(
%!   +FromDirectory:atom,
%!   +FromFormats:list(oneof([ntriples,triples,turtle,xml])),
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToDirectory:atom
%! ) is det.

rdf_convert_directory(FromDir, FromFormats, ToFormat, ToDir):-
  findall(
    FromFileType,
    (
      member(FromFormat, FromFormats),
      rdf_serialization(_, FromFileType, FromFormat, _)
    ),
    FromFileTypes
  ),
  directory_files(
    [
      file_types(FromFileTypes),
      include_directories(false),
      order(lexicographic),
      recursive(true)
    ],
    FromDir,
    FromFiles
  ),
  % Look up the default file type for the new serialization format.
  rdf_serialization(_, ToFileType, ToFormat, _),
  forall(
    member(FromFile, FromFiles),
    (
      relative_file_name(FromFile, FromDir, FromRelativeFile),
      % Change the extension of the new file
      % to reflect the new serialization format.
      file_type_alternative(FromRelativeFile, ToFileType, ToRelativeFile),
      directory_file_path(ToDir, ToRelativeFile, ToFile),
      % The directory structure may not yet exist.
      create_file_directory(ToFile),
      rdf_convert_file(FromFile, ToFormat, ToFile)
    )
  ).

%! rdf_convert_file(
%!   +FromFile:atom,
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToFile:atom
%! ) is det.
% Converts a given file in one format to a new file in a different format.
% The original file is not removed.

rdf_convert_file(FromFile, ToFormat, ToFile):-
  rdf_setup_call_cleanup(
    [format(ToFormat),to(ToFile)],
    % Idle wheel, i.e. a predicate which we know succeeds
    % for every graph argument.
    rdf_graph,
    [FromFile]
  ).

