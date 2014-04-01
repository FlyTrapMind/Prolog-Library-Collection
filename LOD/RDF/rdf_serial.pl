:- module(
  rdf_serial,
  [
    rdf_directory_files/2, % +Directory:atom
                           % -Files:list(atom)
    rdf_convert_directory/4, % +FromDirectory:atom
                             % +ToDirectory:atom
                             % ?ToMIME:atom
                             % -ToFiles:list(atom)
    rdf_convert_file/4, % +FromMIME:atom
                        % +FromFile:atom
                        % ?ToMIME:atom
                        % ?ToFile:atom
    rdf_download_extract_load/2, % +Url:url
                                 % +Options:list(nvpair)
    rdf_merge_directory/4, % +Options:list(nvpair)
                           % +FromDirectory:atom
                           % +ToFile:atom
                           % +SaveOptions:list(nvpair)
    rdf_load/3, % +Options:list(nvpair)
                % ?Graph:atom
                % +File:atom
    rdf_mime/1, % ?MIME:atom
    rdf_mime_format/2, % ?MIME:atom
                       % ?Format:atom
    rdf_save/3, % +Options:list(nvpair)
                % +Graph:atom
                % ?File:atom
    rdf_serialization/5 % ?DefaultExtension:oneof([nt,rdf,triples,ttl])
                        % ?DefaultFileType:oneof([ntriples,rdf_xml,turtle])
                        % ?Format:oneof([ntriples,rdf_xml,triples,turtle])
                        % ?MIMEs:list(atom)
                        % ?URL:atom
  ]
).

/** <module> RDF serialization

Helper predicates for loading/saving RDF graphs.

Also easily converts between different RDF serializations.

At some point (2014/01/27) I decided that file types indicated by
file extensions are not going to work for LOD,
since most datasets are published in a non-standard way.

@author Wouter Beek
@tbd Writing in the N-triples format is not supported.
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(http(http_download_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(option)).
% rdf_file_type(xml,   xml    ).
% rdf_file_type(rdf,   xml    ).
% rdf_file_type(rdfs,  xml    ).
% rdf_file_type(owl,   xml    ).
% rdf_file_type(htm,   xhtml  ).
% rdf_file_type(html,  xhtml  ).
% rdf_file_type(xhtml, xhtml  ).
% rdf_file_type(trp,   triples).
% rdf_storage_encoding('', plain).
% url_protocol(file).
:- use_module(library(semweb/rdf_db)).
% rdf_open_hook(http,  ...)
% rdf_open_hook(https, ...)
% rdf_storage_encoding(_, gzip).
% url_protocol(http).
% url_protocol(https).
:- use_module(library(semweb/rdf_http_plugin)).
% rdf_file_type(nt,       ntriples).
% rdf_file_type(ntriples, ntriples).
% rdf_file_type(nq,       nquads  ).
% rdf_file_type(nquads,   nquads  ).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_turtle_write)).
% rdf_open_decode(gzip, ...)
% rdf_storage_encoding(gz, gzip)
:- use_module(library(semweb/rdf_zlib_plugin)).
% rdf_file_type(ttl,  turtle).
% rdf_file_type(n3,   turtle).
% rdf_file_type(trig, trig  ).
:- use_module(library(semweb/turtle)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_ntriples_write)).
:- use_module(rdf(rdf_serial)).

:- db_add_novel(user:prolog_file_type(nt,      ntriples)).
:- db_add_novel(user:prolog_file_type(nt,      rdf     )).
:- db_add_novel(user:prolog_file_type(owl,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(owl,     rdf     )).
:- db_add_novel(user:prolog_file_type(rdf,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(rdf,     rdf     )).
:- db_add_novel(user:prolog_file_type(rdfs,    rdf_xml )).
:- db_add_novel(user:prolog_file_type(rdfs,    rdf     )).
:- db_add_novel(user:prolog_file_type(triples, triples)).
:- db_add_novel(user:prolog_file_type(triples, rdf     )).
:- db_add_novel(user:prolog_file_type(ttl,     turtle  )).
:- db_add_novel(user:prolog_file_type(ttl,     rdf     )).
:- db_add_novel(user:prolog_file_type(xml,     rdf_xml )).
:- db_add_novel(user:prolog_file_type(xml,     rdf     )).



%! is_rdf_file(+File:atom) is semidet.
% Succeeds if the given file contains an RDF serialization.

is_rdf_file(File):-
  file_mime(File, MIME),
  rdf_mime(MIME), !.
is_rdf_file(File):-
  file_name_extension(_, Ext, File),
  rdf_extension(Ext, _).


%! rdf_directory_files(+Directory:atom, -RdfFiles:list(atom)) is det.
%! rdf_directory_files(
%!   +Options:list(nvpair),
%!   +Directory:atom,
%!   -RdfFiles:list(atom)
%! ) is det.
% Returns RDF files from the given directory.
% This is based on parsing (the top of) the contents of these files.
%
% @arg Options Passed to directory_files/3.
% @arg Directory The atomic name of a directory.
% @arg RdfFiles A list of atomic file names of RDF files.

rdf_directory_files(Dir, RdfFiles):-
  rdf_directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    Dir,
    RdfFiles
  ).

rdf_directory_files(O1, Dir, RdfFiles):-
  % Retrieve all files.
  directory_files(O1, Dir, Files),
  include(is_rdf_file, Files, RdfFiles).


%! rdf_convert_directory(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   ?ToMIME:atom,
%!   -ToFiles:list(atom)
%! ) is det.

rdf_convert_directory(FromDir, ToDir, ToMime, ToFiles):-
  rdf_directory_files(FromDir, FromFiles),

  default('application/x-turtle', ToMime),
  once((
    rdf_serialization(ToExt, _, _, Mimes, _),
    memberchk(ToMime, Mimes)
  )),

  findall(
    ToFile,
    (
      member(FromFile, FromFiles),
      file_alternative(FromFile, ToDir, _, ToExt, ToFile),
      rdf_convert_file(_, FromFile, ToMime, ToFile)
    ),
    ToFiles
  ).


%! rdf_convert_file(
%!   ?FromMIME:atom,
%!   +FromFile:atom,
%!   +SaveOptions:atom,
%!   ?ToFile:atom
%! ) is det.

rdf_convert_file(FromMIME, FromFile, ToMIME, ToFile):-
  (
    var(FromMIME)
  ->
    LoadOptions = []
  ;
    LoadOptions = [mime(FromMIME)]
  ),
  rdf_setup_call_cleanup(
    LoadOptions,
    FromFile,
    rdf_graph,
    [mime(ToMIME)],
    ToFile
  ).


%! rdf_download_extract_load(+Url:url, +Options:list(nvpair)) is det.

rdf_download_extract_load(Url, O1):-
  url_to_file_name(Url, File),
  % The directory not the file (which may be deleted by now).
  file_directory_name(File, Dir),
  exists_directory(Dir), !,

  % Make sure all files are extracted.
  directory_files([], Dir, Files),
  maplist(extract_archive, Files),

  % These are all the RDF files we can get for this URL.
  rdf_directory_files(Dir, RdfFiles),
  rdf_load(RdfFiles, O1).
rdf_download_extract_load(Url, O1):-
  download_and_extract_to_files([], Url, _),
  rdf_download_extract_load(Url, O1).


rdf_extension(Ext, MIME):-
  rdf_serialization(Ext, _, _, [MIME|_], _).


%! rdf_file_correct_extension(+FromFile:atom, -ToFile:atom) is det.

rdf_file_correct_extension(File1, File2):-
  file_mime(File1, Mime),
  rdf_serialization(Extension, _, _, Mimes, _),
  memberchk(Mime, Mimes),
  file_alternative(File1, _, _, Extension, File2),
  File1 \== File2, !,
  link_file(File1, File2, symbolic).
rdf_file_correct_extension(File, File).


%! rdf_merge_directory(
%!   +Options:list(nvpair),
%!   +FromDirectory:atom,
%!   +ToFile:atom,
%!   +SaveOptions:list(nvpair)
%! ) is det.

rdf_merge_directory(O1, FromDir, ToFile, SaveOptions):-
  rdf_directory_files(FromDir, FromFiles),
  FromFiles \== [],
  rdf_setup_call_cleanup(O1, FromFiles, rdf_graph, SaveOptions, ToFile).


%! rdf_load(
%!   +Option:list(nvpair),
%!   ?Graph:atom,
%!   +Input:or([atom,list(atom)])
%! ) is det.
% Load RDF from a file, a list of files, or a directory.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,turtle,xml]))|=
%   * =|mime(+MIME:oneof(['application/rdf+xml','application/x-turtle','text/plain','text/rdf+n3']))|=
%   * =|void(+LoadVoid:boolean)|=
%
% @arg Options A list of name-value pairs.
% @arg Graph The atomic name of an RDF graph.
% @arg Input Either a file, a list of files, or a directory.

% Download the file hosted at the given URL locally.
rdf_load(O1, Graph, Url):-
  is_of_type(iri, Url), !,
  download_to_file([], Url, File),
  rdf_load(O1, Graph, File).
% Loads multiple files and/or directories.
rdf_load(O1, Graph, Files):-
  is_list(Files),
  forall(
    member(File, Files),
    access_file(File, read)
  ), !,
  maplist(rdf_load(O1, Graph), Files).
% Load all files from a given directory.
rdf_load(O1, Graph, Dir):-
  exists_directory(Dir), !,
  directory_files(
    [file_types([rdf]),include_directories(false),recursive(true)],
    Dir,
    Files
  ),
  rdf_load(O1, Graph, Files).
% Extract archives.
rdf_load(O1, Graph, File1):-
  access_file(File1, read),
  is_archive(File1), !,

  file_name(File1, Directory, _, _),
  extract_archive(File1),
  rdf_directory_files(Directory, Files),

  maplist(rdf_load(O1, Graph), Files).
% Load a single file.
rdf_load(O1, Graph, File):-
  access_file(File, read), !,

  % Retrieve the graph name.
  ensure_graph(File, Graph),

  % Retrieve the RDF format.
  ensure_format(O1, File, Format),

  % XML namespace prefixes must be added explicitly.
  merge_options(
    [format(Format),graph(Graph),register_namespaces(false)],
    O1,
    O2
  ),

  % The real job is performed by a predicate from the semweb library.
  rdf_load(File, O2),

  % Send a debug message notifying that the RDF file was successfully loaded.
  debug(rdf_serial, 'RDF graph was loaded from file ~w.', [File]).



% Load more graphs into another graph.
rdf_load(O1, Graph, Graphs):-
  is_list(Graphs),
  maplist(rdf_graph, Graphs), !,
  maplist(rdf_load(O1, Graph), Graphs).
% Look for described datasets that should also be added.
rdf_load(O1, Graph, Graph):-
  rdf_graph(Graph), !,
  findall(
    Location,
    (
      void_db:void_dataset(Graph, Dataset),
      void_db:void_dataset_location(Graph, Dataset, Location)
    ),
    Locations
  ),
  maplist(rdf_load(O1, Graph), Locations).


%! ensure_format(+Options:list(nvpair), +File:atom, -Format:atom) is det.

% Option: format
ensure_format(O1, _, Format):-
  option(format(Format), O1), !.
% Option: mime
ensure_format(O1, _, Format):-
  option(mime(MIME), O1),
  rdf_serialization(_, _, Format, MIMEs, _),
  memberchk(MIME, MIMEs), !.
% Option: mime + cleaning
ensure_format(O1, File, Format):-
  select_option(mime(MIME), O1, O2), !,
  debug(rdf_serial, 'Unrecognized RDF MIME: ~a.', [MIME]),
  ensure_format(O2, File, Format).
% File extension
ensure_format(_, File, Format):-
  file_name_extension(_, Extension, File),
  rdf_serialization(Extension, _, Format, _, _), !.
% Parse file
ensure_format(_, File, Format):-
  file_mime(File, MIME), !,
  (
    rdf_serialization(_, _, Format, MIMEs, _),
    memberchk(MIME, MIMEs), !
  ;
    throw(error(mime_error(File,'RDF',MIME),_))
  ).
% Oops
ensure_format(_, _, turtle):-
  debug(rdf_serial, 'We cannot establish the serialization format.', []).


ensure_graph(_, Graph):-
  nonvar(Graph), !.
ensure_graph(File, Graph):-
  file_to_graph_name(File, Graph).


rdf_mime(MIME):-
  rdf_serialization(_, _, _, MIMEs, _),
  member(MIME, MIMEs).


%! rdf_mime_format(+MIME:atom, +Format:atom) is semidet.
%! rdf_mime_format(+MIME:atom, -Format:atom) is det.
%! rdf_mime_format(-MIME:atom, +Format:atom) is det.
% Relates RDF media content types and RDF formats.

rdf_mime_format(MIME, Format):-
  rdf_serialization(_, _, Format, MIMEs, _),
  memberchk(MIME, MIMEs).


%! rdf_save(+Options:list, +Graph:atom, ?File:atom) is det.
% If the file name is not given, then a file name is construed.
% There are two variants here:
%   1. The graph was loaded from a file. Use the same file
%      (if we have write access) to this file.
%   2. Make up the file name based on the given graph name.
%      If the format is specified as well, then this is used to determine
%      the file extension.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,rdf_xml,turtle])|=
%     The serialization format in which the graph is exported.
%   * =|mime(+MIME:oneof(['application/rdf+xml','application/x-turtle','text/plain','text/rdf+n3']))|=
%
% @arg Options A list of name-value pairs.
% @arg File An atomic absolute file name.

% Derive the file name from the graph.
% This only works if the graph was loaded form file.
rdf_save(O1, Graph, File2):-
  var(File2), !,
  (
    rdf_graph_property(Graph, source(File1))
  ->
    http_path_correction(File1, File2),
    create_file(File2),
    rdf_save(O1, Graph, File2)
  ;
    instantiation_error(File2)
  ).
% Make up the format.
rdf_save(O1, Graph, File):-
  access_file(File, write),

  % Derive the serialization format.
  ensure_format(O1, File, Format),

  (
    % We do not need to save the graph if
    % (1) the contents of the graph did not change, and
    % (2) the serialization format of the graph did not change.
    %
    % Make sure the contents of the graph were not changed.
    rdf_graph_property(Graph, modified(false)),
    %
    % Make sure the file is the same.
    rdf_graph_property(Graph, source(FromFile1)),
    http_path_correction(FromFile1, FromFile2),
    FromFile2 == File
  ->
    debug(rdf_serial, 'No need to save graph ~w; no updates.', [Graph])
  ;
    rdf_save(O1, Format, Graph, File),
    debug(
      rdf_serial,
      'Graph ~w was saved in ~w serialization to file ~w.',
      [Graph,Format,File]
    )
  ).

% Save to RDF/XML
rdf_save(O1, rdf_xml, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  rdf_save(File, O2).
% Save to N-Triples.
rdf_save(O1, ntriples, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  monkey(File, O2).
% Save to Triples (binary storage format).
rdf_save(_, triples, Graph, File):- !,
  rdf_save_db(File, Graph).
% Save to Turtle.
rdf_save(O1, turtle, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  rdf_save_canonical_turtle(File, O2).


%! rdf_serialization(
%!   ?DefaultExtension:oneof([nt,rdf,triples,ttl]),
%!   ?FileType:oneof([ntriples,rdf_xml,triples,turtle]),
%!   ?Format:oneof([ntriples,xml,triples,turtle]),
%!   ?MIME:list(atom),
%!   ?URL:atom
%! ) is nondet.
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
% @arg DefaultFileType The default file type of the RDF serialization.
%      Every file type has the non-default file type =rdf=.
% @arg Format The format name that is used by the Semweb library.
% @arg MIMEs A list of MIME types.
% @arg URL The URL at which the serialization is described, if any.

rdf_serialization(nq, nquads, nquads, ['application/n-quads'], '').
rdf_serialization(nt, ntriples, ntriples, ['application/n-triples'], 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization(rdf, rdf_xml, xml, ['application/rdf+xml'], 'http://www.w3.org/ns/formats/RDF_XML'  ).
rdf_serialization(trig, trig, trig, ['application/x-trig'], 'http://wifo5-03.informatik.uni-mannheim.de/bizer/trig/').
rdf_serialization(ttl, turtle, turtle, ['application/x-turtle','text/turtle'], 'http://www.w3.org/ns/formats/Turtle'   ).
rdf_serialization(n3, n3, turtle, ['text/n3'], '').

