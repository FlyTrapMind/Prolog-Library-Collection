:- module(
  rdf_serial,
  [
    file_or_rdf_graph/2, % +File:atom
                         % -Graph:atom
    file_to_graph_name/2, % +File:atom
                          % -Graph:atom
    files_or_rdf_graphs/2, % +Files:list(atom)
                           % -Graphs:list(atom)
    rdf_convert/3, % +FromFile:atom
                   % +ToFormat:atom
                   % +ToFile:atom
    rdf_graph_source_file/2, % ?Graph:atom
                             % ?File:atom
    rdf_guess_data_format/2, % +Stream:stream
                             % ?Format:atom
    rdf_new_graph/2, % +Graph1:atom
                     % -Graph2:atom
    rdf_serialization/3, % ?Extension:oneof([nt,triples,ttl,rdf])
                         % ?Format:oneof([ntriples,tripels,turtle,rdf_xml])
                         % ?URI:uri

% RDF LOAD
    rdf_load2/1, % +File:atom
    rdf_load2/2, % +File:atom
                 % +Options:list(nvpair)

% RDF SAVE
    rdf_save2/0,
    rdf_save2/1, % +Graph:atom
    rdf_save2/2 % ?File:atom
                % +Options:list(nvpair)
  ]
).

/** <module> RDF serialization

Helper predicates for loading/saving RDF graphs.

Since it is arduous to establish the serialization format based on
the contents of a file, we assume that the file extensions reliably
reflect the serialization format:
  1. =nt= for N-Triples. Format name =ntriples=.
  2. =triples= for the binary storage format provided by SWI-Prolog.
     Format name =triples=.
  3. =ttl= for Turtle. Format name =turtle=.
  4. =rdf= for RDF/XML. Format name =xml=.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(xml(xml_dom)).

:- db_add_novel(user:prolog_file_type(nt,  ntriples)).
:- db_add_novel(user:prolog_file_type(rdf, rdf     )).
:- db_add_novel(user:prolog_file_type(ttl, turtle  )).

:- debug(rdf_serial).



%! file_or_rdf_graph(+File:atom, -Graph:atom) is det.
% This can be used to allow either a file or a graph.

file_or_rdf_graph(G, G):-
  rdf_graph(G), !.
file_or_rdf_graph(File, G):-
  is_absolute_file_name(File),
  access_file(File, read), !,
  % This is the same method used by rdf_load2/2,
  % when no graph option is given.
  file_to_graph_name(File, G),
  rdf_load2(File, [graph(G)]).

%! file_to_graph_name(+File:atom, -Graph:atom) is det.

file_to_graph_name(File, G2):-
  file_name(File, _Dir, G1, _Ext),
  % Make sure the graph does not already exist.
  rdf_new_graph(G1, G2).

%! files_or_rdf_graphs(+Files:list(atom), -Graphs:list(atom)) is det.
% This can be used to allow a (possibly mixed) list of files and graphs.

files_or_rdf_graphs(Files, Gs):-
  maplist(file_or_rdf_graph, Files, Gs).

%! rdf_convert(
%!   +FromFile:atom,
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToFile:atom
%! ) is det.
% Converts a given file in one format to a new file in a different format.
% The original file is not removed.

rdf_convert(FromFile, ToFormat, ToFile):-
  TempG = rdf_convert,
  rdf_unload_graph(TempG),
  setup_call_cleanup(
    rdf_load2(FromFile, [graph(TempG)]),
    rdf_save2(ToFile, [format(ToFormat), graph(TempG)]),
    rdf_unload_graph(TempG)
  ).

%! rdf_graph_source_file(?Graph:atom, ?File:atom) is nondet.
% Returns the name of the file from which the graph with the given name
% was loaded.

rdf_graph_source_file(G, File2):-
  rdf_graph_property(G, source(Source)),
  uri_components(
    Source,
    uri_components(file, _Authority, File1, _Search, _Fragments)
  ),
  sub_atom(File1, 1, _Length, 0, File2).

%! rdf_guess_data_format(
%!   +In:or(atom,stream),
%!   ?Format:oneof([turtle,xml])
%! ) is det.
% Guess the format of an RDF file from the actual content.
% Currently, this seeks for a valid XML document upto the rdf:RDF
% element before concluding that the file is RDF/XML. Otherwise it
% assumes that the document is Turtle.
%
% @author Based on a predicate written by Jan Wielemaker.
% @author Extended to work with files and
%         registered file extensions by Wouter Beek.

rdf_guess_data_format(Stream, xml):-
  is_stream(Stream),
  xml_doctype(Stream, _), !.
rdf_guess_data_format(File, xml):-
  exists_file(File),
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    xml_doctype(Stream, _),
    close(Stream)
  ), !.
rdf_guess_data_format(File, Format):-
  exists_file(File),
  file_name_extension(_Base, Ext, File),
  rdf_serialization(Ext, Format, _URI), !.
rdf_guess_data_format(_, turtle).

%! rdf_load2(+File:atom) is det.
%! rdf_load2(+Files:list(atom)) is det.
% Load the graph that is stored in the given file.
% Then format is derived from the file itself or from the file extension.
% The graph name is the base of the file name.
%
% @see Wrapper to rdf_load/2.

rdf_load2(Spec):-
  rdf_load2(Spec, []).

%! rdf_load2(+File:atom, +Options:list) is det.
%! rdf_load2(+Files:list(atom), +Options:list) is det.
%! rdf_load2(+Directory:atom, +Options:list) is det.
% Load RDF from a file, a list of files, or a directory.
%
% The following options are supported:
%   * format(+Format:oneof([ntriples,triples,turtle,xml]))
%   * graph(+Graph:atom)
%
% @param Spec Either a file, a list of files, or a directory.
% @param Options A list of name-value pairs.

% Loads multiple files and/or directories.
rdf_load2(Files, O1):-
  is_list(Files), !,
  forall(
    member(File, Files),
    rdf_load2(File, O1)
  ).
% Load all files from a given directory.
rdf_load2(Dir, O1):-
  exists_directory(Dir), !,
  directory_files(Dir, rdf, Files),
  rdf_load2(Files, O1).
% The format and graph are set.
rdf_load2(File, O1):-
  access_file(File, read),
  option(format(Format), O1), nonvar(Format),
  option(graph(G), O1), nonvar(G), !,
  % Combine the given with the standard options.
  merge_options([register_namespaces(true),silent(true)], O1, O2),
  % The real job is performed by a predicate from the semweb library.
  rdf_load(File, O2),
  % Send a debug message notifying that the RDF file was successfully loaded.
  debug(rdf_serial, 'Graph ~w was loaded from file ~w.', [G,File]).
% The graph is missing, extrapolate it from the file.
rdf_load2(File, O1):-
  access_file(File, read),
  \+ (option(graph(G), O1), nonvar(G)), !,
  file_to_graph_name(File, G),
  merge_options([graph(G)], O1, O2),
  rdf_load2(File, O2).
% The format is missing, extrapolate it from the file.
rdf_load2(File, O1):-
  access_file(File, read),
  % Returns the format in case it was a variable.
  \+ (option(format(Format), O1), nonvar(Format)), !,
  rdf_guess_data_format(File, Format),
  merge_options([format(Format)], O1, O2),
  rdf_load2(File, O2).

%! rdf_new_graph(+Graph1:atom, -Graph2:atom) is det.
% Returns a graph name that is close to the given graph name,
% and which it is guaranteed to not already exist.
%
% @param Graph1 The atomic name of the graph the user wants to use.
% @param Graph2 An atomic name that is close to the name the user gave.

% No RDF graph with the given name exists, so it is safe to use.
rdf_new_graph(G, G):-
  \+ rdf_graph(G), !.
% An RDF graph with the same name already exists, so the name is altered.
rdf_new_graph(Graph1, Graph3):-
  var(Graph3),
  split_atom_exclusive('_', Graph1, Splits),
  reverse(Splits, [LastSplit | RSplits]),
  (
    atom_number(LastSplit, OldNumber)
  ->
    NewNumber is OldNumber + 1,
    atom_number(NewLastSplit, NewNumber),
    reverse([NewLastSplit | RSplits], NewSplits)
  ;
    reverse(['1', LastSplit | RSplits], NewSplits)
  ),
  atomic_list_concat(NewSplits, '_', Graph2),
  rdf_new_graph(Graph2, Graph3).

%! rdf_save2 is det.
% Saves all currently loaded graphs.
%
% @see Wrapper for rdf_save2/1.

rdf_save2:-
  forall(
    rdf_graph(G),
    rdf_save2(G)
  ).

%! rdf_save2(+Graph:atom) is det.
% Saves the graph with the given name.
%
% Exports are saved in the project directory.
% The Turtle serialization format is used.
%
% @see Wrapper for rdf_save2/2.

rdf_save2(G):-
  absolute_file_name(
    project(G),
    File,
    [access(write),file_type(turtle)]
  ),
  rdf_save2(File, [format(turtle),graph(G)]).

%! rdf_save2(-File, +Options:list) is det.
% If the file name is not given, then a file name is construed.
% There are two variants here:
%   1. The graph was loaded from a file. Use the same file
%      (if we have write access) to this file.
%   2. Make up the file name based on the given graph name.
%      If the format is specified as well, then this is used to determine
%      the file extension.
%
% @param File A variable.
% @param Options A list of options, containing at least =graph/1=
%              and possibly format/1.
%! rdf_save2(+File:atom, +Options:list) is det.
% The following options are supported:
%   * =|format(+Format:oneof([rdf_xml,turtle])|=
%     The serialization format in which the graph is exported.
%   * =|graph(+Graph:atom)|=
%     The name of the graph that is exported.
%
% @param File An atomic absolute file name.
% @param Options A list of name-value pairs.

% Derive the file name from the graph.
% This only works if the graph was loaded form file.
rdf_save2(File, O1):-
  var(File),
  option(graph(G), O1),
  rdf_graph_source_file(G, File),
  access_file(File, write), !,
  % Recurse once, to extract the serialization format.
  rdf_save2(File, O1).
% Derive the file name from the graph name.
% The file is located in the project directory.
rdf_save2(File, O1):-
  var(File),
  option(graph(G), O1), !,
  option(format(Format), O1, turtle),
  absolute_file_name(project(G), File, [access(write),file_type(Format)]),
  rdf_save2(File, O1).
% Make up the format.
rdf_save2(File, O1):-
  access_file(File, write),
  option(graph(G), O1),
  rdf_graph(G),
  \+ option(format(_Format), O1), !,
  file_name_extension(_Base, Ext, File),
  rdf_serialization(Ext, Format, _URI),
  merge_options([format(Format)], O1, O2),
  rdf_save(File, O2).
% Format and graph are both given.
rdf_save2(File, O1):-
  access_file(File, write),
  option(graph(G), O1),
  rdf_graph(G),
  option(format(Format), O1),
  once(rdf_serialization(_Ext, Format, _URI)), !,
  (
    rdf_graph_property(G, modified(true))
  ->
    rdf_save2(File, O1, Format),
    debug(
      rdf_serial,
      'Graph ~w was saved in ~w serialization to file ~w.',
      [G,Format,File]
    )
  ;
    debug(rdf_serial, 'No need to save graph ~w; no updates.', [G])
  ).

%! rdf_save2(
%!   +File:atom,
%!   +Options:list,
%!   +Format:oneof([ntriple,triples,turtle,xml])
%! ) is det.

% Save to N-Triples.
rdf_save2(File, O1, ntriples):- !,
  merge_options(
    [
      align_prefixes(true),
      indent(2),
      only_known_prefixes(true),
      tab_distance(0)
    ],
    O1,
    O2
  ),
  rdf_save_turtle(File, O2).
rdf_save2(File, O1, rdf_xml):- !,
  select_option(format(rdf_xml), O1, O2),
  rdf_save(File, O2).
% Save to Triples (binary storage format).
rdf_save2(File, O1, triples):- !,
  option(graph(G), O1),
  rdf_save_db(File, G).
% Save to Turtle.
rdf_save2(File, O1, turtle):- !,
  % Remove the format option.
  select_option(format(turtle), O1, O2),
  merge_options(
    [
      align_prefixes(true),
      encoding(utf8),
      indent(2),
      only_known_prefixes(true),
      tab_distance(0)
    ],
    O2,
    O3
  ),
  rdf_save_turtle(File, O3).

%! rdf_serialization(
%!   ?Extension:oneof([nt,rdf,triples,ttl]),
%!   ?Serialization:oneof([ntriples,rdf_xml,triples,turtle]),
%!   ?URI:uri
%! ) is nondet.

rdf_serialization(nt, ntriples, 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization(rdf, rdf_xml, 'http://www.w3.org/ns/formats/RDF_XML').
rdf_serialization(triples, triples, '').
rdf_serialization(ttl, turtle, 'http://www.w3.org/ns/formats/Turtle').

