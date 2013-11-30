:- module(
  rdf_serial,
  [
    rdf_graph_source_file/2, % +Graph:atom
                             % -File:atom
    rdf_guess_data_format/2, % +Stream:stream
                             % ?Format:atom
    rdf_load2/1, % +File:atom
    rdf_load2/2, % +File:atom
                 % +Options:list(nvpair)
    rdf_loads/2, % +Files:list(atom)
                 % +Graph:atom
    rdf_save2/0,
    rdf_save2/1, % +Graph:atom
    rdf_save2/2, % ?File:atom
                 % +Options:list(nvpair)
    rdf_serialization/4 % ?DefaultExtension:oneof([nt,rdf,triples,ttl])
                        % ?DefaultFileType:oneof([ntriples,rdf_xml,turtle])
                        % ?Format:oneof([ntriples,rdf_xml,triples,turtle])
                        % ?URL:atom
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
@tbd Writing in the N-triples format is not supported.
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11
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
:- use_module(rdf(rdf_graph_name)).
:- use_module(xml(xml_dom)).

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

:- nodebug(rdf_serial).



%! rdf_graph_source_file(+Graph:atom, -File:atom) is nondet.
% Returns the name of the file from which the graph with the given name
% was loaded.

rdf_graph_source_file(G, F2):-
  rdf_graph_property(G, source(Source)),
  uri_components(
    Source,
    uri_components(file, _Authority, F1, _Search, _Fragments)
  ),
  sub_atom(F1, 1, _Length, 0, F2).

%! rdf_guess_data_format(
%!   +In:or(atom,stream),
%!   ?Format:oneof([turtle,xml])
%! ) is det.
% Guess the format of an RDF file from the actual content.
% Currently, this seeks for a valid XML document upto the rdf:RDF
% element before concluding that the file is RDF/XML. Otherwise it
% assumes that the document is Turtle.
%
% @author Jan Wielemaker, predicate taken from ClioPatria codebase.
% @author Wouter Beek, extended to work with files
%         and registered file extensions.

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
  rdf_serialization(Ext, _FileType, Format, _URL), !.
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
%   * format(+Format:oneof([ntriples,turtle,xml]))
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
  directory_files(
    [file_types([rdf]),include_directories(false),recursive(true)],
    Dir,
    Files
  ),
  rdf_load2(Files, O1).
% The format and graph are set.
rdf_load2(File, O1):-
  access_file(File, read),
  option(format(Format), O1), nonvar(Format),
  option(graph(G), O1), nonvar(G), !,
  % XML namespace prefixes must be added explicitly.
  merge_options([register_namespaces(false)], O1, O2),
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

%! rdf_loads(+Files:list(atom), +Graph:atom) is det.

rdf_loads(Fs, G):-
  maplist(rdf_loads_(G), Fs).

%! rdf_loads_(+Graph:atom, +File:atom) is det.

rdf_loads_(G, F):-
  setup_call_cleanup(
    (
      rdf_new_graph(temp, TmpG, 'Load multiple files into one graph.'),
      rdf_load2(F, [graph(TmpG)])
    ),
    forall(
      rdf(S, P, O, TmpG),
      rdf_assert(S, P, O, G)
    ),
    rdf_unload_graph(TmpG)
  ).

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
  rdf_graph_source_file(G, F),
  rdf_save2(F, [format(turtle),graph(G)]).

%! rdf_save2(?File, +Options:list) is det.
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
%   * =|format(+Format:oneof([ntriples,rdf_xml,turtle])|=
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
  rdf_serialization(Ext, _FileType, Format, _URL),
  merge_options([format(Format)], O1, O2),
  rdf_save2(File, O2).
% Format and graph are both given.
rdf_save2(File, O1):-
  access_file(File, write),
  option(graph(G), O1),
  rdf_graph(G),
  select_option(format(Format), O1, O2),
  % Check whether this is a legal format.
  once(rdf_serialization(_, _FileType, Format, _)), !,
  (
    % We do not need to save the graph if
    % (1) the contents of the graph did not change, and
    % (2) the serialization format of the graph did not change.
    
    % Make sure the contents of the graph were not changed.
    rdf_graph_property(G, modified(false)),
    
    % Make sure the serialization format under which the graph was saved
    % did not change.
    rdf_graph_source_file(G, FromFile),
    file_name_type(_, FromFileType, FromFile),
    rdf_serialization(_, FromFileType, Format, _)
  ->
    debug(rdf_serial, 'No need to save graph ~w; no updates.', [G])
  ;
    rdf_save2(File, O2, Format),
    debug(
      rdf_serial,
      'Graph ~w was saved in ~w serialization to file ~w.',
      [G,Format,File]
    )
  ).

% Save to RDF/XML
rdf_save2(File, O1, rdf_xml):- !,
  rdf_save(File, O1).
% Save to Triples (binary storage format).
rdf_save2(File, O1, triples):- !,
  option(graph(G), O1),
  rdf_save_db(File, G).
% Save to Turtle.
rdf_save2(File, O1, turtle):- !,
  merge_options(
    [
      align_prefixes(true),
      encoding(utf8),
      indent(2),
      % Use all and only the namespace prefixes that have been created
      % by the user, i.e. rdf_current_namespace/2, but also suggest new ones.
      % @tbd Ask JW whether this is correct.
      only_known_prefixes(true),
      tab_distance(0)%,
      % Use all the namespace prefixes that have been created by the user,
      % i.e. rdf_current_namespace/2, but also suggest new ones.
      % @tbd Ask JW whether this is correct.
      %%%%user_prefixes(true)
    ],
    O1,
    O2
  ),
  rdf_save_turtle(File, O2).

%! rdf_serialization(
%!   ?DefaultExtension:oneof([nt,rdf,triples,ttl]),
%!   ?DefaultFileType:oneof([ntriples,rdf_xml,turtle]),
%!   ?Format:oneof([ntriples,rdf_xml,triples,turtle]),
%!   ?URL:atom
%! ) is nondet.
%
% @param DefaultExtension The default extension of the RDF serialization.
%        RDF serializations may have multiple non-default extensions,
%        e.g. =owl= and =xml= for RDF/XML.
% @param DefaultFileType The default file type of the RDF serialization.
%        Every file type has the non-default file type =rdf=.
% @param Format The format name that is used by the Semweb library.
% @param URL The URL at which the serialization is described, if any.

rdf_serialization(nt,      ntriples, ntriples, 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization(rdf,     rdf_xml,  xml,      'http://www.w3.org/ns/formats/RDF_XML'  ).
rdf_serialization(triples, triples,  triples,  ''                                      ).
rdf_serialization(ttl,     turtle,   turtle,   'http://www.w3.org/ns/formats/Turtle'   ).

