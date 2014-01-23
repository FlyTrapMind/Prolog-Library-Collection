:- module(
  rdf_meta,
  [
    rdf_call_cleanup/3, % +Options:list(nvpair)
                        % :Goal
                        % +Graphs:list(atom)
    rdf_process_directory_files/5, % +FromDirectory:atom
                                   % +FromFileTypes:list(atom)
                                   % +ToDirectory:atom
                                   % +ToFormat:oneof([ntriples,rdf_xml,turtle])
                                   % :Goal
    rdf_process_directory_files/6, % +FromDirectory:atom
                                   % +FromFileTypes:list(atom)
                                   % +ToDirectory:atom
                                   % +ToFormat:oneof([ntriples,rdf_xml,turtle])
                                   % :Goal
                                   % +Arguments:list
    rdf_setup_call_cleanup/3 % +Options:list(nvpair)
                             % :Goal
                             % +Files:list(atom)
  ]
).

/** <module> RDF auto-process

Automated processing of RDF data.

@author Wouter Beek
@version 2013/11-2013/12
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).

:- meta_predicate(rdf_call_cleanup(+,1,+)).
:- meta_predicate(rdf_process_directory_files(+,+,+,+,:)).
:- meta_predicate(rdf_process_directory_files(+,+,+,+,:,+)).
:- meta_predicate(rdf_setup_call_cleanup(+,1,+)).



% ! rdf_call_cleanup(+Options:list(nvpair), :Goal, +Graphs:list(atom)) is det.

rdf_call_cleanup(_O1, Goal, Graphs):-
  setup_call_cleanup(
    rdf_graph_merge(Graphs, Graph),
    call(Goal, Graph),
    rdf_unload_graph(Graph)
  ).

%! rdf_process_directory_files(
%!   +FromDirectory:atom,
%!   +FromFileTypeOrFormats:list(atom),
%!   +ToDirectory:atom,
%!   +ToFileTypeOrFormat:atom,
%!   :Goal
%! )
% `Goal` takes the following arguments:
%   1. from file,
%   2. to file,
%   3. to format, in case `ToFileTypeOrFormat` is a format.

rdf_process_directory_files(FromDir, Froms, ToDir, To, Goal):-
  rdf_process_directory_files(FromDir, Froms, ToDir, To, Goal, []).

rdf_process_directory_files(FromDir, Froms, ToDir, To, Goal, Args1):-
  % Allow for combinations of file types and RDF serialization formats.
  maplist(
    rdf_serialization_or_file_type,
    [To|Froms],
    [ToFileType|FromFileTypes]
  ),

  % If an RDF serialization format has been given,
  % this this should be passed as an argument.
  % This allows the called goal to use the RDF serialization format.
  (
    To \== ToFileType
  ->
    Args2 = [To|Args1]
  ;
    Args2 = Args1
  ),

  process_directory_files(
    FromDir,
    FromFileTypes,
    ToDir,
    ToFileType,
    Goal,
    [To|Args2]
  ).

rdf_serialization_or_file_type(Format, FileType):-
  rdf_serialization(_, FileType, Format, _, _), !.
rdf_serialization_or_file_type(FileType, FileType).

%! rdf_setup_call_cleanup(
%!   +Options:list(nvpair),
%!   :Goal,
%!   +Files:list(atom)
%! ) is det.
% RDF-based variant of setup_call_cleanup/3.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,rdf_xml,triples,turtle])|=
%     The RDF serialization that is used to store the results.
%     Default: =turtle=.
%     This is only used if option =to= is set as well.
%   * =|to(+ToFile:atom)|=
%     The file in which the resultant RDF graph is stored.
%     Default: unset.

rdf_setup_call_cleanup(O1, Goal, FromFiles):-
  setup_call_cleanup(
    (
      rdf_new_graph(temp, TmpGraph),
      rdf_loads(FromFiles, TmpGraph)
    ),
    call(Goal, TmpGraph),
    (
      (
        option(to(ToFile), O1)
      ->
        option(format(Format), O1, turtle),
        rdf_save2(ToFile, [format(Format),graph(TmpGraph)])
      ;
        true
      ),
      rdf_unload_graph(TmpGraph)
    )
  ).

