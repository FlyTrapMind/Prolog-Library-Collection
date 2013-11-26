:- module(
  rdf_ap,
  [
    rdf_convert/3, % +FromFile:atom
                   % +ToFormat:atom
                   % +ToFile:atom
    rdf_setup_call_cleanup/3 % +Options:list(nvpair)
                             % :Goal
                             % +FileOrFiles:or([atom,list(atom)])
  ]
).

/** <module> RDF auto-process

Automated processing of RDF data.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).

:- meta_predicate(rdf_setup_call_cleanup(+,1,+)).



%! rdf_convert(
%!   +FromFile:atom,
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToFile:atom
%! ) is det.
% Converts a given file in one format to a new file in a different format.
% The original file is not removed.

rdf_convert(FromFile, ToFormat, ToFile):-
  rdf_setup_call_cleanup(
    [format(ToFormat),to(ToFile)],
    % Idle wheel.
    rdf_graph,
    [FromFile]
  ).

%! rdf_load2s(+FileOrFiles:or([atom,list(atom)]), +Graph:atom) is det.

rdf_load2s(Fs, G):-
  is_list(Fs), !,
  rdf_load2s_(Fs, TmpGs),
  rdf_graph_merge(TmpGs, G),
  maplist(rdf_unload_graph, TmpGs).
rdf_load2s(F, G):-
  rdf_load2(F, [graph(G)]).

rdf_load2s_(Files, Graphs):-
  rdf_load2s_(Files, 1, Graphs).

rdf_load2s_([], _N, []).
rdf_load2s_([File|Files], N1, [Graph|Graphs]):-
  atomic_list_concat([temp,N1], '_', Graph),
  rdf_load2(File, [graph(Graph)]),
  N2 is N1 + 1,
  rdf_load2s_(Files, N2, Graphs).

%! rdf_setup_call_cleanup(
%!   +Options:list(nvpair),
%!   :Goal,
%!   +FileOrFiles:or([atom,list(atom)])
%! ) is det.
% RDF-based variant of setup_call_cleanup/3.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,rdf_xml,triples,turtle])|=
%     The RDF serialization that is used to store the results.
%     Default: =turtle=.
%   * =|to(+ToFile:atom)|=
%     The file in which the resultant RDF graph is stored.

rdf_setup_call_cleanup(O1, Goal, FileOrFiles):-
  % Determine the graph name.
  file_to_graph_name(File, DefaultGraph),
  option(graph(Graph), O1, DefaultGraph),
  option(format(Format), O1, turtle),
  
  setup_call_cleanup(
    rdf_load2s(FileOrFiles, Graph),
    call(Goal, Graph),
    (
      rdf_save2(File, [format(Format),graph(Graph)]),
      rdf_unload_graph(Graph)
    )
  ).

