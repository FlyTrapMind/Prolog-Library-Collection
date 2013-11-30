:- module(
  rdf_meta,
  [
    rdf_call_cleanup/3, % +Options:list(nvpair)
                        % :Goal
                        % +Graphs:list(atom)
    rdf_setup_call_cleanup/3 % +Options:list(nvpair)
                             % :Goal
                             % +Files:list(atom)
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
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).

:- meta_predicate(rdf_call_cleanup(+,1,+)).
:- meta_predicate(rdf_setup_call_cleanup(+,1,+)).



% ! rdf_call_cleanup(+Options:list(nvpair), :Goal, +Graphs:list(atom)) is det.

rdf_call_cleanup(_O1, Goal, Graphs):-
  setup_call_cleanup(
    rdf_graph_merge(Graphs, Graph),
    call(Goal, Graph),
    rdf_clean_graph(Graph)
  ).

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
      rdf_new_graph(temp, TmpGraph, 'Use in rdf_setup_call_cleanup/3.'),
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
      rdf_clean_graph(TmpGraph)
    )
  ).

