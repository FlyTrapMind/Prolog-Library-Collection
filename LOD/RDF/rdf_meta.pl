:- module(
  rdf_meta,
  [
    rdf_setup_call_cleanup/4 % :Goal
                             % +FromFiles:list(atom)
                             % +ToFormat:oneof([ntriples,triples,turtle,xml])
                             % +ToFile:atom
  ]
).

/** <module> RDF auto-process

Automated processing of RDF data.

@author Wouter Beek
@version 2013/11-2014/01
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).



%! rdf_setup_call_cleanup(
%!   :Goal,
%!   +FromFiles:list(atom),
%!   +ToFormat:oneof([ntriples,triples,turtle,xml]),
%!   +ToFile:atom
%! ) is det.
% RDF-based variant of setup_call_cleanup/3.
%
% @arg Goal Takes one argument, which is instantiated with
%      an RDF graph name.
% @arg FromFiles A list of atomic file names.
% @arg ToFormat The RDF serialization format the result is stored in.
%      Possible values: `ntriples`, `triples`, `turtle`, `xml`.
% @arg ToFile The file in which the resultant RDF graph is stored.

:- meta_predicate(rdf_setup_call_cleanup(1,+,+,+)).
rdf_setup_call_cleanup(Goal, FromFiles, ToFormat, ToFile):-
  setup_call_cleanup(
    (
      rdf_new_graph(temp, Graph),
      rdf_load_into_one_graph(FromFiles, Graph)
    ),
    call(Goal, Graph),
    (
      rdf_save2(ToFile, [format(ToFormat),graph(Graph)]),
      rdf_unload_graph(Graph)
    )
  ).

