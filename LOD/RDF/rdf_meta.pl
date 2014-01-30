:- module(
  rdf_meta,
  [
    rdf_setup_call_cleanup/5 % +LoadOptions:list(nvpair)
                             % +FromFile:atom
                             % :Goal
                             % +SaveOptions:list(nvpair)
                             % ?ToFile:atom
  ]
).

/** <module> RDF meta

Meta-callings on an RDF graph.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).



%! rdf_setup_call_cleanup(
%!   +LoadOptions:list(nvpair),
%!   +FromFile:atom,
%!   :Goal,
%!   +SaveOptions:list(nvpair),
%!   ?ToFile:atom
%! ) is det.
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

:- meta_predicate(rdf_setup_call_cleanup(+,+,1,+,?)).
rdf_setup_call_cleanup(O1_Load, From, Goal, O1_Save, ToFile):-
  % If the output file is not given,
  % then it is based on the input file.
  (
    nonvar(ToFile), is_absolute_file_name(ToFile), !
  ;
    rdf_serial:ensure_format(O1_Save, ToFile, ToFormat),
    once(rdf_serialization(ToExt, _, ToFormat, _, _)),
    (
      exists_directory(From)
    ->
      file_name(ToFile, From, output, ttl)
    ;
      From = [FromFile|_]
    ->
      file_alternative(FromFile, _, _, ToExt, ToFile)
    ;
      FromFile = From,
      file_alternative(FromFile, _, _, ToExt, ToFile)
    )
  ),
  
  setup_call_cleanup(
    rdf_new_graph(temp, Graph),
    setup_call_cleanup(
      rdf_load(O1_Load, Graph, From),
      call(Goal, Graph),
      rdf_save(O1_Save, Graph, ToFile)
    ),
    rdf_unload_graph(Graph)
  ).

