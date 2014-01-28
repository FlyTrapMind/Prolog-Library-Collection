:- module(
  rdf_meta,
  [
    rdf_setup_call_cleanup/5 % +FromMIME:atom,
                             % +FromFile:atom,
                             % :Goal,
                             % ?ToMIME:atom,
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
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_serial)).



%! rdf_setup_call_cleanup(
%!   +FromMIME:atom,
%!   +FromFile:atom,
%!   :Goal,
%!   ?ToMIME:atom,
%!   ?ToFile:atom
%! ) .
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

:- meta_predicate(rdf_setup_call_cleanup(+,+,1,+,+)).
rdf_setup_call_cleanup(FromMIME, FromFile, Goal, ToMIME1, ToFile):-
  default(ToMIME1, 'application/x-turtle', ToMIME2),

  % If the output file is not given,
  % then it is based on the input file.
  (
    is_absolute_file_name(ToFile), !
  ;
    once(rdf_serialization(ToExt, _, _, ToMIME2, _)),
    file_alternative(FromFile, _, _, ToExt, ToFile)
  ),
  
  setup_call_cleanup(
    rdf_new_graph(temp, Graph),
    setup_call_cleanup(
      rdf_load2(FromFile, [graph(Graph),mime(FromMIME)]),
      call(Goal, Graph),
      rdf_save2(ToFile, [graph(Graph),mime(ToMIME2)])
    ),
    rdf_unload_graph(Graph)
  ).

