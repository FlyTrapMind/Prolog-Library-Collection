:- module(
  rdf_save_ntriples,
  [
    rdf_save_ntriples/2 % +File:atom
                        % +Options:list
  ]
).

/** <module> RDF save to N-Triples

A simple predicate for emitting RDF data in N-Triples serialization format.
Works with SWI-Prolog's Semweb library.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.

:- thread_local(bnode_map/2).
:- thread_local(bnode_counter/1).



rdf_save_ntriples(File1, O1):-
  absolute_file_name(File1, File2, [access(write)]),
  setup_call_cleanup(
    open(File2, write, Out),
    rdf_write_ntriples(Out, O1),
    close(Out)
  ).


rdf_write_ntriples(Out, O1):-
  retractall(bnode_counter/1),
  assert(bnode_counter(0)),
  option(graph(Graph), O1, _VAR),
  forall(
    rdf(S, P, O, Graph),
    rdf_write_ntriple(Out, S, P, O)
  ).


rdf_write_ntriple(Out, S, P, O):-
  rdf_write_term_space(Out, S),
  rdf_write_term_space(Out, P),
  rdf_write_term_space(Out, O),
  put_char(Out, '.'),
  put_code(Out, 10). % Newline


rdf_write_term_space(Out, Term):-
  rdf_write_term(Out, Term),
  put_char(Out, ' ').


rdf_write_term(Out, BNode):-
  rdf_is_bnode(BNode), !,
  (
    bnode_map(BNode, Id2)
  ->
    true
  ;
    retract(bnode_counter(Id1)),
    Id2 is Id1 + 1,
    assert(bnode_counter(Id2)),
    assert(bnode_map(BNode, Id2))
  ),
  format(Out, '_:~w', [Id2]).
rdf_write_term(Out, literal(type(Datatype,Value))):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  write(Out, '^^'),
  rdf_write_term(Out, Datatype).
rdf_write_term(Out, literal(lang(Language,Value))):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  format(Out, '@~w', [Language]).
rdf_write_term(Out, literal(Value)):- !,
  turtle:turtle_write_quoted_string(Out, Value).
rdf_write_term(Out, IRI):-
  turtle:turtle_write_uri(Out, IRI).

