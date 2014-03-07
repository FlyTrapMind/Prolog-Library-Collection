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



rdf_save_ntriples(File1, O1):-
  absolute_file_name(File1, File2, [access(write)]),
  setup_call_cleanup(
    open(File2, write, Stream),
    rdf_write_ntriples(Stream, O1),
    close(Stream)
  ).


rdf_write_ntriples(Stream, O1):-
  option(graph(Graph), O1, _VAR),
  findall(BNode, rdf_bnode(Graph, BNode), BNodes1),
  list_to_ord_set(BNodes1, BNodes2),
  forall(
    rdf(S, P, O, Graph),
    rdf_write_ntriple(Stream, BNodes2, S, P, O)
  ).


rdf_write_ntriple(Stream, BNodes, S, P, O):-
  maplist(rdf_write_term_space(Stream, BNodes), [S,P,O]),
  put_char(Stream, '.'),
  put_code(Stream, 10). % Newline


rdf_write_term_space(Stream, BNodes, Term):-
  rdf_write_term(Stream, BNodes, Term),
  put_char(Stream, ' ').


rdf_write_term(Stream, BNodes, BNode):-
  rdf_is_bnode(BNode), !,
  nth0(I, BNodes, BNode),
  format(Stream, '_:~w', [I]).
rdf_write_term(Stream, BNodes, literal(type(Datatype1,Value1))):- !,
  rdf_write_term(atom(Datatype2), BNodes, Datatype1),
  rdf_replace_lexical_value(Value1, Value2),
  format(Stream, '"~w"^^~w', [Value2,Datatype2]).
rdf_write_term(Stream, _, literal(lang(Language,Value1))):- !,
  rdf_replace_lexical_value(Value1, Value2),
  format(Stream, '"~w"@~w', [Value2,Language]).
rdf_write_term(Stream, _, literal(Value1)):- !,
  rdf_replace_lexical_value(Value1, Value2),
  format(Stream, '"~w"', [Value2]).
rdf_write_term(Stream, _, IRI):-
  format(Stream, "<~w>", [IRI]).


rdf_replace_lexical_value(Value1, Value2):-
  atom_codes(Value1, Codes1),
  rdf_replace_lexical_value_codes(Codes1, Codes2),
  atom_codes(Value2, Codes2).


rdf_replace_lexical_value_codes([], []):- !.
rdf_replace_lexical_value_codes([10|T1], [92,117,48,48,48,97|T2]):- !,
  rdf_replace_lexical_value_codes(T1, T2).
rdf_replace_lexical_value_codes([13|T1], [92,117,48,48,48,100|T2]):- !,
  rdf_replace_lexical_value_codes(T1, T2).
rdf_replace_lexical_value_codes([H|T1], [H|T2]):-
  rdf_replace_lexical_value_codes(T1, T2).


rdf_bnode(Graph, BNode):-
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).


rdf_subject(Graph, Subject):-
  rdf(Subject, _, _, Graph).


rdf_object(Graph, Object):-
  rdf(_, _, Object, Graph).

