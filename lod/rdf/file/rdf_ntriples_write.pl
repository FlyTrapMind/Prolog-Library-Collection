:- module(
  rdf_ntriples_write,
  [
    rdf_ntriples_write/2 % +File:atom
                         % +Options:list
  ]
).

/** <module> RDF save to N-Triples

A simple implementation for emitting RDF data in
 N-Triples serialization format, reusing some of the Turtle writer.
Intended to work with RDF data stored using SWI-Prolog's Semweb library.

In N-Triples only short Turtle strings (delimited by a single double quote)
 occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
 is the same as the number of lines in the generated file.

@author Wouter Beek
@author Jan Wielemaker
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@tbd We would like to serialize no duplicate triples.
     Provide this at least as an option.
@version 2014/03-2014/04
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(uri)).

:- thread_local(bnode_counter/1).
:- thread_local(bnode_map/2).



%! rdf_ntriples_write(+File:atom, +Options:list) is det.
% Writes RDF data serialization in the N-Triples format to the given file.
%
% The following options are supported:
%   * =|bnode_base(?Iri:atom)|=
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   * =|graph(?Graph:atom)|=
%     The atomic name of a currently loaded RDF graph,
%     to restrict the triples that are saved,
%     or uninstantiated, in which case
%     all currently loaded triples are saved.
%   * =|number_of_triples(-Triples:nonneg)|=
%     The number of triples that was written.
%
% @arg File The atomic name of a file.
% @arg Options A list of name-value pairs.
%
% @see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-skolemization

rdf_ntriples_write(File1, O1):-
  absolute_file_name(File1, File2, [access(write)]),
  setup_call_cleanup(
    open(File2, write, Out),
    rdf_write_ntriples(Out, O1),
    close(Out)
  ).


%! rdf_write_ntriples(+Output:stream, +Options:list(nvpair))is det.

rdf_write_ntriples(Out, O1):-
  % Reset the blank node store.
  reset_bnode_admin,

  State = state(0),			% number_of_triples

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (
    option(bnode_base(Iri), O1)
  ->
    uri_components(Iri, uri_components(Scheme,Authority,_,_,_)),
    uri_components(IriPrefix, uri_components(Scheme,Authority,_,_,_)),
    atom_concat(IriPrefix, IriPostfix, Iri),
    rdf_atom_md5(IriPostfix, 1, Hash),
    atomic_list_concat(['','.well-known',genid,Hash], '/', Path),
    uri_components(
      BNodePrefix,
      uri_components(Scheme,Authority,Path,_,_)
    )
  ;
    BNodePrefix = '_:'
  ),
  (
    option(graph(Graph), O1)
  ->
    forall(
      rdf(S, P, O, Graph:_),
      ( inc_number_of_triples(State),
	rdf_write_ntriple(Out, S, P, O, BNodePrefix)
      )
    )
  ;
    forall(
      % Avoid duplicate triples.
      rdf(S, P, O),
      ( inc_number_of_triples(State),
	rdf_write_ntriple(Out, S, P, O, BNodePrefix)
      )
    )
  ),

  % Statistics option: number of triples written.
  (
    option(number_of_triples(TripleCount), O1)
  ->
    arg(1, State, TripleCount)
  ;
    true
  ).

inc_number_of_triples(State) :-
  arg(1, State, C0),
  C1 is C0+1,
  nb_setarg(1, State, C1).

rdf_write_ntriple(Out, S, P, O, BNodePrefix):-
  flag(number_of_ntriples, X, X + 1),
  rdf_write_subject(Out, S, BNodePrefix),
  put_char(Out, ' '),
  rdf_write_predicate(Out, P),
  put_char(Out, ' '),
  rdf_write_object(Out, O, BNodePrefix),
  put_char(Out, '.'),
  put_code(Out, 10), !. % Newline

% Typed literal.
rdf_write_object(Out, literal(type(Datatype,Value)), _):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  write(Out, '^^'),
  rdf_write_predicate(Out, Datatype).
% Language-tagged string.
rdf_write_object(Out, literal(lang(Language,Value)), _):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  format(Out, '@~w', [Language]).
% XSD string.
rdf_write_object(Out, literal(Value), _):- !,
  turtle:turtle_write_quoted_string(Out, Value).
% Subject.
rdf_write_object(Out, Term, BNodePrefix):-
  rdf_write_subject(Out, Term, BNodePrefix).


% IRI.
rdf_write_predicate(Out, Iri):-
  turtle:turtle_write_uri(Out, Iri).


% Blank node.
rdf_write_subject(Out, BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  (
    bnode_map(BNode, Id2)
  ->
    true
  ;
    increment_bnode_counter(Id2),
    assert(bnode_map(BNode, Id2))
  ),
  atomic_list_concat([BNodePrefix,Id2], '/', BNodeName),

  % If the blank node is replaced by a well-known IRI,
  % then we use predicate term writer.
  (
    BNodePrefix == '_:'
  ->
    write(Out, BNodeName)
  ;
    rdf_write_predicate(Out, BNodeName)
  ).
% Predicate.
rdf_write_subject(Out, Iri, _):-
  rdf_write_predicate(Out, Iri).



% Blank node administration.

increment_bnode_counter(Id2):-
  retract(bnode_counter(Id1)),
  Id2 is Id1 + 1,
  assert(bnode_counter(Id2)).

reset_bnode_admin:-
  reset_bnode_counter,
  reset_bnode_map.

reset_bnode_counter:-
  retractall(bnode_counter(_)),
  assert(bnode_counter(0)).

reset_bnode_map:-
  retractall(bnode_map(_,_)).

