:- module(
  prolog_to_rdf,
  [
    prolog_to_rdf/3 % +Graph:atom
                    % +Module:atom
                    % +Term:term
  ]
).

/** <module> Prolog to RDF

Automated conversion from Prolog terms to RDF triples.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).



prolog_to_rdf(Graph, Module, Term):-
  % DEB
  flag(aap, Id, Id + 1),
  format(current_output, '~d\n', [Id]),
  (Id = 999 -> gtrace ; true),
  
  % Namespace.
  (
    xml_current_namespace(Module, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com',Module,''], '/', URL),
    xml_register_namespace(Module, URL)
  ),
  
  % Class.
  Term =.. [Functor|Args],
  once(dcg_phrase(capitalize, Functor, ClassName)),
  rdf_global_id(Module:ClassName, Class),
  rdfs_assert_class(Class, Graph),
  
  % Individual.
  rdf_bnode(Individual),
  rdf_assert_individual(Individual, Class, Graph),
  
  % Propositions.
  Module:rdf_legend(Functor, ArgRequirements),
  maplist(prolog_to_rdf(Graph, Module, Individual), Args, ArgRequirements).

prolog_to_rdf(Graph, Module, Individual, Value, PredicateName-Type-Optional):-
  rdf_global_id(Module:PredicateName, Predicate),
  (
    xsd_datatype(Type, Datatype),
    to(Type, Value, Value_)
  ->
    rdf_assert_datatype(Individual, Predicate, Datatype, Value_, Graph)
  ;
    call(Type, Individual, Predicate, Value, Graph)
  ->
    true
  ;
    Optional = true
  ).

to(Type, Value1, Value2):-
  atomic_list_concat([to,Type], '_', Predicate),
  call(Predicate, Value1, Value2).

% Prolog native.
to_boolean(true, true).
to_boolean(false, false).
% Prolog DSL for JSON.
to_boolean(@(true), true).
to_boolean(@(false), false).
% Integer boolean.
to_boolean(1, true).
to_boolean(0, false).
% CKAN boolean.
to_boolean('True', true).
to_boolean('False', false).

atom(S, P, O, G):-
  rdf_assert_literal(S, P, O, G).

