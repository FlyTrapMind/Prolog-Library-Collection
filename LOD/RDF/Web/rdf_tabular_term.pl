:- module(
  rdf_tabular_term,
  [
    rdf_tabular_term//2, % ?Graph:atom
                         % +RDF_Term
    rdf_tabular_terms//2 % ?Graph:atom
                         % +RDF_Terms:list
  ]
).

/** <module> RDF HTML table term

Generates HTML tables for overviews of RDF terms.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_content)). % Meta-argument.
:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_parse)). % Meta-argument.
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular)).
:- use_module(rdf_web(rdf_tabular_class)).
:- use_module(rdf_web(rdf_tabular_property)).



rdf_tabular_term(Graph, Term) -->
  {
    % This allows atomic renditions of prefix-abbreviated IRIs as input,
    % e.g. =|dbpedia:Monkey|=.
    once(dcg_phrase(rdf_parse_term(S1), Term)),
    rdf_global_id(S1, S2)
  },
  rdf_tabular_term1(Graph, S2).

% RDFS class
rdf_tabular_term1(Graph, Class) -->
  {rdfs_individual_of(Class, rdfs:'Class')}, !,
  rdf_tabular_class(Graph, Class).
% RDF property.
% Show:
%   1. all domain classes,
%   2. all range classes,
%   3. all values for literal ranges.
rdf_tabular_term1(Graph, P) -->
  {rdfs_property(P)}, !,
  rdf_tabular_property(Graph, P).
% Datatype (in typed literal).
% Show all its values.
rdf_tabular_term1(Graph, D) -->
  {
    rdf_datatype(_, D), !,
    setoff([Value], rdf_datatype(_, _, D, Value, Graph), Values)
  },
  rdf_html_table(
    [graph(Graph)],
    html(['Ordered value list for datatype ',D,'.']),
    [['Value']|Values]
  ).
% Subject term.
% Display all predicate-object pairs
% and subject-predicate pairs (on a per-graph basis).
rdf_tabular_term1(Graph, RDF_Term) -->
  html([
    h1('Predicate-object pairs'),
    \rdf_tabular_triples(RDF_Term, _, _, Graph),
    h1('Subject-object pairs'),
    \rdf_tabular_triples(_, _, RDF_Term, Graph)
  ]).

rdfs_property(P):-
  rdf(_, P, _).
rdfs_property(P):-
  rdf(P, rdf:type, rdf:'Property').


rdf_tabular_terms(Graph, RDF_Terms) -->
  {
    % Order all resources based on the number of triples describing them.
    setoff(
      NumberOfTriples-RDF_Term,
      (
        member(RDF_Term, RDF_Terms),
        rdf_estimate_complexity(RDF_Term, _, _, NumberOfTriples)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [RDF_Term,NumberOfTriples],
      member(NumberOfTriples-RDF_Term, Pairs3),
      Rows
    )
  },
  rdf_html_table([graph(Graph)], html('RDF terms'), [['RDF term']|Rows]).

