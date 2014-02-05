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
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_parse)). % Meta-argument.
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular)).
:- use_module(rdf_web(rdf_tabular_predicate)).



rdf_tabular_term(Graph, Term) -->
  {
    % This allows atomic renditions prefix-abbreviated IRIs as input,
    % e.g. =|dbpedia:Monkey|=.
    once(dcg_phrase(rdf_parse_term(S1), Term)),
    rdf_global_id(S1, S2)
  },
  rdf_tabular_term1(Graph, S2).

% Datatype (in typed literal).
% Show all its values.
rdf_tabular_term1(Graph, D) -->
  {
    rdf_datatype(_, D), !,
    setoff([Value], rdf_datatype(_, _, D, Value, Graph), Values)
  },
  rdf_html_table(
    Graph,
    (`Ordered value list for datatype `, atom(D), `.`),
    ['Value'],
    Values
  ).
% Predicate term.
% Show:
%   1. all domain classes,
%   2. all range classes,
%   3. all values for literal ranges.
rdf_tabular_term1(Graph, P) -->
  {rdf(_, P, _, Graph)}, !,
  rdf_tabular_predicate(Graph, P).
% Subject term.
% Display all predicate-object pairs (per graph).
rdf_tabular_term1(Graph, S) -->
  rdf_tabular_triples(S, _, _, Graph).


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
  rdf_html_table(
    Graph,
    `RDF terms`,
    ['RDF term'],
    Rows
  ).

