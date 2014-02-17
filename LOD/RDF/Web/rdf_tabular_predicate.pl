:- module(
  rdf_tabular_predicate,
  [
    rdf_tabular_predicate//2, % +Graph:atom
                              % +Predicate:iri
    rdf_tabular_predicates//1 % +Graph:atom
  ]
).

/** <module> RDF tabular predicate term

Generates HTML tables that descrive RDF predicate terms.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular)).



%! rdf_tabular_predicate(+Graph:atom, +Predicate:iri)// is det.

rdf_tabular_predicate(Graph, P) -->
  rdf_tabular_property_domain(Graph, P),
  rdf_tabular_property_range(Graph, P),
  % For literal ranges we also display the values that occur.
  rdf_tabular_predicate_literals(Graph, P),
  rdf_tabular_triples(P, _, _, Graph).


rdf_tabular_property_domain(Graph, P) -->
  {setoff(
    [Domain],
    (
      rdf(S, P, _, Graph),
      rdfs_individual_of(S, Domain)
    ),
    Rows
  )},
  rdf_html_table(
    Graph,
    (`Domain of property `, rdf_term_name(P), `.`),
    ['Class'],
    Rows
  ).


rdf_tabular_property_range(Graph, P) -->
  {setoff(
    [Range],
    (
      rdf(_, P, O, Graph),
      rdfs_individual_of(O, Range)
    ),
    Rows
  )},
  rdf_html_table(
    Graph,
    (`Range of property `, rdf_term_name(P), `.`),
    ['Class'],
    Rows
  ).


rdf_tabular_predicate_literals(Graph, P) -->
  {
    setoff(
      [LiteralValue],
      ((
        rdf(_, P, literal(type(_,LiteralValue)))
      ;
        rdf(_, P, literal(lang(_,LiteralValue)))
      ;
        rdf(_, P, literal(LiteralValue)),
        \+ compound(LiteralValue)
      )),
      Rows1
    ),
    list_truncate(Rows1, 100, Rows2)
  },
  rdf_html_table(
    Graph,
    (`Values that occur for property `, rdf_term_name(P), `.`),
    ['Literal value'],
    Rows2
  ).


rdf_tabular_predicates(Graph) -->
  {
    setoff(
      Property,
      rdf(_, Property, _, Graph),
      Properties
    ),
    findall(
      NumberOfOccurrences-Property,
      (
        member(Property, Properties),
        aggregate_all(
          count,
          rdf(_, Property, _, Graph),
          NumberOfOccurrences
        )
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [Property,NumberOfOccurrences],
      member(NumberOfOccurrences-Property, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    Graph,
    `Overview of properties.`,
    ['Property','Occurrences'],
    Rows
  ).

