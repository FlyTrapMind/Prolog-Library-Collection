:- module(
  rdf_tabular_property,
  [
    rdf_tabular_property//2, % +Graph:atom
                             % +Property:iri
    rdf_tabular_properties//1 % +Graph:atom
  ]
).

/** <module> RDF tabular predicate term

Generates HTML tables that descrive RDF predicate terms.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular)).



%! rdf_tabular_property(+Graph:atom, +Property:iri)// is det.

rdf_tabular_property(Graph, P) -->
  % The extension of the interpretation of the property consists of pairs.
  % We enumerate the classes of individuals that occur in these pairs.
  % We distinguish between individuals that occur in
  % the first argument position
  % (classes denoting the domain of the property)
  % and the second argument position
  % (classes denoting the range of the property).
  rdf_tabular_property_domain(Graph, P),
  rdf_tabular_property_range(Graph, P),
  
  % For literal ranges we also display the values that occur.
  rdf_tabular_predicate_literals(Graph, P),
  
  % Triples that describe the property, if any.
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
    [graph(Graph)],
    html(['Domain of property ',\rdf_term_name(P),'.']),
    [['Class']|Rows]
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
    [graph(Graph)],
    html(['Range of property ',\rdf_term_name(P),'.']),
    [['Class']|Rows]
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
    [graph(Graph)],
    html(['Values that occur for property ',\rdf_term_name(P),'.']),
    [['Literal value']|Rows2]
  ).


rdf_tabular_properties(Graph) -->
  {
    setoff(
      Predicate,
      rdf_predicate(Graph, Predicate),
      Predicates
    ),
    findall(
      NumberOfOccurrences-Predicate,
      (
        member(Predicate, Predicates),
        aggregate_all(
          count,
          rdf(_, Predicate, _, Graph),
          NumberOfOccurrences
        )
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [Predicate,NumberOfOccurrences],
      member(NumberOfOccurrences-Predicate, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    [graph(Graph)],
    html('Overview of properties.'),
    [['Predicate','Occurrences']|Rows]
  ).

