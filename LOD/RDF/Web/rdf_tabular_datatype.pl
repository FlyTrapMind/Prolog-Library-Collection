:- module(
  rdf_tabular_datatype,
  [
    rdf_tabular_datatype//2, % ?RdfGraph:atom
                             % +Datatype:iri
    rdf_tabular_datatypes//1 % ?RdfGraph:atom
  ]
).

/** <module> RDF tabular datatype

Generated HTML overviews of singular and of multiple
datatype IRIs.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_term_html)).

:- rdf_meta(rdf_tabular_datatype(?,r,?,?)).



%! rdf_tabular_datatype(?RdfGraph:atom, +Datatype:iri)// is det.

% Datatype =|rdf:langString|= has no interesting value (= lexical form)
% but does have a language tag.
rdf_tabular_datatype(G, D) -->
  {
    rdf_equal(rdf:langString, D), !,
    setoff(
      N-[LexicalForm,LangTag],
      (
        rdf_language_tagged_string(Literal, G),
        rdf_language_tagged_string(Literal, LexicalForm, LangTag),
        rdf_triples_by_object(G, literal(lang(LangTag,LexicalForm)), N)
      ),
      Pairs
    )
  },
  rdf_tabular_datatype_table(G, D, Pairs, 'Language tag').
% All other datatypes.
rdf_tabular_datatype(G, D) -->
  {
    setoff(
      N-[LexicalForm,Value],
      (
        rdf_literal(_, _, LexicalForm, D, _, G),
        rdf_literal_map(LexicalForm, D, _, Value),
        rdf_triples_by_object(G, literal(type(D,LexicalForm)), N)
      ),
      Pairs
    )
  },
  rdf_tabular_datatype_table(G, D, Pairs, 'Value').


rdf_tabular_datatype_table(G, D, Pairs1, ColumnHeader) -->
  {
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [H|T],
      member(H-T, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    [graph(G)],
    html(['Overview of datatype ',\rdf_term_in_graph_html(D, G)]),
    [['Lexical expression',ColumnHeader]|Rows]
  ).



%! rdf_tabular_datatypes(?RdfGraph:atom)// is det.
% Generates an HTML enumeration of datatypes in the given graph (if given),
% sorted by the number of occurrences (as a proxy for relevance).

rdf_tabular_datatypes(G) -->
  {
    setoff(
      N-D,
      (
        rdf_datatype(D, G),
        rdf_triples_by_datatype(G, D, N)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [N,D],
      member(N-D, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    [graph(G)],
    html(['Overview of datatype ',\rdf_term_in_graph_html(D, G)]),
    [['Number of literals','Datatype']|Rows]
  ).

