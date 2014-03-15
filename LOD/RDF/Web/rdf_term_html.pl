:- module(
  rdf_term_html,
  [
    rdf_graph_html//1, % +RdfGraph:atom
    rdf_term_html//1, % +RdfTerm
    rdf_term_html//2 % +RdfGraph:atom
                     % +RdfTerm
  ]
).

/** <module> RDF HTML

HTML generation of RDF content.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(html(pl_term_html)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(server(web_ui)).
:- use_module(xml(xml_namespace)).



% TERM %

rdf_term_html(RDF_Term) -->
  rdf_term_html(_, RDF_Term).

% Graph.
rdf_term_html(_, Graph1) -->
  {rdf_is_graph(Graph1, Graph2)}, !,
  rdf_graph_html(Graph2).
rdf_term_html(_, RDF_Term) -->
  {
    rdf_is_resource(RDF_Term),
    rdfs_individual_of(RDF_Term, rdf:'List')
  }, !,
  rdf_list_html(RDF_Term).
% Blank node.
rdf_term_html(_, RDF_Term) -->
  {rdf_is_bnode(RDF_Term)}, !,
  rdf_blank_node_html(RDF_Term).
% Literal.
rdf_term_html(Graph, RDF_Term) -->
  {rdf_is_literal(RDF_Term)}, !,
  rdf_literal_html(Graph, RDF_Term).
% IRI.
rdf_term_html(Graph, RDF_Term) -->
  rdf_iri_html(Graph, RDF_Term).
% Prolog term.
rdf_term_html(_, PL_Term) -->
  html(span(class='prolog-term', \pl_term_html(PL_Term))).



% GRAPH %

rdf_graph_html(Graph) -->
  {
    http_absolute_location(root(rdf_tabular), Location1, []),
    uri_query_add(Location1, graph, Graph, Location2)
  },
  html(span(class='rdf-graph', a(href=Location2, Graph))).


% RDF LIST %

rdf_list_html(RDF_List) -->
  html(div(class='rdf-list', \rdf_term_name(RDF_List))).


% BLANK NODE %

rdf_blank_node_html(BNode) -->
  {
    atom(BNode),
    atom_prefix(BNode, '__'), !,
    http_absolute_location(root(rdf_tabular), Location1, []),
    uri_query_add(Location1, term, BNode, Location2)
  },
  html(div(class='blank-node', a(href=Location2, BNode))).



% LITERAL %

rdf_language_tag_html(Language) -->
  html(span(class='language-tag', atom(Language))).

rdf_literal_html(_, Type) -->
  rdf_plain_literal_html(Type).
rdf_literal_html(Graph, Type) -->
  rdf_typed_literal_html(Graph, Type).

rdf_plain_literal_html(literal(lang(Language,Value))) -->
  html(
    span(class='plain-literal', [
      \rdf_simple_literal_html(Value),
      '@',
      \rdf_language_tag_html(Language)
    ])
  ).
rdf_plain_literal_html(literal(Value)) -->
  {atom(Value)},
  html(span(class='plain-literal', \rdf_simple_literal_html(Value))).

rdf_simple_literal_html(Value) -->
  html(span(class='simple-literal', ['"',Value,'"'])).

rdf_typed_literal_html(Graph, literal(type(Datatype,Value))) -->
  html(
    span(class='typed-literal', [
      '"',
      Value,
      '"^^',
      \rdf_iri_html(Graph, Datatype)
    ])
  ).



% IRI %

% E-mail.
rdf_iri_html(_, IRI1) -->
  {
    uri_components(IRI1, uri_components(Scheme, _, IRI2, _, _)),
    Scheme == mailto
  }, !,
  html(span(class='e-mail', a(href=IRI1, IRI2))).
% Image.
rdf_iri_html(_, IRI) -->
  {is_image_url(IRI)}, !,
  html(span(class='image', a(href=IRI, img(src=IRI, [])))).
% IRI.
rdf_iri_html(Graph, IRI1) -->
  {rdf_global_id(IRI2, IRI1)},
  (
    {IRI2 = Prefix:Postfix}
  ->
    {
      (
        xml_current_namespace(Prefix, _), !
      ;
        existence_error('XML namespace', Prefix)
      ),
      http_absolute_location(root(rdf_tabular), Location1, []),
      uri_query_add(Location1, term, IRI1, Location2),
      (
        var(Graph)
      ->
        Location3 = Location2
      ;
        uri_query_add(Location2, graph, Graph, Location3)
      )
    },
    html(
      span(class='iri', [
        a(href=Location3, [
          span(class=prefix, Prefix),
          ':',
          span(class=postfix, Postfix)
        ]),
        ' ',
        \external_link(IRI1)
      ])
    )
  ;
    {is_of_type(iri, IRI1)}
  ->
    html(span(class='iri', a(href=IRI1, IRI1)))
  ).

