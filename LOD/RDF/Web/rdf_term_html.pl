:- module(
  rdf_term_html,
  [
    rdf_graph_html//1, % +RdfGraph:atom
    rdf_literal_html//4, % +LexicalForm:atom
                         % +DatatypeIri:iri
                         % +LanguageTag:atom
                         % +RdfGraph:atom
    rdf_term_html//1, % +RdfTerm:compound
    rdf_term_html//2 % +RdfGraph:atom
                     % +RdfTerm:compound
  ]
).

/** <module> RDF term HTML

HTML generation for RDF terms.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(dcg(dcg_collection)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(html(html_list)).
:- use_module(html(pl_term_html)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(server(web_ui)).
:- use_module(xml(xml_namespace)).



% TERM %

rdf_term_html(RdfTerm) -->
  rdf_term_html(_, RdfTerm).

% RDF graphs.
rdf_term_html(_, Graphs) -->
  {maplist(rdf_graph, Graphs)}, !,
  html(\html_list([ordered(false)], rdf_term_html, Graphs)).
% RDF graph.
rdf_term_html(_, Graph1) -->
  {rdf_is_graph(Graph1, Graph2)}, !,
  rdf_graph_html(Graph2).
rdf_term_html(_, RdfTerm) -->
  {
    rdf_is_resource(RdfTerm),
    rdfs_individual_of(RdfTerm, rdf:'List')
  }, !,
  rdf_list_html(RdfTerm).
% Blank node.
rdf_term_html(_, RdfTerm) -->
  {rdf_is_bnode(RdfTerm)}, !,
  rdf_blank_node_html(RdfTerm).
% Literal.
rdf_term_html(Graph, RdfTerm) -->
  {rdf_literal(RdfTerm, LexicalForm, Datatype, LangTag)}, !,
  rdf_literal_html(LexicalForm, Datatype, LangTag, Graph).
% IRI.
rdf_term_html(Graph, RdfTerm) -->
  rdf_iri_html(RdfTerm, Graph).
% Prolog term.
rdf_term_html(_, PlTerm) -->
  html(span(class='prolog-term', \pl_term_html(PlTerm))).



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
  html(span(class='blank-node', a(href=Location2, BNode))).



% LITERAL %

rdf_language_tag_html(LangTag) -->
  html(span(class='language-tag', atom(LangTag))).

xsd_lexical_form(LexicalForm) -->
  html(span(class='rdf-lexical-form', LexicalForm)).

% Simple literal.
rdf_literal_html(LexicalForm, Datatype, LangTag, Graph) -->
  {var(Datatype)}, !,
  rdf_literal_html(LexicalForm, xsd:string, LangTag, Graph).
% Language-tagged string.
rdf_literal_html(LexicalForm, rdf:langString, LangTag, _) -->
  {nonvar(LangTag)}, !,
  html(
    span(class='language-tagged-string', [
      \xsd_lexical_form(LexicalForm),
      '@',
      \rdf_language_tag_html(LangTag)
    ])
  ).
% XSD datatypes.
rdf_literal_html(LexicalForm, Datatype, _, Graph) -->
  html(
    span(class='rdf-literal', [
      '"',
      \xsd_lexical_form(LexicalForm),
      '"^^',
      \rdf_iri_html(Datatype, Graph)
    ])
  ).



% IRI %

% E-mail.
rdf_iri_html(IRI1, _) -->
  {
    uri_components(IRI1, uri_components(Scheme, _, IRI2, _, _)),
    Scheme == mailto
  }, !,
  html(span(class='e-mail', a(href=IRI1, IRI2))).
% Image.
rdf_iri_html(IRI, _) -->
  {is_image_url(IRI)}, !,
  html(span(class='image', a(href=IRI, img(src=IRI, [])))).
% IRI.
rdf_iri_html(IRI1, Graph) -->
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

