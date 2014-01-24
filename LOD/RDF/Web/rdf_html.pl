:- encoding(utf8).
:- module(
  rdf_html,
  [
    rdf_html_table//2, % +Caption:atom
                       % +Data:list(list)
    rdf_html_term//1 % +PL_Term
  ]
).

/** <module> RDF HTML

HTML generation of RDF content.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(uri_ext)).
:- use_module(html(html_table)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_image)).
:- use_module(xml(xml_namespace)).



% TERM %

% @tbd HACK
% Graph.
rdf_html_term(Graph) -->
  {atom(Graph), rdf_graph(Graph)}, !,
  html(span(class='rdf-graph', Graph)).
% Blank node.
rdf_html_term(Type) -->
  rdf_blank_node(Type).
% Literal.
rdf_html_term(Type) -->
  rdf_literal(Type).
% IRI.
rdf_html_term(Type) -->
  rdf_iri(Type).
% Prolog term.
rdf_html_term(PL_Term) -->
  html(span(class='prolog-term', PL_Term)).



% BLANK NODE %

rdf_blank_node(BNode) -->
  {
    atom(BNode),
    atom_prefix(BNode, '__'), !,
    http_absolute_location(root(rdf_tabular), Location1, []),
    uri_query_add(Location1, term, BNode, Location2)
  },
  html(div(class='blank-node', a(href=Location2, BNode))).



% LITERAL %

rdf_language_tag(Language) -->
  html(span(class='language-tag', atom(Language))).

rdf_literal(Type) -->
  rdf_plain_literal(Type).
rdf_literal(Type) -->
  rdf_typed_literal(Type).

rdf_plain_literal(literal(lang(Language,Value))) -->
  html(
    span(class='plain-literal', [
      \rdf_simple_literal(Value),
      '@',
      \rdf_language_tag(Language)
    ])
  ).
rdf_plain_literal(literal(Value)) -->
  {atom(Value)},
  html(span(class='plain-literal', \rdf_simple_literal(Value))).

rdf_simple_literal(Value) -->
  html(span(class='simple-literal', ['"',Value,'"'])).

rdf_typed_literal(literal(type(Datatype,Value))) -->
  html(span(class='typed-literal', ['"',Value,'"^^',\rdf_iri(Datatype)])).



% IRI %

% E-mail.
rdf_iri(IRI1) -->
  {uri_components(IRI1, uri_components(mailto, _, IRI2, _, _))}, !,
  html(span(class='e-mail', a(href=IRI1, IRI2))).
% Image.
rdf_iri(IRI) -->
  {is_image_url(IRI)}, !,
  html(span(class='image', a(href=IRI, img(src=IRI, [])))).
% IRI.
rdf_iri(IRI1) -->
  {rdf_global_id(IRI2, IRI1)},
  (
    {IRI2 = Prefix:Postfix}
  ->
    {
      (
        xml_current_namespace(Prefix, _), !
      ;
        existence_error('XML namespace',Prefix)
      ),
      http_absolute_location(root(rdf_tabular), Location1, []),
      uri_query_add(Location1, term, IRI1, Location2)
    },
    html(
      span(class='iri', [
        a(href=Location2, [
          span(class=prefix, Prefix),
          ':',
          span(class=postfix, Postfix)
        ]),
        ' ',
        a(href=IRI1, '🐭')
      ])
    )
  ;
    {IRI1 = IRI2, atom(IRI1)},
    html(span(class='iri', a(href=IRI1, IRI1)))
  ).



% TABLE %

%! rdf_html_table(+Caption:atom, +Data:list(list))// .
% @param Data `P-O-G` or `S-P-O-G`.

rdf_html_table(_, []) --> !, [].
rdf_html_table(Caption, [H|T]) -->
  {
    O1 = [cell_dcg(rdf_html_term),header(true),indexed(true)],
    (
      var(Caption), !
    ;
      merge_options([caption(Caption)], O1, O2)
    ),
    same_length(H, H0),
    append(_, H0, ['Subject','Predicate','Object','Graph'])
  },
  html(\html_table(O2, [H0,H|T])).

