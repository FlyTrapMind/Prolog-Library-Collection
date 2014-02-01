:- module(
  rdf_tabular,
  [
    rdf_tabular/2, % +Request:list
                   % :Content
    rdf_tabular_triples//4 % ?Subject:or([bnode,iri])
                           % ?Predicate:iri
                           % ?Object:or([bnode,iri,literal])
                           % ?Graph:atom
  ]
).

/** <module> RDF tabular

Generated RDF HTML tables.

@author Wouter Beek
@tbd Add blank node map.
@tbd Add namespace legend.
@tbd Add local/remote distinction.
@tbd Include images.
@version 2013/12-2014/02
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_tabular_graph)).
:- use_module(rdf_web(rdf_tabular_term)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).
:- use_module(tms(tms)).

http:location(rdf_tabular, root(rdf_tabular), []).
:- http_handler(root(rdf_tabular), rdf_tabular, [priority(-1)]).

:- initialization(web_module_add('RDF Tabular', rdf_tabular)).



% RDF term.
rdf_tabular(Request):-
  memberchk(search(Search), Request),
  memberchk(term=Term, Search), !,

  % The graph is optional (in which case it is left uninstantiated).
  ignore(memberchk(graph=Graph, Search)),

  reply_html_page(
    app_style,
    title(['Overview of resource ',Term]),
    \rdf_tabular_term(Graph, Term)
  ).

% RDF graph.
rdf_tabular(Request):-
  memberchk(search(Search), Request),
  memberchk(graph=Graph, Search), !,

  reply_html_page(
    app_style,
    title(['Overview of graph ',Graph]),
    \rdf_tabular_graph(Graph)
  ).

% Default: RDF graphs.
rdf_tabular(_Request):-
  rdf_tabular_graphs.


rdf_tabular_triples(S, P, O, Graph) -->
  {setoff(
    [S,P,O],
    rdf(S, P, O, Graph),
    Rows
  )},
  rdf_html_table(
    Graph,
    `RDF triples`,
    Rows
  ).

