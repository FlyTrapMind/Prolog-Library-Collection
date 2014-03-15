:- module(rdf_duplicates, []).

/** <module> RDF duplicates

Support for visualizing and managing duplicates in an RDF store.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(duplicates), rdf_duplicates, []).

:- web_module_add('RDF duplicates', rdf_duplicates).



rdf_duplicates(_Request):-
  reply_html_page(app_style, title('RDF duplicates'), html(\rdf_duplicates)).

rdf_duplicates -->
  {
    % Find all duplicate triples.
    setoff(
      [S,P,O],
      (
        rdf(S, P, O, G1),
        rdf(S, P, O, G2),
        G1 @< G2
      ),
      DuplicateTriples
    ),
    findall(
      [S,P,O,Gs2],
      (
        member([S,P,O], DuplicateTriples),
        setoff(G1, rdf(S, P, O, G1), Gs1),
        % Remove line numbers / indices
        % if a triple is not a duplicate within a graph.
        once(maplist(remove_graph_index(Gs1), Gs1, Gs2))
      ),
      Rows
    )
  },
  html(
    \rdf_html_table(
      [header_row(true)],
      html(p('Overview of RDF duplicates')),
      [['Subject','Predicate','Object','Graphs']|Rows]
    )
  ).


remove_graph_index(Graphs, Graph:Index1, Graph):-
  \+ ((
    member(Graph:Index2, Graphs),
    Index1 \= Index2
  )).
remove_graph_index(_, Graph, Graph).

