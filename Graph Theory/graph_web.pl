:- module(
  graph_web,
  [
    circle_graph_web/2, % +Graph:graph
                        % -Markup:list
    graph_web/2, % +Graph:graph
                 % -Markup:list
    harary_web/3, % +K:integer
                  % +N:integer
                  % -Markup:list
    random_graph_web/2, % +Graph:graph
                        % -Markup:list
    spring_embedding_web/2, % +Graph:graph
                            % -Markup:list
    spring_embedding_web/3, % +Graph:graph
                            % +Iterations:integer
                            % -Markup:list
    table_graph_web/2, % +Graph:atom
                       % -Markup:element
    vertex_web/3 % +Graph:atom
                 % +Vertex:atom
                 % -Markup:list
  ]
).

/** <module> Graph Web

Web front-end for generic graph visualizations.

@author Wouter Beek
@tbd All these methods should become graph datatype-independent.
@version 2013/01-2013/02
*/

:- use_module(generics(atom_ext)).
:- use_module(graph_theory(circular_graph_representation)).
:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_export_svg)).
:- use_module(graph_theory(graph_generic)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(graph_theory(spring_embedding)).
:- use_module(graph_theory(vertex_coordinate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_graph(rdf_graph_theory)).
:- use_module(server(web_error)).
:- use_module(ugraph(ugraph_ext)).



%! circle_graph_web(+Graph:graph, -Markup:list) is det.

circle_graph_web(Graph, html/app_style/[GraphElement | TableMarkup]):-
  export_graph(
    [edge_labels(all)],
    circular_vertex_coordinate,
    Graph,
    GraphElement
  ),
  table_graph_web(Graph, _DTD_Name/_StyleName/TableMarkup).

%! graph_web(+Graph:graph, -Markup:list) is det.
% Writes the graph with the given name to a file in GraphViz DOT format.
%
% @arg Graph
% @arg Markup

graph_web(G, svg11/app_style/SVG):-
  export_graph_svg(G, SVG).

%! harary_web(+K:integer, +N:integer, -Markup:list) is det.
% Returns markup represening the K-connected Harary graph with N vertices.

harary_web(K, N, Markup):-
  ugraph_harary(K, N, Harary),
  circle_graph_web(Harary, Markup).

%! random_graph_web(+Graph:graph, -Markup:list) is det.
% Returns an Web representation of the RDF graph with the given title.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.
%
% @arg Graph
% @arg Markup

random_graph_web(G, [GraphElement,TableElement]):-
  random_vertex_coordinate([], G, rdf_vertices, VCoords),
  vertice_coordinates_web(G, VCoords, GraphElement),
  vertice_coordinates_table(VCoords, TableElement).

%! spring_embedding_web(+Graph:graph, -Markup:list) is det.
% @see spring_embedding_web/3

spring_embedding_web(Graph, Markup):-
  spring_embedding_web(Graph, 50, Markup).

%! spring_embedding_web(
%!   +Graph:graph,
%!   +Iterations:integer,
%!   -Markup:list
%! ) is det.
% Returns the markup for the spring embedding of the given graph.
%
% @arg Graph
% @arg Iterations An integer, representing the number of iterations
%        of spring embedding, i.e., the number of subsequent function
%        applications.
% @arg Markup A list of markup elements.

spring_embedding_web(Graph, Iterations, Markup):-
  default_spring_embedding(
    Graph,
    ugraph_vertices,
    ugraph_edges,
    ugraph_neighbors,
    Iterations,
    _FinalVerticeCoordinates,
    History
  ),
  findall(
    MarkupElement,
    (
      member(VerticeCoordinates, History),
      vertice_coordinates_web(Graph, VerticeCoordinates, MarkupElement)
    ),
    Markup
  ).

%! table_graph_web(+Graph:graph, -Markup:triple) is det.

table_graph_web(Graph, html/app_style/[TableElement]):-
  export_graph(
    [
      colorscheme(none),
      edge_labels(all),
      in(rdf),
      literals(collapse),
      out(html_table)
    ],
    Graph,
    TableElement
  ).

%! vertex_web(+Graph:atom, +Vertex:vertex, -Markup:dom) is det.
% Creates a DOM representation for the given vertex in the given graph.
%
% @arg Graph The atomic name of an RDF graph.
% @arg Vertex
% @arg Markup

vertex_web(G, V1, SVG):-
  term_to_atom(V1, V2),
  rdf_global_id(G:V2, V),
  export_vertex(
    [edge_labels(all),literals(preferred_label)],
    rdf_neighbors,
    V,
    G_Term
  ),
  export_graph_svg(G_Term, SVG).

