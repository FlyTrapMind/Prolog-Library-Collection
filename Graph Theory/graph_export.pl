:- module(
  graph_export,
  [
    export_graph/3, % +Options:list(nvpair)
                    % +Graph:oneof([dgraph,rdf_graph,ugraph])
                    % -GraphTerm:compound
    export_graph/4, % +Options:list(nvpair)
                    % :CoordFunc
                    % +Graph:oneof([dgraph,rdf_graph,ugraph])
                    % -GraphTerm:compound
    export_vertex/4, % +Options:list(nvpair)
                     % :N_P
                     % +Vertex
                     % -GraphTerm:compound
    shared_attributes/3 % +Terms:list(compound)
                        % -SharedAttributes:list(nvpair)
                        % -NewTerms:list(compound)
  ]
).

/** <module> GRAPH_EXPORT

Generic graph export module.

# Options

The following options are used for exporting graphs:
  1. `colorscheme(+ColorScheme:oneof([none,svg,x11]))`
     The colorscheme for the colors assigned to vertices and edges.
     Default: `svg`.
     Supported for: GraphViz, HTML_TABLE.
  2. `edge_labels(oneof([all,none,replace])`
     Whether edge labels are displayed, not displayed, or
     replaced by alternative labels.
     Default: `none`.
     Supported for: RDF, UGRAPH.
  4. `language(+Language:atom)`
     The atomic tag of the language that is preferred for vertex naming.
     Default: `en`.
  5. `literals(oneof([collapse,hide,labels_only,show]))`
     Whether or not literals are allowed as vertices in the edge.
     Default: `collapse`.
     Supported for: RDF.
  7. `rdf_list(onef([concise,full]))`
     Whether vertices that are part of an RDF list should be included or not.
     Default: `full`.
     Supported for: RDF.
  8. `vertex(oneof([rdf_node,rdf_term])`
     `rdf_node` means that only subject and object terms are
     considered as vertices.
     `rdf_term` means that subject, predicate and object terms
     are considered as vertices.
     Default: `rdf_node`.
     Supported for: RDF.
  9. `vertex_coordinate(oneof([none,circular_vertice_coordinate,lookup_vertice_coordinate,random]))`
     The algorithm used for determining the vertex coordinates.
     Default: `none`.
     Supported for: SVG.

# Graph Interchange Format

Better known as the GIF format :-).

~~~{.pl}
graph(VertexTerms, EdgeTerms, GraphAttributes)
~~~

~~~{.pl}
vertex(VertexId, Vertex, VertexAttributes)
~~~

~~~{.pl}
edge(FromVertexId, ToVertexId, EdgeAttributes)
~~~

@author Wouter Beek
@version 2012/12-2013/04, 2013/07
*/

:- use_module(generics(option_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(ugraph(ugraph_ext)).
:- use_module(ugraph(ugraph_export)).

:- meta_predicate(export_graph(+,4,+,-)).
:- meta_predicate(export_vertex(+,3,+,-)).

:- rdf_meta(export_vertex(+,:,r,-)).



export_graph(O, G, G_Term):-
  export_graph(O, random_vertex_coordinate, G, G_Term).

%export_graph(O, CoordFunc, G, G_Term):-
%  is_dgraph(G),
%  export_dgraph(O, CoordFunc, G, G_Term).
export_graph(O, CoordFunc, G, G_Term):-
  atomic(G), rdf_graph(G), !,
  export_rdf_graph(O, CoordFunc, G, G_Term).
export_graph(O, CoordFunc, G, G_Term):-
  is_ugraph(G),
  export_ugraph(O, CoordFunc, G, G_Term).

%! export_vertex(
%!   +Options:list(nvpair),
%!   :N_P,
%!   +Vertex,
%!   -GraphTerm:compound
%! ) is det.

export_vertex(O1, N_P, V, G_Term):-
  select_option(depth(Depth), O1, O2, 1),
  merge_options([directed(false)], O2, O3),
  depth(O3, N_P, V, Depth, Vs, Es),
  export_graph(O1, ugraph(Vs, Es), G_Term).

remove_attribute(Attrs, T, NewT):-
  T =.. L,
  append(L1, [T_Attrs], L),
  subtract_option(T_Attrs, Attrs, NewT_Attrs),
  append(L1, [NewT_Attrs], NewL),
  NewT =.. NewL.

shared_attribute([T1|Ts], N=V):-
  T1 =.. L1,
  last(L1, Attrs1),
  member(Attr1, Attrs1),
  option_dep(Attr1, N=V),
  forall(
    member(T2, Ts),
    (
      T2 =.. L2,
      last(L2, Attrs2),
      member(Attr2, Attrs2),
      option_dep(Attr2, N=V)
    )
  ).

shared_attributes(Terms, SharedAttrs, NewTerms):-
  findall(
    SharedAttr,
    shared_attribute(Terms, SharedAttr),
    SharedAttrs
  ),
  maplist(remove_attribute(SharedAttrs), Terms, NewTerms).
