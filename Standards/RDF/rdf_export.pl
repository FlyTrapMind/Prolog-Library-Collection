:- module(
  rdf_export,
  [
    export_rdf_graph/4, % +Options:list(nvpair)
                        % :CoordFunc
                        % +RDF_Graph:atom
                        % -GraphTerm:compound
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_namespace_color/3, % +Graph:graph
                                    % +Namespace:atom
                                    % +Color:atom
    rdf_schema/2, % +Graph:atom
                  % -Triples:list(rdf_triple)
    rdf_vertex_name/3 % +Options:list(nvpair)
                        % +RDF_Term:oneof([bnode,literal,uri])
                        % -Name:atom
  ]
).

/** <module> RDF export

Predicates for exporting RDF.

# Edge naming

Specific edge labels can be hidden by adding clauses to
rdf_edge_label_replace/2.

# Edge styling

# Vertex coloring

The procedure for determining the color of a vertex:
    1. Look whether the =color_scheme= option is not set to =none=.
    2. See whether the vertex is an individual of a colored class.
    3. See whether the vertex belongs to a colored namespace.
    4. If at least one vertex is not colored by class or namespace, then all
       namespaces are (re)assigned colors.

# Vertex naming

@author Wouter Beek
@version 2013/01-2013/03, 2013/07
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(svg(svg)).

:- dynamic(class_color(_Graph, _Class, _Color)).
:- dynamic(namespace_color(_Graph, _Namespace, _Color)).

:- rdf_meta(rdf_edge_label_replace(r,r)).
:- rdf_meta(rdf_edge_style(r,?)).
:- rdf_meta(rdf_register_class_color(+,r,+)).



% COLOR %

%! rdf_colorize_namespaces(+Graph:atom, +ColorScheme:atom) is det.
% Uses colors from the given color scheme to colorize the namespaces in the
% given graph.
%
% @tbd Throw exception for unknown color scheme.

rdf_colorize_namespaces(G, _ColorScheme):-
  \+ rdf_graph(G), !,
  existence_error(atom, G).
rdf_colorize_namespaces(G, svg):- !,
  rdf_current_namespaces(G, Namespaces),
  length(Namespaces, NumberOfNamespaces),
  NumberOfNamespaces > 0,
  svg_colors(Colors),
  length(Colors, NumberOfColors),
  Delta is NumberOfColors // NumberOfNamespaces,
  forall(
    nth1(I, Namespaces, Namespace),
    (
      % In case there are more namespaces than colors, Delta=1 and we use
      % the same color for all namespaces with index I mod M.
      J is (I * Delta) mod NumberOfColors,
      % J can be 0 becasue of the modulus function, so do not use nth1/3.
      nth0(J, Colors, Color),
      assert(namespace_color(G, Namespace, Color))
    )
  ).
rdf_colorize_namespaces(_G, ColorScheme):-
  existence_error(atom, ColorScheme).

rdf_register_class_color(G, Class, ClassColor):-
  db_add_novel(class_color(G, Class, ClassColor)).

rdf_register_namespace_color(G, Namespace, NamespaceColor):-
  db_add_novel(namespace_color(G, Namespace, NamespaceColor)).

%! rdf_vertex_color_by_namespace(
%!   +Graph:atom,
%!   +ColorScheme:atom,
%!   +Vertex,
%!   -Color:atom
%! ) is det.
% Returns the automatically assigned color name.
% The color names belong to the given color_scheme.
% The color assignments are based on the RDF node's namespace.
% Note that the same node may have different colors in different graphs.
%
% @arg Graph The atomic name of a graph.
% @arg ColorScheme The atomic name of a color_scheme. Currently supported:
%      1. `svg`
%      2. `x11`
% @arg Vertex A resource.
% @arg Color The atomic name of a color within the color_scheme.

rdf_vertex_color_by_namespace(Graph, _ColorScheme, Vertex, Color):-
  rdf_global_id(Namespace:_, Vertex),
  namespace_color(Graph, Namespace, Color), !.
rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color):-
  rdf_colorize_namespaces(Graph, ColorScheme),
  rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color).



% GRAPH %

export_rdf_graph(O, CoordFunc, G, graph(V_Terms, [], E_Terms, G_Attrs)):-
  % Vertices
  rdf_vertices(G, Vs),
  maplist(rdf_vertex_term(O), Vs, V_Terms),
  
  % Edges
  rdf_edges(G, Es),
  maplist(rdf_edge_term(O), Es, E_Terms),
  
  % Graph
  rdf_graph_name(G_Name),
  G_Attrs = [label(G_Name)].

%! rdf_graph_name(+Graph:rdf_graph, -GraphName:atom) is det.
% Returns a name for the given graph.

rdf_graph_name(G, G).



% EDGES %

rdf_edge_term(O, E, edge(E_Id, E_Attrs)):-
  rdf_edge_arrow_head(O, G, E, E_ArrowHead),
  rdf_edge_color(O, G, E, E_Color),
  rdf_edge_name(O, G, E, E_Name),
  rdf_edge_style(O, G, E, E_Style),
  E_Attrs = [
    arrow_type(E_ArrowHead),
    color(E_Color),
    label(E_Name),
    style(E_Style)
  ].

%! rdf_edge_arrow_head(
%!   +Options:list(nvpair),
%!   +Graph:rdf_graph,
%!   +Edge:edge,
%!   -E_ArrowHead:atom
%! ) is det.

rdf_edge_arrow_head(O, G, FromV-ToV, E_ArrowHead):-
  rdf(FromV, P, ToV, G),
  once(rdf_edge_arrow_head(P, E_ArrowHead)).

rdf_edge_arrow_head(rdf:type,           empty  ).
rdf_edge_arrow_head(rdfs:label,         none   ).
rdf_edge_arrow_head(rdfs:subClassOf,    box    ).
rdf_edge_arrow_head(rdfs:subPropertyOf, diamond).
rdf_edge_arrow_head(_RDF_Property,      normal ).

rdf_edge_color(O, _G, _E, black):-
  option(color_scheme(none), O, none), !.
rdf_edge_color(O, _G, FromV-ToV, E_Color):-
  rdf_vertex_color(O, FromV, FromV_Color),
  rdf_vertex_color(O, ToV, ToV_Color), !,
  % Notice that color can be uninstantiated in rdf_vertex_color/3?
  FromColor = ToColor,
  Color = FromColor.
rdf_edge_color(O, G, FromV-ToV, E_Color):-
  rdf(FromV, P, ToV, G),
  rdf_vertex_color(O, P, E_Color).

%! rdf_edge_name(
%!   +Options:list(nvpair),
%!   +Graph:rdf_graph,
%!   +Edge:edge,
%!   -EdgeName:atom
%! ) is det.
% Returns a name for the given edge.
%
% @arg Options The following options are supported:
%      1. `edge_labels(oneof([all,replace]))`

rdf_edge_name(O, G, FromV-ToV, E_Name):-
  rdf(FromV, P, ToV, G),
  % Some edge labels are not displayed.
  % This concerns RDF(S) terminology.
  (
    % Make use of explicit replacements.
    option(edge_labels(replace), O),
    % Apply the explicit replacement.
    rdf_edge_label_replace(P, E_Name)
  ->
    true
  ;
    % The edge name is the predicate RDF term.
    rdf_vertex_name(O, P, E_Name)
  ).

rdf_edge_label_replace(rdf:type,           '').
rdf_edge_label_replace(rdfs:label,         '').
rdf_edge_label_replace(rdfs:subClassOf,    '').
rdf_edge_label_replace(rdfs:subPropertyOf, '').

%! rdf_edge_style(
%!   +Options:list(nvpair),
%!   +Graph:rdf_graph,
%!   +Edge:edge,
%!   -E_Style:atom
%! ) is det.

rdf_edge_style(O, G, FromV-ToV, E_Style):-
  rdf(FromV, P, ToV, G),
  once(rdf_edge_style(P, E_Style)).

rdf_edge_style(rdf:type,           solid ).
rdf_edge_style(rdfs:label,         dotted).
rdf_edge_style(rdfs:subClassOf,    solid ).
rdf_edge_style(rdfs:subPropertyOf, solid ).
rdf_edge_style(_RDF_Property,      solid ).



% VERTICES %

rdf_vertex_term(O, G, V, vertex(V_Id, V_Attrs)):-
  rdf_vertex_name(V, V_Name),
  rdf_vertex_color(O, G, V, V_Color),
  V_Attrs = [color(V_Color),label(V_Name)].

%! rdf_vertex_color(
%!   +Options:list(nvpair),
%!   +Vertex:vertex,
%!   -Color:atom
%! ) is det.
% Returns a color name for the given vertex.
%
% @arg Options A list of name-value pairs.
%        1. =color_scheme(ColorScheme:oneof([none,svg,x11]))= The atomic name
%           of the color scheme from which the color names are drawn.
%        2. =graph(Graph:atom)= The atomic name of a graph.
% @arg Vertex A vertex.
% @arg Color The atomic name of a color for the given vertex.

rdf_vertex_color(O, _G, _V, black):-
  option(color_scheme(none), Options), !.
% Literals.
rdf_vertex_color(_O, _G, literal(_Value), blue):- !.
rdf_vertex_color(_O, _G, literal(lang(_Lang, _Value)), blue):- !.
rdf_vertex_color(_O, _G, literal(type(_Datatype, _LexicalValue)), blue):- !.
% Individual or subclass of a color-registered class.
rdf_vertex_color(O, G, V, V_Color):-
  (
    rdfs_individual_of(V, Class)
  ;
    rdfs_subclass_of(V, Class)
  ),
  class_color(G, Class, V_Color), !.
% Resource colored based on its namespace.
rdf_vertex_color(O, G, V, V_Color):-
  option(color_scheme(ColorScheme), O, svg),
  (
    % URI resources with registered namespace/prefix.
    rdf_global_id(_:_, V)
  ->
    rdf_vertex_color_by_namespace(G, ColorScheme, V, V_Color)
  ;
    % URI resources with unregistered namespace/prefix.
    is_uri(V)
  ->
    V_Color = red
  ;
    % Non-URI resources, probably blank nodes.
    V_Color = purple
  ).

rdf_vertex_picture(G, V, V_Picture):-
  % Only display the first picutre that is related to this vertex.
  rdf_datatype(V, _P, image, V_Picture, G).

% Non-resource vertex (e.g. literals).
rdf_vertex_shaping(literal(_), [peripheries(0), shape(plaintext)]):- !.
% Class vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(2), shape(octagon)]):-
  rdfs_individual_of(RDF_Term, rdfs:'Class'), !.
% Property vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(hexagon)]):-
  rdfs_individual_of(RDF_Term, rdf:'Property'), !.
% Non-class and non-property resource vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(ellipse)]):-
  rdfs_individual_of(RDF_Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(circle)]):-
  rdf_is_bnode(RDF_Term), !.
% Catch-all.
rdf_vertex_shaping(_RDF_Term, [peripheries(1), shape(ellipse)]):.

