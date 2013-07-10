:- module(
  rdf_export,
  [
% COLORS
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_edge_style/2, % +RDF_Term
                               % -EdgeStyle:atom
    rdf_register_namespace_color/3, % +Graph:graph
                                    % +Namespace:atom
                                    % +Color:atom

% GRAPH EXPORT
    export_rdf_graph/4 % +Options:list(nvpair)
                       % :CoordFunc
                       % +RDF_Graph:atom
                       % -GraphTerm:compound
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
    1. Look whether the =colorscheme= option is not set to =none=.
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
:- use_module(svg(svg_colors)).

:- dynamic(class_color(_G, _Class, _Color)).
:- dynamic(namespace_color(_G, _Namespace, _Color)).
:- dynamic(rdf_edge_style(_RDF_Term, _EdgeStyle)).

:- meta_predicate(export_rdf_graph(+,4,+,-)).
:- meta_predicate(rdf_vertex_term(+,+,+,4,+,-)).

% COLOR
:- rdf_meta(rdf_register_class_color(+,r,+)).
:- rdf_meta(rdf_vertex_color_by_namespace(+,+,r,-)).
% EDGES
:- rdf_meta(rdf_edge_arrow_head(r,-)).
:- rdf_meta(rdf_edge_name(r,-)).
:- rdf_meta(rdf_edge_style(r,?)).
% VERTICES
:- rdf_meta(rdf_vertex_color(+,+,r,-)).
:- rdf_meta(rdf_vertex_peripheries(r,-)).
:- rdf_meta(rdf_vertex_picture(+,r,-)).
:- rdf_meta(rdf_vertex_shape(r,-)).
:- rdf_meta(rdf_vertex_term(+,+,+,:,r,-)).



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
  db_replace_novel(class_color(G, Class, ClassColor)).

rdf_register_edge_style(RDF_Term, EdgeStyle):-
  db_replace_novel(rdf_edge_style(RDF_Term, EdgeStyle)).

rdf_register_namespace_color(G, Namespace, NamespaceColor):-
  db_replace_novel(namespace_color(G, Namespace, NamespaceColor)).

%! rdf_vertex_color_by_namespace(
%!   +Graph:atom,
%!   +ColorScheme:atom,
%!   +Vertex,
%!   -VertexColor:atom
%! ) is det.
% Returns the automatically assigned color name.
% The color names belong to the given colorscheme.
% The color assignments are based on the RDF node's namespace.
% Note that the same node may have different colors in different graphs.
%
% @arg Graph The atomic name of a graph.
% @arg ColorScheme The atomic name of a colorscheme. Currently supported:
%      1. `svg`
%      2. `x11`
% @arg Vertex A resource.
% @arg VertexColor The atomic name of a color within the colorscheme.

rdf_vertex_color_by_namespace(G, _ColorScheme, V, V_Color):-
  rdf_global_id(Namespace:_, V),
  namespace_color(G, Namespace, V_Color), !.
rdf_vertex_color_by_namespace(G, ColorScheme, V, V_Color):-
  rdf_colorize_namespaces(G, ColorScheme),
  rdf_vertex_color_by_namespace(G, ColorScheme, V, V_Color).



% GRAPH EXPORT %

export_rdf_graph(O, CoordFunc, G, graph(V_Terms, E_Terms, G_Attrs)):-
  % Vertices
  rdf_vertices(G, Vs),
  maplist(rdf_vertex_term(O, G, Vs, CoordFunc), Vs, V_Terms),

  % Edges
  rdf_edges(G, Es),
  maplist(rdf_edge_term(O, G, Vs), Es, E_Terms),

  % Graph
  rdf_graph_name(G, G_Name),
  G_Attrs = [label(G_Name)].

%! rdf_graph_name(+Graph:rdf_graph, -GraphName:atom) is det.
% Returns a name for the given graph.

rdf_graph_name(G, G).



% EDGE EXPORT %

%! rdf_edge_arrow_head(+Edge:edge, -E_ArrowHead:atom) is det.

rdf_edge_arrow_head(_FromV-P-_ToV, E_ArrowHead):-
  once(rdf_edge_arrow_head(P, E_ArrowHead)).

rdf_edge_arrow_head(rdf:type,           empty  ).
rdf_edge_arrow_head(rdfs:label,         none   ).
rdf_edge_arrow_head(rdfs:subClassOf,    box    ).
rdf_edge_arrow_head(rdfs:subPropertyOf, diamond).
rdf_edge_arrow_head(_RDF_Property,      normal ).

rdf_edge_color(O, _G, _E, black):-
  option(colorscheme(none), O, none), !.
rdf_edge_color(O, G, FromV-_P-ToV, E_Color):-
  rdf_vertex_color(O, G, FromV, FromV_Color),
  rdf_vertex_color(O, G, ToV, ToV_Color), !,
  % Notice that color can be uninstantiated in rdf_vertex_color/4?
  FromV_Color = ToV_Color,
  E_Color = FromV_Color.
rdf_edge_color(O, G, _FromV-P-_ToV, E_Color):-
  rdf_vertex_color(O, G, P, E_Color).

%! rdf_edge_name(+Options:list(nvpair), +Edge:edge, -EdgeName:atom) is det.
% Returns a name for the given edge.
%
% @arg Options The following options are supported:
%      1. `edge_labels(oneof([all,replace]))`

rdf_edge_name(O, _FromV-P-_ToV, E_Name):-
  % Some edge labels are not displayed.
  % This concerns RDF(S) terminology.
  (
    % Make use of explicit replacements.
    option(edge_labels(replace), O),
    % Apply the explicit replacement.
    rdf_edge_name(P, E_Name)
  ->
    true
  ;
    % The edge name is the predicate RDF term.
    rdf_term_name(O, P, E_Name)
  ).

rdf_edge_name(rdf:type,           '').
rdf_edge_name(rdfs:label,         '').
rdf_edge_name(rdfs:subClassOf,    '').
rdf_edge_name(rdfs:subPropertyOf, '').

%! rdf_edge_style(+Edge:edge, -E_Style:atom) is det.

rdf_edge_style(_FromV-P-_ToV, E_Style):-
  once(rdf_edge_style(P, E_Style)).

rdf_edge_style(rdf:type,           solid ).
rdf_edge_style(rdfs:label,         dotted).
rdf_edge_style(rdfs:subClassOf,    solid ).
rdf_edge_style(rdfs:subPropertyOf, solid ).
rdf_edge_style(_RDF_Property,      solid ).

rdf_edge_term(O, G, Vs, E, edge(FromV_Id, ToV_Id, E_Attrs)):-
  % Ids.
  E = FromV-_P-ToV,
  nth0(FromV_Id, Vs, FromV),
  nth0(ToV_Id, Vs, ToV),

  % Arrow head.
  rdf_edge_arrow_head(E, E_ArrowHead),

  % Color.
  rdf_edge_color(O, G, E, E_Color),

  % Label.
  rdf_edge_name(O, E, E_Name),

  % Style.
  rdf_edge_style(E, E_Style),

  E_Attrs = [
    arrow_type(E_ArrowHead),
    color(E_Color),
    label(E_Name),
    style(E_Style)
  ].



% VERTEX EXPORT %

%! rdf_vertex_color(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +Vertex:vertex,
%!   -Color:atom
%! ) is det.
% Returns a color name for the given vertex.
%
% @arg Options A list of name-value pairs.
%        1. =colorscheme(ColorScheme:oneof([none,svg,x11]))= The atomic name
%           of the color scheme from which the color names are drawn.
%        2. =graph(Graph:atom)= The atomic name of a graph.
% @arg Vertex A vertex.
% @arg Color The atomic name of a color for the given vertex.

rdf_vertex_color(O, _G, _V, black):-
  option(colorscheme(none), O), !.
% Literals.
rdf_vertex_color(_O, _G, literal(_Value), blue):- !.
rdf_vertex_color(_O, _G, literal(lang(_Lang, _Value)), blue):- !.
rdf_vertex_color(_O, _G, literal(type(_Datatype, _LexicalValue)), blue):- !.
% Individual or subclass of a color-registered class.
rdf_vertex_color(_O, G, V, V_Color):-
  (
    rdfs_individual_of(V, Class)
  ;
    rdfs_subclass_of(V, Class)
  ),
  class_color(G, Class, V_Color), !.
% Resource colored based on its namespace.
rdf_vertex_color(O, G, V, V_Color):-
  option(colorscheme(ColorScheme), O, svg),
  (
    % URI resources with registered namespace/prefix.
    rdf_global_id(_:_, V),
    rdf_vertex_color_by_namespace(G, ColorScheme, V, V_NamespaceColor)
  ->
     V_Color = V_NamespaceColor
  ;
    % URI resources with unregistered namespace/prefix.
    is_uri(V)
  ->
    V_Color = red
  ;
    % Non-URI resources, probably blank nodes.
    V_Color = purple
  ).

%! rdf_vertex_peripheries(
%!   +RDF_Term:oneof([bnode,literal,uri]),
%!   -Peripheries:integer
%!) is det.

% RDF literals.
rdf_vertex_peripheries(literal(_Literal), 0):- !.
% RDFS classes.
rdf_vertex_peripheries(RDF_Term, 2):-
  rdfs_individual_of(RDF_Term, rdfs:'Class'), !.
% RDF properties.
rdf_vertex_peripheries(RDF_Term, 1):-
  rdfs_individual_of(RDF_Term, rdf:'Property'), !.
% RDFS resources that are not RDF properties or RDFS classes.
rdf_vertex_peripheries(RDF_Term, 1):-
  rdfs_individual_of(RDF_Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_peripheries(RDF_Term, 1):-
  rdf_is_bnode(RDF_Term), !.
% Catch-all.
rdf_vertex_peripheries(_RDF_Term, 1).

rdf_vertex_picture(G, V, V_Picture):-
  % Only display the first picture that is related to this vertex.
  once(rdf_datatype(V, _P, image, V_Picture, G)).

%! rdf_vertex_shape(+RDF_Term:oneof([bnode,literal,uri]), -Shape:atom) is det.
% @arg Shape The atomic name of a vertex shape.
%      The following shapes are supported:
%      * `circle`
%      * `ellipse`
%      * `hexagon`
%      * `octagon`
%      * `plaintext`

% RDF literals.
rdf_vertex_shape(literal(_Literal), plaintext):- !.
% RDFS class.
rdf_vertex_shape(RDF_Term, octagon):-
  rdfs_individual_of(RDF_Term, rdfs:'Class'), !.
% RDF property.
rdf_vertex_shape(RDF_Term, hexagon):-
  rdfs_individual_of(RDF_Term, rdf:'Property'), !.
% RDFS resources that are not RDF properties or RDFS classes.
rdf_vertex_shape(RDF_Term, ellipse):-
  rdfs_individual_of(RDF_Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_shape(RDF_Term, circle):-
  rdf_is_bnode(RDF_Term), !.
% Catch-all.
rdf_vertex_shape(_RDF_Term, ellipse).

rdf_vertex_term(O, G, Vs, CoordFunc, V, vertex(V_Id, V, V_Attrs2)):-
  nth0(V_Id, Vs, V),
  rdf_term_name(O, V, V_Name),
  rdf_vertex_color(O, G, V, V_Color),
  call(CoordFunc, O, Vs, V, V_Coord),
  rdf_vertex_peripheries(V, V_Peripheries),
  rdf_vertex_shape(V, V_Shape),
  V_Attrs1 = [
    color(V_Color),
    coord(V_Coord),
    label(V_Name),
    peripheries(V_Peripheries),
    shape(V_Shape)
  ],
  (
    rdf_vertex_picture(G, V, V_Picture)
  ->
    merge_options([image(V_Picture)], V_Attrs1, V_Attrs2)
  ;
    V_Attrs2 = V_Attrs1
  ).

