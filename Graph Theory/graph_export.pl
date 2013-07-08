:- module(
  graph_export,
  [
% EXPORTING
    export_graph/3, % +Options:list(nvpair)
                    % +Graph:graph
                    % -Export:element
    export_triples/3, % +Options:list(nvpair)
                      % +Triples:list(triple)
                      % -Export:element
    export_vertex/3, % +Options:list(nvpair)
                     % +Vertex:vertex
                     % -Export:element

% EXPORT TO DOM
    graph_to_dom/3, % +Options:list(nvpair)
                    % +Graph:graph
                    % -Stream:stream
    triples_to_dom/3, % +Options:list(nvpair)
                      % +Triples:list(triple)
                      % -Stream:stream
    vertex_to_dom/3 % +Options:list(nvpair)
                    % +Vertex:vertex
                    % -Stream:stream
  ]
).

/** <module> GRAPH_EXPORT

Generic graph export module.

# Graph input formats

  * Directed graphs (dgraphs)
  * Undirected graphs (ugraphs)
  * RDF graphs

From the input formats we require the following things:
    1. The list of vertices.
    2. The list of edges. We assume there a vertex that occurs in an edge
       also occurs in the list of vertices.
    3. A predicate for naming a vertex. This is typically very different
       for UGRAPHs and RDF graphs.
    4. A predicate for colorizing vertices. Whether an output format uses
       these colors is up to the output format.
    5. An argument that specifies the output format, currently =DOT=,
       =HTML_TABLE= or =SVG=.

# In-between graph format

We have to come up with a black box in between the input and output layers.



# Graph output formats

  * GraphViz
  * HTML table
  * SVG

@author Wouter Beek
@version 2012/12-2013/04
*/

:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(html(html)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).
:- use_module(os(file_ext)).
:- use_module(standards(graphviz)).
:- use_module(standards(markup)).
:- use_module(standards(graphviz)).
:- use_module(standards(standards)).
:- use_module(svg(svg)).

:- setting(
  default_vertex_radius,
  float,
  0.1,
  'The default radius of drawn vertices.'
).



% EXPORTING %

% BASIC PROCEDURES: CREATE (INPUT -> GRAPH) & WRITE (GRAPH -> OUTPUT) %

%! export_graph(+Options:list(nvpair), +Graph, -Export) is det.
% Creates a graph in the format specified using the =output_format= option.
%
% @arg Options A list of name-value pairs.
%        The following options are supported:
%        1. =|colorscheme(oneof([none,svg,x11]))|= The colorscheme for the
%           colors assigned to vertices and edges.
%           Supported for: GraphViz, HTML_TABLE.
%           Default: =svg=.
%        2. =|edge_labels(oneof([all,none,replace])|= Whether edge labels are
%           displayed, not displayed, or replaced by alternative labels.
%           Supported for: RDF, UGRAPH.
%           Default: =none=.
%        4. =language(Language:atom)= The atomic tag of the language that is
%           preferred for vertex naming.
%           Defaults to =en=.
%        5. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
%           Supported for: RDF.
%        7. =|rdf_list(onef([concise,full]))|= Whether vertices that are part
%           of an RDF list should be included or not.
%           Default: =full=.
%           Supported for: RDF.
%        8. =|vertex(oneof([rdf_node,rdf_term])|=
%           Value =rdf_node= means that only subject and object terms are
%           considered as vertices.
%           Value =rdf_term= means that subject, predicate and object terms
%           are considered as vertices.
%           Default: =rdf_node=.
%           Supported for: RDF.
%        9. =|vertex_coordinate(oneof([none,circular_vertice_coordinate,lookup_vertice_coordinate,random]))|=
%           The algorithm used for determining the vertex coordinates.
%           Default: =none=.
%           Supported for: SVG.

export_vertex(O1, G, N_G, V, G_Term):-
  select_option(depth(Depth), O1, O2, 1),
  merge_options([directed(false)], O2, O3),
  depth(O3, N_P, V, Depth, Vs, Es),
  G_Term = graph(Vs, [], Es, []).

create_edge(
  graphviz,
  Options,
  FromVertex0/FromVertexID0,
  ToVertex0/ToVertexID0,
  edge(FromVertexID0, ToVertexID0, Attributes)
):-
  (
    FromVertex = FromVertex0,
    ToVertex = ToVertex0,
    Direction = forward
  ;
    option(directed(false), Options, true),
    FromVertex = ToVertex0,
    ToVertex = FromVertex0,
    Direction = back
  ),

  % Label
  option(edge_labels(DisplayEdgeLabels), Options, none),
  (
    DisplayEdgeLabels == none
  ->
    Attributes0 = []
  ;
    edge_naming(Options, FromVertex-ToVertex, Label),
    Attributes0 = [label(Label)]
  ),

  % Style and arrow head.
  edge_styling(Options, FromVertex-ToVertex, Style/ArrowHead),

  append([arrowhead(ArrowHead), dir(Direction), style(Style)], Attributes0, Attributes),
  parse_attributes_graphviz(edge, Attributes).

create_graph(
  graphviz,
  Options,
  VertexElements,
  EdgeElements,
  graph(VertexElements, [], EdgeElements, GraphAttributes)
):-
  % Label
  graph_naming(Options, GraphLabel),

  option(colorscheme(Colorscheme), Options, svg),
  GraphAttributes =
    [
      charset('UTF-8'),
      colorscheme(Colorscheme),
      fontsize(11.0),
      label(GraphLabel),
      overlap(false)
    ],
  parse_attributes_graphviz(graph, GraphAttributes).

create_vertex(graphviz, Options, Vertex/ID, node(ID, Attributes)):-
  % Label
  vertex_naming(Options, Vertex, Name),

  % Color
  vertex_coloring(Options, Vertex, Color),

  % Shape
  vertex_shaping(Options, Vertex, ShapeAttributes),

  % Picture
  (
    vertex_picturing(Options, Vertex, Image)
  ->
    ImageAttributes = [image(Image)]
  ;
    ImageAttributes = []
  ),

  append(ShapeAttributes, ImageAttributes, OtherAttributes),
  Attributes = [color(Color), label(Name) | OtherAttributes],
  parse_attributes_graphviz(node, Attributes).
