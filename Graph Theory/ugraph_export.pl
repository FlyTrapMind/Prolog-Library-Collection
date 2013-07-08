:- module(
  ugraph_export,
  [
    export_ugraph/4 % +Options
                    % +CoordFunc
                    % +UG:ugraph
                    % -G_Term:compound
  ]
).

/** <module> UGRAPH_EXPORT

Predicates for exporting undirected graphs.

# The intermediate graph format

  graph(Vs, Ranks, Es, G_Attrs)

The following attributes are supported:
  * `label(Name:atom)`

## Ranks format

  rank(RankVertex, ContentVertices)

## Vertex format

  vertex(V_Id, V_Attrs)

The following attributes are supported:
  * `color(Color:atom)`
  * `coord(Coord:coord)`
  * `label(Name:atom)`
  * `radius(Radius:float)`

## Edge format

  edge(FromV/FromV_Id, ToV/ToV_Id, E_Attrs)

The following attributes are supported:
  * `arrow_type(ArrowType:atom)`
  * `color(Color:atom)`
  * `label(Name:atom)`
  * `style(Style:atom)`

@author Wouter Beek
@version 2013/02-2013/03, 2013/07
*/

:- use_module(graph_theory(ugraph_ext)).
:- use_module(library(lists)).

:- meta_predicate(export_ugraph(+,4,+,+)).

:- setting(default_radius, float, 0.1, 'The default radius of vertices.').



%! export_ugraph(+Options, :CoordFunc, +UG:ugraph, -G_Term:compound) is det.
% Exports the given unordered graph to the intermediate graph format.
%
% @arg Options Supported values:
%      * `border(+Border:coord)`
%      * `surface(+Surface:coord)`
% @arg CoordFunc A function that maps vertices to coordinates.
% @arg UG An undirected graph.
% @arg G A compound term in the intermediate graph format.

export_ugraph(Options, CoordFunc, UG, graph(V_Terms, [], E_Terms, G_Attrs)):-
  ugraph_vertices(UG, UG_Vs),
  setting(default_radius, V_R),
  findall(
    vertex(V_Id, V_Attrs),
    (
      nth0(V_Id, UG_Vs, V),
      call(CoordFunc, Options, Vs, V, V_Coord),
      ugraph_vertex_color(V, V_Color),
      ugraph_vertex_name(V, V_Name),
      V_Attrs = [color(V_Color), coord(V_Coord), label(V_Name), radius(V_R)]
    ),
    V_Terms
  ),
  
  ugraph_edges(UG, UG_Es),
  findall(
    edge(FromV/FromV_Id, ToV/ToV_Id, E_Attrs),
    (
      member(FromV-ToV, UG_Es),
      nth0(FromV_Id, Vs, FromV),
      nth0(ToV_Id, Vs, ToV),
      ugraph_edge_arrow_type(FromV-ToV, E_ArrowType),
      ugraph_edge_color(FromV-ToV, E_Color),
      ugraph_edge_name(FromV-ToV, E_Name),
      ugraph_edge_style(FromV-ToV, E_Style),
      E_Attrs = [
        arrow_type(E_ArrowType),
        color(E_Color),
        label(E_Name),
        style(E_Style)
      ]
    ),
    E_Terms
  ),
  
  ugraph_name(UG, UG_Name),
  G_Attrs = [label(UG_Name)].

%! ugraph_edge_arrow_type(+E:edge, -ArrowType:atom) is det.
% @arg ArrowType One of the following values:
%      * `box`
%      * `crow`
%      * `diamond`
%      * `dot`
%      * `ediamond`
%      * `empty`
%      * `halfopen`
%      * `inv`
%      * `invodot`
%      * `normal`
%      * `invdot`
%      * `invempty`
%      * `none`
%      * `odiamond`
%      * `obox`
%      * `odot`
%      * `open`
%      * `tee`
%      * `vee`

ugraph_edge_arrow_type(_FromV-_ToV, normal).

ugraph_edge_color(_FromV-_ToV, black).

%! ugraph_edge_name(+E:edge, -Name:atom) is det.
% Returns a name for the given edge.

ugraph_edge_name(FromV-ToV, Name):-
  maplist(ugraph_vertex_name, [FromV, ToV], [FromV_Name, ToV_Name]),
  format(atom(Name), '~w <-> ~w', [FromV_Name, ToV_Name]).

%! ugraph_edge_style(+E:edge, -Style:atom) is det.
% @arg Style One of the following values:
%      * `bold`
%      * `dashed`
%      * `dotted`
%      * `invis`
%      * `solid`
%      * `tapered`

ugraph_edge_style(_FromV-_ToV, solid/normal).

ugraph_name(UG, Name):-
  term_to_atom(UG, Name).

ugraph_vertex_color(_V, black).

ugraph_vertex_name(V, V_Name):-
  term_to_atom(V, V_Name).
