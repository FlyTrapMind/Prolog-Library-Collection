:- module(
  graph_export_graphviz,
  [
    export_graph_graphviz/2 % +Out
                            % +Graph
  ]
).

/** <module> GRAPH_EXPORT_GRAPHVIZ

@author Wouter Beek
@tbd Use DCGs to file.
@tbd Use the GV module to lookup whether an attribute is supported or not.
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_c)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate(attribute(//,+,?,?)).
:- meta_predicate(inline_attributes(//,+,?,?)).



%! attribute(+Separator:list(code), +Attribute:nvpair)// is det.

attribute(_Separator, Name=_Value) -->
  {disabled_attribute(Name)}, !.
attribute(Separator, Name=Value) -->
  {
    atom_codes(Value, ValueCodes),
    phrase(c_convert, ValueCodes, C_ValueCodes),
    atom_codes(Name, NameCodes)
  },
  ({flag(first, 0, 1)} -> "" ; Separator),
  NameCodes,
  equals_sign,
  double_quote,
  C_ValueCodes,
  double_quote.

disabled_attribute(coord).

%! export_edge_graphviz(+Indent:integer, +Edge:compound)//
% Writes an edge term.
%
% @arg Indent An integer indicating the indentation.
% @arg Edge A GraphViz edge compound term.

export_edge_graphviz(Indent, edge(FromV_Id1, ToV_Id1, E_Attrs)) -->
  indent(Indent),
  "node_", {number_codes(FromV_Id1, FromV_Id2)}, FromV_Id2,
  space,
  dcg_arrow(2),
  space,
  "node_", {number_codes(ToV_Id1, ToV_Id2)}, ToV_Id2,
  space,
  inline_attributes(", ", E_Attrs),
  semi_colon,
  newline.

%! export_graph_graphviz(+Stream:stream, +Graph:compound) is det.
% Writes a graph term that is in Graph Intermediary Format
% to the given output stream.
%
% @arg Stream An output stream.
% @arg Graph A graph representation in the following format:
%      `graph(Vertices, Ranks, Edges, GraphAttrs)`.

export_graph_graphviz(Out, G):-
  phrase(export_graph_graphviz(G), Codes, []),
  put_codes(Out, Codes).

export_graph_graphviz(graph(Vs, Ranks, Es, G_Attrs)) -->
  {
    Indent = 1,
    option(graph_name(G_Name), G_Attrs, noname),
    atom_codes(G_Name, G_NameCodes)
  },
  "digraph",
  space,
  G_NameCodes,
  opening_curly_bracket,

  % Vertices: ranked
  dcg_multi_list(export_rank_graphviz(Indent), Ranks),
  newline,

  % Vertices: unranked
  dcg_multi_list(export_vertex_graphviz(Indent), Vs),
  newline,

  % Edges: rank edges
  {
    findall(
      RankV_Id,
      member(
        rank(vertex(RankV_Id, _RankV_Attrs), _ContentVs),
        Ranks
      ),
      RankV_Ids
    )
  },
  export_rank_edges_graphviz(Indent, RankV_Ids),
  newline,

  % Edges: nonrank edges
  dcg_multi_list(export_edge_graphviz(Indent), Es),
  newline,

  % Graph properties
  multiline_attributes(Indent, G_Attrs),
  closing_curly_bracket.

%! export_rank_graphviz(+Indent:integer, +Rank:rank)//
% @arg Indent An integer indicating the indentation.

export_rank_graphviz(Indent, rank(RankVertex, ContentVertices)) -->
  % Open rank.
  indent(Indent),
  opening_curly_bracket,

  % Rank property.
  {NewIndent is Indent + 1},
  indent(NewIndent),
  "rank=same;",
  newline,

  % Write vertices.
  dcg_multi_list(
    export_vertex_graphviz(NewIndent),
    [RankVertex | ContentVertices]
  ),

  % Close rank.
  indent(Indent),
  closing_curly_bracket.

export_rank_edges_graphviz(_Indent, []) --> [].
export_rank_edges_graphviz(_Indent, [_RankV_Id]) --> [].
export_rank_edges_graphviz(Indent, [RankV1_Id, RankV2_Id | RankV_Ids]) -->
  % No edge properties are written.
  export_edge_graphviz(
    Indent,
    edge(_RankV1/RankV1_Id, _RankV2/RankV2_Id, [])
  ),
  export_rank_edges_graphviz(Indent, [RankV2_Id | RankV_Ids]).

%! export_vertex_graphviz(+Indent:integer, +Vertex:vertex)//
% Writes a vertex term.
%
% @arg Indent An integer indicating the indentation.
% @arg Vertex A vertex compound term.

export_vertex_graphviz(Indent, vertex(V_Id, V_Attrs)) -->
  indent(Indent),
  {atom_codes(V_Id, V_Id_Codes)},
  "node_", V_Id_Codes, space,
  inline_attributes(", ", V_Attrs),
  ";".

inline_attributes(Separator, Attributes) -->
  opening_square_bracket,
  dcg_multi_list(attribute(Separator), Attributes),
  closing_square_bracket,
  {flag(first, _OldId, 0)}.

multiline_attribute_separator -->
  newline,
  dcg_multi(space, 2).

%! multiline_attributes(+Indent:integer, +G_Attrs:list(nvpair))//
% Writes the given GraphViz graph attributes.
%
% The writing of graph attributes deviates a little bit from the writing of
% edge and node attributes, because the written attributes are not enclosed in
% square brackets and they are written on separate lines (and not as
% comma-separated lists).
%
% @arg Indent An integer indicating the indentation.
% @arg G_Attrs A list of name-value pairs.

multiline_attributes(Indent, G_Attrs) -->
  indent(Indent),
  dcg_multi_list(
    attribute(multiline_attribute_separator),
    G_Attrs
  ),
  newline,
  {flag(first, _OldId, 0)}.

