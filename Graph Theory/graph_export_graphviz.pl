:- module(
  graph_export_graphviz,
  [
    export_graph_graphviz/2 % +Stream:stream
                            % +Graph
  ]
).

/** <module> GRAPH_EXPORT_GRAPHVIZ

@author Wouter Beek
@tbd Use DCGs to file.
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_c)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).



inline_attribute(Separator, Attribute) -->
  {
    Attribute =.. [Name, Value],
    atom_codes(Value, ValueCodes),
    dcg_phrase(c_convert, ValueCodes, C_ValueCodes),
    atom_codes(Name, NameCodes)
  },
  [NameCodes],
  equals_sign,
  double_quote,
  [C_ValueCodes],
  double_quote,
  [Separator].

inline_attributes(Separator, Attributes) -->
  opening_square_bracket,
  dcg_multi_list(inline_attribute(Separator), Attributes),
  closing_square_bracket.

%! edge(+Indent:integer, +Edge:compound)//
% Writes an edge term.
%
% @arg Indent An integer indicating the indentation.
% @arg Edge A GraphViz edge compound term.

edge(Indent, edge(_FromV/FromV_Id1, _ToV/ToV_Id, E_Attrs)) -->
  dcg_indent(Indent),
  "node_", {number_codes(FromV_Id1, FromV_Id2)}, [FromV_Id2],
  space,
  dcg_arrow(2),
  space,
  "node_", {number_codes(ToV_Id1, ToV_Id2)}, [ToV_Id2],
  space,
  inline_attributes(", ", E_Attrs),
  semi_colon.

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
  dcg_indent(Indent),
  dcg_multi_list(
    multiline_attribute(multiline_attribute_separator),
    G_Attrs
  ),
  newline.

%! export_graph_graphviz(+Stream:stream, +Graph:compound) is det.
% Writes a graph term that is in Graph Intermediary Format
% to the given output stream.
%
% @arg Stream An output stream.
% @arg Graph A graph representation in the following format:
%      `graph(Vertices, Ranks, Edges, GraphAttrs)`.

export_graph_graphviz(Out, G):-
  phrase(export_graph(G), Codes, []),
  put_codes(Out, Codes).

export_graph(graph(Vs, Ranks, Es, G_Attrs)) -->
  {
    Indent = 1,
    option(graph_name(G_Name), G_Attrs, noname),
    atom_codes(G_Name, G_NameCodes)
  },
  "digraph",
  space,
  G_NameCodes,
  opening_curly_bracket.

  % Vertices: ranked
  dcg_multi_list(rank(Indent), Ranks),
  newline,

  % Vertices: unranked
  dcg_multi_list(vertex(Indent), Vs),
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
  rank_edges(Indent, RankV_Ids),
  newline,

  % Edges: nonrank edges
  dcg_multi_list(edge(Indent), Es),
  newline,

  % Graph properties
  multiline_attributes(Indent, G_Attrs),
  closing_curly_bracket.

%! rank(+Indent:integer, +Rank:rank)//
% @arg Indent An integer indicating the indentation.

rank(Indent, rank(RankVertex, ContentVertices)) -->
  % Open rank.
  dcg_indent(Indent),
  opening_curly_bracket,
  
  % Rank property.
  {NewIndent is Indent + 1},
  dcg_indent(NewIndent),
  "rank=same;",
  newline,
  
  % Write vertices.
  dcg_multi_list(vertex(NewIndent), [RankVertex | ContentVertices]),
  
  % Close rank.
  dcg_indent(Indent),
  closing_curly_bracket.

rank_edges(_Indent, []) --> [].
rank_edges(_Indent, [_RankV_Id]) --> [].
rank_edges(Indent, [RankV1_Id, RankV2_Id | RankV_Ids]) -->
  % No edge properties are written.
  edge(Indent, edge(RankV1/RankV1_Id, RankV2/RankV2_Id, [])),
  export_graph_rank_edges(Indent, [RankV2_Id | RankV_Ids]).

%! vertex(+Indent:integer, +Vertex:vertex)//
% Writes a vertex term.
%
% @arg Indent An integer indicating the indentation.
% @arg Vertex A vertex compound term.

export_graph_vertex(Indent, vertex(V_Id, V_Attrs)) -->
  dcg_indent(Indent),
  {atom_codes(V_Id, V_Id_Codes)},
  "node_", V_Id_Codes, space,
  inline_attributes(", ", V_Attrs),
  ";".

