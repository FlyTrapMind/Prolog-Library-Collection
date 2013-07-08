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

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_c)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).



%! export_graph_attribute(+Separator:atom, +Attribute:nvpair) is det.
% Writes an attribute, together with a separator.
%
% @arg Attribute A name-value pair.
% @arg Separator An atomic separator.

export_graph_attribute(Separator, Attribute):-
  Attribute =.. [Name, Value],
  (
    attribute(Name, _Type, _Context, _Attributes, _Default)
  ->
    dcg_phrase(c_convert, Value, C_Value),
    format('~w="~w"~w', [Name, C_Value, Separator])
  ;
    true
  ).

%! export_graph_attributes(+Attributes:list(nvpair), +Separator:atom) is det.
% Writes the given list of attributes to an atom.
%
% @arg Attributes A list of name-value pairs.
% @arg Separator An atomic separator that is written between the attributes.

% Empty attribute list.
export_graph_attributes([], _Separator):- !.
% Non-empty attribute list.
% Write the open and colsing signs of the attribute list.
export_graph_attributes(Attributes, Separator):-
  write('['),
  export_graph_attributes_(Attributes, Separator),
  write(']').

% We know that the list in not empty.
% For the last attribute in the list we use the empty separator.
export_graph_attributes_(Stream, [Attribute], _Separator):- !,
  export_graph_attribute(Stream, '', Attribute).
export_graph_attributes_(Stream, [Attribute | Attributes], Separator):-
  export_graph_attribute(Stream, Separator, Attribute),
  export_graph_attributes_(Stream, Attributes, Separator).

%! export_edge_graphviz(+Indent:integer, +Edge:edge) is det.
% Writes an edge term.
%
% @arg Indent An integer indicating the indentation.
% @arg Edge A GraphViz edge compound term.

export_edge_graphviz(Indent, edge(_FromV/FromV_Id, _ToV/ToV_Id, E_Attrs)):-
  indent(Indent),
  format('node_~w -> node_~w ', [FromV_Id, ToV_Id]),
  export_graph_attributes(E_Attrs, ', '),
  writeln(';').

%! stream_graph_attributes(+Indent:integer, +G_Attrs:list(nvpair)) is det.
% Writes the given GraphViz graph attributes.
%
% The writing of graph attributes deviates a little bit from the writing of
% edge and node attributes, because the written attributes are not enclosed in
% square brackets and they are written on separate lines (and not as
% comma-separated lists).
%
% @arg Indent An integer indicating the indentation.
% @arg G_Attrs A list of name-value pairs.

stream_graph_attributes(Indent, G_Attrs):-
  indent(Indent),
  export_graph_attributes_(G_Attrs, '\n  '),
  nl.

%! export_graph_graphviz(+Stream:stream, +G:compound) is det.
% Writes a GraphViz structure to an output stream.
%
% @arg Stream An output stream.
% @arg G A graph representation in the intermediate format
%      `graph(Vertices, Edges, GraphAttrs)`.

export_graph_graphviz(Out, G):-
  with_output_to(Out, export_graph_graphviz(G)).

export_graph_graphviz(graph(Vs, Ranks, Es, G_Attrs)):-
  Indent = 1,

  % Header
  option(graph_name(G_Name), G_Attrs, noname),
  formatln('digraph ~w {', [G_Name]),

  % Vertices: ranked
  maplist(export_graph_rank(Indent), Ranks),
  nl,

  % Vertices: unranked
  maplist(export_graph_vertex(Indent), Vs),
  nl,

  % Edges: rank edges
  findall(
    RankV_Id,
    member(
      rank(vertex(RankV_Id, _RankV_Attrs), _ContentVs),
      Ranks
    ),
    RankV_Ids
  ),
  export_graph_rank_edges(Indent, RankV_Ids),
  nl,

  % Edges: nonrank edges
  maplist(export_edge_graphviz(Indent), Es),
  nl,

  % Graph properties
  stream_graph_attributes(Indent, G_Attrs),

  % Footer
  writeln('}').

%! export_graph_rank(+Indent:integer, +Rank:rank) is det.
% @arg Indent An integer indicating the indentation.

export_graph_rank(Indent, rank(RankVertex, ContentVertices)):-
  indent(Indent),
  writeln('{'),
  NewIndent is Indent + 1,
  indent(NewIndent),
  writeln('rank=same;'),
  maplist(export_graph_vertex(NewIndent), [RankVertex | ContentVertices]),
  indent(Indent),
  writeln('}').

export_graph_rank_edges(_Indent, []):- !.
export_graph_rank_edges(_Indent, [_RankV_Id]):- !.
export_graph_rank_edges(Indent, [RankV1_Id, RankV2_Id | RankV_Ids]):-
  export_edge_graphviz(Indent, edge(RankV1/RankV1_Id, RankV2/RankV2_Id, [])),
  export_graph_rank_edges(Indent, [RankV2_Id | RankV_Ids]).

%! export_graph_vertex(+Indent:integer, +Vertex:vertex) is det.
% Writes a vertex term.
%
% @arg Indent An integer indicating the indentation.
% @arg Vertex A GraphViz vertex compound term.

export_graph_vertex(Indent, vertex(V_Id, V_Attrs)):-
  indent(Indent),
  format('node_~w ', [V_Id]),
  export_graph_attributes(V_Attrs, ', '),
  writeln(';').
