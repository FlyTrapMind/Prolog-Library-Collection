:- module(
  gv_dcg,
  [
  ]
).

/** <module> GV_DCG

DCG rules for GraphViz DOT file generation.

Methods for writing to the GraphViz DOT format.

In GraphViz vertices are called 'nodes'.

@author Wouter Beek
@see http://www.graphviz.org/content/dot-language
@version 2013/07
*/

:- use_module(gv(gv_attrs)).


% No attributes.
gv_a_list(_Attrs, []) --> [].
% A single attribute.
gv_a_list(Attrs, [Name=Value]) -->
  {gv_attribute_value(Attrs, Name=Value)},
  gv_id(Name),
  "=",
  gv_id(Value).
% Multiple attributes, separated by comma.
gv_a_list(Attrs_, [Attr|Attrs]) -->
  gv_a_list(Attrs_, [Attr]),
  ",",
  gv_a_list(Attrs_, Attrs).

% Attributes occur between square brackets.
gv_attribute_list(G_Attrs, Attrs) -->
  "[",
  {merge_options(G_Attrs, Attrs, Attrs_)},
  gv_a_list(Attrs_, Attrs),
  "]".

gv_compass_pt --> "_".
gv_compass_pt --> "c".
gv_compass_pt --> "e".
gv_compass_pt --> "n".
gv_compass_pt --> "ne".
gv_compass_pt --> "nw".
gv_compass_pt --> "s".
gv_compass_pt --> "se".
gv_compass_pt --> "sw".
gv_compass_pt --> "w".

gv_edge_operator(directed) --> "->".
gv_edge_operator(undirected) --> "--".

%! gv_edge_rhs(+G_Attrs:list(nvpair), +To_Id)//
% @tbd Instead of gv_node_id//1 we could have a gv_subgraph//1 here.
% @tbd Add support for multiple, consecutive occurrences of gv_edge_rhs//2.

gv_edge_rhs(G_Attrs, To_Id) -->
  {option(directedness(Dir), G_Attrs, undirected)},
  gv_edge_operator(Dir), space,
  gv_node_id(To_Id).

%! gv_edge_statement(+I:integer, +G_Attrs:list(nvpair), +E_Term:compound)//
% @tbd Instead of gv_node_id//1 we could have a gv_subgraph//1 here.

gv_edge_statement(I, G_Attrs, edge(From_Id, To_Id, E_Attrs)) -->
  indent(I), gv_node_id(From_Id), space,
  gv_edge_rhs(G_Attrs, To_Id), space,
  % We want `colorscheme/1` from the edges and
  % `directionality/1` from the graph.
  gv_attribute_list(G_Attrs, E_Attrs), newline.

gv_edge_statements(_I, _G_Attrs, []) --> [].
gv_edge_statements(I, G_Attrs, [E_Term|E_Terms]) -->
  gv_edge_statement(I, G_Attrs, E_Term),
  gv_edge_statements(I, G_Attrs, E_Terms).

gv_generic_edge_attributes_statement(I, G_Attrs, E_Attrs) -->
  indent(I), e,d,g,e, space,
  gv_attribute_list(G_Attrs, E_Attrs), newline.

gv_generic_graph_attributes_statement(I, G_Attrs, G_Attrs) -->
  indent(I), g,r,a,p,h, space,
  gv_attribute_list(G_Attrs, G_Attrs), newline.

gv_generic_node_attributes_statement(I, G_Attrs, V_Attrs) -->
  indent(I), n,o,d,e, space,
  gv_attribute_list(G_Attrs, V_Attrs), newline.

%! gv_graph(+GraphTerm:compound)//
% The follow graph attributes are supported:
%   1. `directonality(+Directionality:oneof([directed,undirected]))`
%      A directed graph uses the keyword `digraph`.
%      An undirected graph uses the keyword `graph`.
%   2. `name(+GraphName:atom)`
%   3. `strict(+StrictGraph:boolean)`
%      This forbids the creation of self-arcs and multi-edges;
%      they are ignored in the input file.
%      Only in combinattion with directionality `directed`.
%
% @tbd Add support for subgraphs in edge statements.
% @tbd Add support for HTML-like labels:
%      http://www.graphviz.org/doc/info/shapes.html#html
% @tbd Add support for escape strings:
%      http://www.graphviz.org/doc/info/attrs.html#k:escString

gv_graph(graph(V_Terms, E_Terms, G_Attrs)) -->
  {
    shared_attributes(V_Terms, V_Attrs, NewV_Terms),
    shared_attributes(E_Terms, E_Attrs, NewE_Terms),
    option(strict(Strict), G_Attrs, false),
    option(directedness(Dir), G_Attrs, undirected),
    option(name(G_Name), G_Attrs),
    I = 0
  },
  indent(I), gv_strict(Strict), space,
  gv_graph_type(Dir), space,
  gv_id(G_Name), space,
  "{", newline,
  
  {NewI is I + 1},
  gv_generic_graph_attributes_statement(NewI, G_Attrs3, G_Attrs3),
  gv_generic_node_attributes_statement(NewI, G_Attrs3, V_Attrs),
  gv_generic_edge_attributes_statement(NewI, G_Attrs3, E_Attrs),
  newline,
  gv_node_statements(I, G_Attrs3, NewV_Terms),
  newline,
  gv_edge_statements(I, G_Attrs3, NewE_Terms),
  
  indent(I), "}".

gv_graph_type(directed) --> d,i,g,r,a,p,h.
gv_graph_type(undirected) --> g,r,a,p,h.

%! gv_id(-Codes:list(code))// is det.
% Parse a GraphViz identifier.
% There are 4 variants:
%   1. Any string of alphabetic (`[a-zA-Z'200-'377]`) characters,
%      underscores (`_`) or digits (`[0-9]`), not beginning with a digit.
%   2. A numeral `[-]?(.[0-9]+ | [0-9]+(.[0-9]*)? )`.
%   3. Any double-quoted string (`"..."`) possibly containing
%      escaped quotes (`\"`).
%      In quoted strings in DOT, the only escaped character is
%      double-quote (`"`). That is, in quoted strings, the dyad `\"`
%      is converted to `"`. All other characters are left unchanged.
%      In particular, `\\` remains `\\`.
%      Layout engines may apply additional escape sequences.
%   4. An HTML string (`<...>`).
%
% @tbd Add the grammar for HTML strings. This requires an XML grammar!

gv_id(X) -->
  gv_id_(X),
  % GraphViz ids that are not double-quotes cannot be one of
  % (the case-variants of) the GraphViz keywords.
  {\+ gv_keyword(X)}.
% Double-quoted ids may be GraphViz keywords.
gv_id([H|T]) -->
  double_quote(H),
  gv_quoted_string(S),
  double_quote(H),
  {append(S, [H], T)}.

gv_id_([H|T]) -->
  gv_id_first(H),
  gv_id_rest(T).
gv_id_(N) --> float(N).
% HTML string.
%gv_id_ -->
%  "<",
%  gv_html_string,
%  ">".

gv_id_first(X) --> letter(X).
gv_id_first(X) --> underscore(X).

gv_id_rest([]) --> [].
gv_id_rest([H|T]) -->
  alpha_numeric(H),
  gv_id_rest(T).
gv_id_rest([H|T]) -->
  underscore(H),
  gv_id_rest(T).

gv_keyword(Codes):-
  phrase(gv_keyword, Codes).

% GraphViz has reserved keywords that cannot be used as identifiers.
% GraphViz keywords are case-insensitive.
gv_keyword --> d,i,g,r,a,p,h.
gv_keyword --> e,d,g,e.
gv_keyword --> g,r,a,p,h.
gv_keyword --> n,o,d,e.
gv_keyword --> s,t,r,i,c,t.
gv_keyword --> s,u,b,g,r,a,p,h.

gv_node_id(V_Id) -->
  gv_id(V_Id).
%gv_node_id(_) -->
%  gv_id(_),
%  gv_port.

gv_node_statement(I, G_Attrs, vertex(V_Id, _V, V_Attrs)) -->
  indent(I), gv_node_id(V_Id), space,
  gv_attribute_list(G_Attrs, V_Attrs), newline.

gv_node_statements(_I, _G_Attrs, []) --> [].
gv_node_statements(I, G_Attrs, [V_Term|V_Terms]) -->
  gv_node_statement(I, G_Attrs, V_Term),
  gv_node_statements(I, G_Attrs, V_Terms).

gv_port -->
  gv_port_location,
  (gv_port_angle ; "").
gv_port -->
  gv_port_angle,
  (gv_port_location ; "").
gv_port -->
  ":",
  gv_compass_pt.

gv_port_angle -->
  "@",
  gv_id(_).

gv_port_location -->
  ":",
  gv_id(_).
gv_port_location -->
  ":(",
  gv_id(_),
  ",",
  gv_id(_),
  ")".

% No double quote.
gv_quoted_string([H|T]) -->
  dcg_peek(H),
  {H \== 34},
  [H],
  gv_quoted_string(T).
% A double quote is only allowed if it is escaped by a backslash.
gv_quoted_string([X,Y|T]) -->
  backslash(X),
  double_quote(Y),
  gv_quoted_string(T).
% May end normally / in non-backslash.
gv_quoted_string([]) --> [].

gv_strict(false) --> [].
gv_strict(true) -->
  s,t,r,i,c,t.
