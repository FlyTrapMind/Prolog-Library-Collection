:- module(
  rdf_graph_theory,
  [
    rdf_beam/5, % +Options:list(nvpair)
                % +RootVertex:vertex
                % +Predicates:list
                % -Vertices:ordset(vertex)
                % -Edges:ordset(edge)
    rdf_edge/2, % +Graph:atom
                % ?Edge:edge
    rdf_edge/3, % +Options:list(nvpair)
                % +Graph:atom
                % ?Edge:edge
    rdf_edges/2, % +Graph:atom
                 % ?Edges:ord_set(edge)
    rdf_edges/3, % +Options:list(nvpair)
                 % +Graph:atom
                 % ?Edges:ord_set(edge)
    rdf_edges_to_vertices/2, % +Edges:ord_set(edge)
                             % -Vertices:ord_set(vertex)
    rdf_graph_to_ugraph/2, % +Graph:atom
                           % -UG:ugraph
    rdf_subgraph/2, % +SubVertices:ordset(vertex)
                    % +SubGraph:graph
    rdf_neighbor/3, % +Graph:atom
                    % ?Vertex:vertex
                    % ?Neighbor:vertex
    rdf_neighbors/3, % +Graph:atom
                     % +Vertex:vertex
                     % -Neighbors:ordset(vertex)
    rdf_vertex/2, % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex/3, % +Options:list(nvpair)
                  % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex_equivalence/2, % +Resource1:uri
                              % +Resource2:uri
    rdf_vertices/2, % +Graph:atom
                    % -Vertices:ord_set(vertex)
    rdf_vertices/3 % +Options:list(nvpair)
                   % +Graph:atom
                   % -Vertices:ord_set(vertex)
  ]
).

/** <module> RDF graph theory

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
edges (as defined by RDF abstract syntax) in one triple can be nodes in
another. This means that the definitions 'edge' and 'vertex' for graph
theoretic operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03, 2013/07
*/

:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_read)). % Used for meta-calls.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_beam(+,r,+,-,-)).
:- rdf_meta(rdf_edge(+,r)).
:- rdf_meta(rdf_edge(+,+,r)).
:- rdf_meta(rdf_edges(+,r)).
:- rdf_meta(rdf_edges(+,+,r)).
:- rdf_meta(rdf_neighbor(+,r,r)).
:- rdf_meta(rdf_neighbors(+,r,-)).
:- rdf_meta(rdf_vertex(+,r)).
:- rdf_meta(rdf_vertex(+,+,r)).
:- rdf_meta(rdf_vertex_equivalence(r,r)).



%! rdf_beam(
%!   +Options:list(nvpair),
%!   +RootVertex,
%!   +Predicates:list,
%!   -Vertices:ord_set(vertex),
%!   -Edges:ord_set(edge)
%! ) is det.

rdf_beam(O, V, Ps, Vs, Es):-
  rdf_beam(O, [V], Ps, Vs, [], Es).

rdf_beam(_O, [], _Ps, AllVs, AllEs, AllEs):-
  rdf_edges_to_vertices(AllEs, AllVs), !.
rdf_beam(O, Vs, Ps, AllVs, Es, AllEs):-
  setoff(
    V-NextV,
    (
      member(V, Vs),
      member(P, Ps),
      rdf_has(V, P, NextV),
      \+ member(V-NextV, Es)
    ),
    NextEs
  ),
  ord_union(Es, NextEs, NewEs),
  rdf_edges_to_vertices(NextEs, NextVs),
  rdf_beam(O, NextVs, Ps, AllVs, NewEs, AllEs).

rdf_bnode_to_var(S, _):-
  rdf_is_bnode(S), !.
rdf_bnode_to_var(X, X).

rdf_edge(G, E):-
  rdf_edge([], G, E).

%! rdf_edge(+Options:list(nvpair), +Graph:atom, ?Edge:edge) is nondet.
% RDF edges between resources.
%
% @arg Options A list of the following name-value pairs:
%      1. `directed(+DirectedGraph:boolean)` Whether or not the
%         directionality of the edge is taken into account.
%      2. `literals(+ShowLiterals:oneof([collapse,hide,labels_only,show]))`
%         Whether or not literals are allowed as vertices in edges.
%         Default: `collapse`.
%      3. `named_edges(+Named:boolean)`
%         Whether edges are qualified by the RDF predicate term.
% @arg Graph The atomic name of an RDF graph.
% @arg Edge An edge, either `FromV-ToV` or `FromV-P-ToV`.

rdf_edge(O1, G, Edge):-
  % Whether the edge's directionality is relevant or not.
  (
    select_option(directed(true), O1, O2, false)
  ->
    rdf(FromV, Predicate, ToV, G)
  ;
    (
      rdf(FromV, Predicate, ToV, G)
    ;
      rdf(ToV, Predicate, FromV, G)
    )
  ),
  
  % Make sure the vertices pass the vertex filter.
  rdf_vertex(O2, FromV),
  rdf_vertex(O2, ToV),
  
  % Whether or not literal vertices are included.
  unless(
    select_option(literals(show), O2, O3, collapse),
    (
      % When the literals option is set to `collapse` or `hide`,
      % then edges incident with a literal vertex are excluded.
      FromV \= literal(_),
      ToV \= literal(_)
    )
  ),
  
  if_then_else(
    select_option(named_edges(true), O3, _O4, false),
    Edge = FromV-Predicate-ToV,
    Edge = FromV-ToV
  ).

rdf_edges(G, Es):-
  rdf_edges([], G, Es).

rdf_edges(O, G, Es):-
  setoff(E, rdf_edge(O, G, E), Es).

rdf_edges_to_vertices(Es, Vs):-
  setoff(V, (member(V-_W1, Es) ; member(_W2-V, Es)), Vs).

%! rdf_graph_to_ugraph(+Graph:atom, -UGraph:ugraph) is det.
% Returns the UG representation of a loaded RDF graph.
%
% @arg G The atomic name of a loaded RDF graph.
% @arg UG:ugraph A UG datastructure.

rdf_graph_to_ugraph(G, UG):-
  setoff(
    From-Neighbors,
    (
      rdf_vertex(G, From),
      setoff(
        To,
        rdf_edge(G, From-To),
        Neighbors
      )
    ),
    UG
  ).

%! rdf_neighbor(+Graph:atom, ?Vertex:vertex, ?Neighbor:vertex) is nondet.
% Neighboring vertices.

rdf_neighbor(G, V1, V2):-
  rdf_edge(G, V1-V2).

rdf_neighbors(G, V, Ns):-
  setoff(N, rdf_neighbor(G, V, N), Ns).

%! rdf_subgraph(+Vertices:ordset(vertice), +SubGraph:graph) is semidet.
% Succeeds if the graph in Options has SubGraph as one of its vertice-induced
% subgraph.

rdf_subgraph(SubG, G):-
  % Make sure that the vertices are a subset.
  rdf_vertices(SubG, SubVs),
  rdf_vertices(G, Vs),
  ord_subset(SubVs, Vs),
  % Make sure all triples in the subgraph occur in the supergraph as well.
  % Note that this can be tricky for blank nodes.
  forall(
    rdf(S1, P, O1, SubG),
    (
      maplist(rdf_bnode_to_var, [S1,O1], [S2,O2]),
      rdf(S2, P, O2, G)
    )
  ).

rdf_vertex(G, V):-
  rdf_vertex([], G, V).

%! rdf_vertex(+Options:list(nvpair), +Graph:atom, ?Vertex:uri) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% @arg Options A list of name-value pairs.
%      1. `literals(oneof([collapse,hide,labels_only,show]))`
%         Whether or not literals are allowed as vertices in the edge.
%         Default: `collapse`.
%      2. `rdf_list(onef([concise,full]))`
%         Whether vertices that are part of an RDF list
%         should be included in full (default) or in a concise way.
%      3. `vertex(oneof([rdf_node,rdf_term])`
%         Value `rdf_node` means that only subject and object terms are
%         considered as vertices (default).
%         Value `rdf_term` means that subject, predicate and object terms
%         are considered as vertices.
% @arg Graph The atomic name of an RDF graph.
% @arg Vertex An RDF term.

rdf_vertex(O1, G, V):-
  % RDF nodes or RDF terms?
  select_option(vertex(VerticePredicate), O1, O2, rdf_node),
  call(VerticePredicate, G, V),

  % RDF literals.
  % If the literal setting is `collapse` or `hide' then literals
  % are not allowed as vertices.
  unless(
    select_option(literals(show), O2, O3, collapse),
    V \= literal(_Literal)
  ),

  % RDF lists.
  if_then(
    select_option(rdf_list(concise), O3, _O4, full),
    if_then_else(
      is_rdf_list(V),
      % Only RDF list vertices that are list heads are included.
      \+ rdf_has(_, rdf:rest, V),
      % Non-RDF list vertices should not occur as a member of an RDF list.
      \+ rdf_has(_, rdf:first, V)
    )
  ).

% @tbd What is this?
rdf_vertex_equivalence(X, Y):-
  % Subject
  forall(
    rdf_has(X, P, O),
    rdf_has(Y, P, O)
  ),
  forall(
    rdf_has(Y, P, O),
    rdf_has(X, P, O)
  ),
  % Predicate
  forall(
    rdf_has(S, X, O),
    rdf_has(S, Y, O)
  ),
  forall(
    rdf_has(S, Y, O),
    rdf_has(S, X, O)
  ),
  % Object
  forall(
    rdf_has(S, P, X),
    rdf_has(S, P, Y)
  ),
  forall(
    rdf_has(S, P, Y),
    rdf_has(S, P, X)
  ).

rdf_vertices( G, Vs):-
  rdf_vertices([], G, Vs).

rdf_vertices(O, G, Vs):-
  setoff(V, rdf_vertex(O, G, V), Vs).
