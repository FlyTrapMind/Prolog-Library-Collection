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
                 % ?Edges:ordset(edge)
    rdf_edges/3, % +Options:list(nvpair)
                 % +Graph:atom
                 % ?Edges:ordset(edge)
    rdf_edges_to_vertices/2, % +Edges:ordset(edge)
                             % -Vertices:ordset(vertex)
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
                    % -Vertices:ordset(vertex)
    rdf_vertices/3 % +Options:list(nvpair)
                   % +Graph:atom
                   % -Vertices:ordset(vertex)
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
:- use_module(rdfs(rdfs_read)).
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
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge)
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
% The following options are supported:
%   1. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%
% @arg Options A list of name-value pairs.
% @arg Graph The atomic name of an RDF graph.
% @arg Edge An edge, either `FromV-ToV` or `FromV-P-ToV`.

rdf_edge(O, G, FromV-P-ToV):-
  rdf(FromV, P, ToV, G),
  % Make sure the vertices pass the vertex filter.
  rdf_vertex_check(O, FromV),
  rdf_vertex_check(O, ToV).

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

%! rdf_vertex(+Options:list(nvpair), +Graph:atom, ?Vertex:rdf_term) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% The following options are supported:
%   1. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%   2. `rdf_list(+Include:boolean)`
%      Whether vertices that are part of an RDF list should be included
%      in full (`true`, default) or in a concise way (`false`).
%
% @arg Options A list of name-value pairs.
% @arg Graph The atomic name of an RDF graph.
% @arg Vertex An RDF term.

rdf_vertex(O, G, V):-
  (rdf(V, _, _, G) ; rdf(_, V, _, G) ; rdf(_, _, V, G)),
  rdf_vertex_check(O, V).

% Typed literals are only included when `literals-all`.
rdf_vertex_check(O, literal(type(_Datatype,_Value))):-
  option(literals(all), O, all), !.
% Untyped literals.
rdf_vertex_check(O, Literal1):-
  (
    Literal1 = literal(Literal2)
  ;
    Literal1 = literal(lang(_Language, Literal2))
  ), !,

  option(literals(IncludeLiterals), O, all),
  % No literal is allowed as vertex.
  IncludeLiterals \== none,

  (
    IncludeLiterals == preferred_label
  ->
    % Only preferred labels are allowed as vertices.
    option(language(Language), O, en),
    rdfs_preferred_label(_RDF_Term, Language, Literal2)
  ;
    % All literals are vertices.
    true
  ).
% Non-literal RDF terms.
% No restriction on RDF lists.
rdf_vertex_check(O, _V):-
  option(rdf_list(true), O, true), !.
% With setting `rdf_list=false` RDF terms should not be part of an RDF list.
rdf_vertex_check(O, V):-
  option(rdf_list(false), O, true),
  % The vertex must have some occurrence outside an RDF list.
  once((
    rdf(V, _, _)
  ;
    rdf(_, P, V),
    \+ rdf_memberchk(P, [rdf:first,rdf:rest])
  )).

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

