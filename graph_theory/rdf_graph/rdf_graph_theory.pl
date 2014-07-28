:- module(
  rdf_graph_theory,
  [
    rdf_directed_edge/2, % +Graph:atom
                         % ?DirectedEdge:compound
    rdf_graph_to_ugraph/2, % +Graph:atom
                           % -UG:ugraph
    rdf_neighbor/3, % +Graph:atom
                    % ?Vertex:vertex
                    % ?Neighbor:vertex
    rdf_neighbors/3, % +Graph:atom
                     % +Vertex:vertex
                     % -Neighbors:ordset(vertex)
    rdf_triples_to_edges/2, % +Triples:list(rdf_triple)
                            % -Edges:ordset(rdf_term)
    rdf_triples_to_vertices/2, % +Triples:list(rdf_triple)
                               % -Vertices:ordset(rdf_term)
    rdf_undirected_edge/2, % +Graph:atom
                           % ?UndirectedEdge:compound
    rdf_vertex/2, % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex/3, % +Options:list(nvpair)
                  % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex_equivalence/2 % +Resource1:uri
                             % +Resource2:uri
  ]
).

/** <module> RDF graph theory

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
 edges (as defined by RDF abstract syntax) in one triple can be nodes in
 another.
This means that the definitions 'edge' and 'vertex' for graph theoretic
 operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03, 2013/08, 2014/03, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declaration.
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_read)).
:- use_module(plRdf_term(rdf_language_tagged_string)).
:- use_module(plRdf_term(rdf_literal)).

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_neighbor(+,r,r)).
:- rdf_meta(rdf_neighbors(+,r,-)).
:- rdf_meta(rdf_vertex(+,r)).
:- rdf_meta(rdf_vertex(+,+,r)).
:- rdf_meta(rdf_vertex_equivalence(r,r)).

:- predicate_options(rdf_vertex/3, 3, [
     literals(+oneog([all,none,preferred_label])),
     rdf_list(+boolean)
   ]).



rdf_bnode_to_var(S, _):-
  rdf_is_bnode(S), !.
rdf_bnode_to_var(X, X).


%! rdf_directed_edge(+Graph:atom, +DirectedEdge:compound) is semidet.
%! rdf_directed_edge(+Graph:atom, -DirectedEdge:compound) is det.

rdf_directed_edge(Graph, FromV-P-ToV):-
  rdf(FromV, P, ToV, Graph).


%! rdf_graph_to_ugraph(+Graph:atom, -UGraph:compound) is det.
% Returns the UG representation of a loaded RDF graph.
%
% @arg G The atomic name of a loaded RDF graph.
% @arg UG:ugraph A UG datastructure.

rdf_graph_to_ugraph(Graph, UGraph):-
  aggregate_all(
    set(From-Neighbors),
    (
      rdf_vertex(Graph, From),
      aggregate_all(
        set(To),
        rdf_undirected_edge(Graph, From-_-To),
        Neighbors
      )
    ),
    UGraph
  ).


%! rdf_neighbor(+Graph:atom, ?Vertex:vertex, ?Neighbor:vertex) is nondet.
% Neighboring vertices.

rdf_neighbor(Graph, V1, V2):-
  rdf(V1, _, V2, Graph).
  rdf_edge(Graph, V1-V2).

rdf_neighbors(G, V, Ns):-
  aggregate_all(
    set(N),
    rdf_neighbor(G, V, N),
    Ns
  ).


rdf_triples_to_edges(Ts, Es):-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_,ToV), Ts),
    Es
  ).


rdf_triples_to_vertices(Ts, Vs):-
  aggregate_all(
    set(V),
    (
      member(rdf(V1,_,V2), Ts),
      (
        V = V1
      ;
        V = V2
      )
    ),
    Vs
  ).

%! rdf_undirected_edge(+Graph:atom, +UndirectedEdge:edge) is semidet.
%! rdf_undirected_edge(+Graph:atom, -UndirectedEdge:edge) is nondet.

rdf_undirected_edge(Graph, FromV-P-ToV):-
  rdf(FromV, P, ToV, Graph).
rdf_undirected_edge(Graph, FromV-P-ToV):-
  rdf(ToV, P, FromV, Graph).


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

rdf_vertex(IncludeLiterals, G, V):-
  (
    rdf(V, _, _, G)
  ;
    rdf(_, V, _, G)
  ;
    rdf(_, _, V, G),
    rdf_vertex_check(IncludeLiterals, V)
  ),


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

