:- module(
  graph_export,
  [
    export_vertex/5 % +Options:list(nvpair)
                    % +Graph
                    % :N_P
                    % +Vertex
                    % -G_term:compound
  ]
).

/** <module> GRAPH_EXPORT

Generic graph export module.

@author Wouter Beek
@version 2012/12-2013/04, 2013/07
*/

:- use_module(graph_theory(graph_generic)).
:- use_module(library(option)).
:- use_module(library(settings)).



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

