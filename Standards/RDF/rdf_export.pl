:- module(
  rdf_export,
  [
    export_rdf_graph/4, % +Options
                        % :CoordFunc
                        % +RDF_Graph:atom
                        % -G_Term:compound
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_namespace_color/3, % +Graph:graph
                                    % +Namespace:atom
                                    % +Color:atom
    rdf_resource_naming/2, % +Resource:oneof([bnode,list,literal,uri])
                           % -Name:atom
    rdf_schema/2, % +Graph:atom
                  % -Triples:list(rdf_triple)
    rdf_vertex_naming/3 % +Options:list(nvpair)
                        % +RDF_Term:oneof([bnode,literal,uri])
                        % -Name:atom
  ]
).

/** <module> RDF export

Predicates for exporting RDF.

# Edge naming

Specific edge labels can be hidden by adding clauses to
rdf_edge_label_replace/2.

# Edge styling

# Vertex coloring

The procedure for determining the color of a vertex:
    1. Look whether the =colorscheme= option is not set to =none=.
    2. See whether the vertex is an individual of a colored class.
    3. See whether the vertex belongs to a colored namespace.
    4. If at least one vertex is not colored by class or namespace, then all
       namespaces are (re)assigned colors.

# Vertex naming

@author Wouter Beek
@version 2013/01-2013/03, 2013/07
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(svg(svg)).

:- dynamic(class_color(_Graph, _Class, _Color)).
:- dynamic(namespace_color(_Graph, _Namespace, _Color)).
:- dynamic(rdf_edge_style(_Predicate, _Style)).

:- rdf_meta(rdf_edge_label_replace(r,r)).
:- rdf_meta(rdf_edge_style(r,?)).
:- rdf_meta(rdf_register_class_color(+,r,+)).



%! colorize_namespaces(+Graph:atom, +ColorScheme:atom) is det.
% Uses colors from the given colorscheme to colorize the namespaces in the
% given graph.
%
% @tbd Throw exception for unknown colorscheme.

colorize_namespaces(Graph, _ColorScheme):-
  \+ rdf_graph(Graph), !,
  existence_error(atom, Graph).
colorize_namespaces(Graph, svg):- !,
  rdf_current_namespaces(Graph, Namespaces),
  length(Namespaces, N),
  N > 0,
  svg_colors(Colors),
  length(Colors, M),
  Delta is M // N,
  forall(
    nth1(I, Namespaces, Namespace),
    (
      % In case there are more namespaces than colors, Delta=1 and we use
      % the same color for all namespaces with index I mod M.
      J is (I * Delta) mod M,
      % J can be 0 becasue of the modulus function, so do not use nth1/3.
      nth0(J, Colors, Color),
      assert(namespace_color(Graph, Namespace, Color))
    )
  ).
colorize_namespaces(_Graph, ColorScheme):-
  existence_error(atom, ColorScheme).

export_rdf_graph(
  Options,
  CoordFunc,
  RDF_Graph,
  graph(V_Terms, [], E_Terms, Attrs)
):-
  

rdf_edge_coloring(Options, _Edge, black):-
  option(colorscheme(none), Options, none), !.
rdf_edge_coloring(Options, FromVertex-ToVertex, Color):-
  rdf_vertex_coloring(Options, FromVertex, FromColor),
  rdf_vertex_coloring(Options, ToVertex, ToColor),
  !,
  % Notive that color must be uninstantiated in rdf_vertex_coloring/3.
  FromColor = ToColor,
  Color = FromColor.
rdf_edge_coloring(Options, FromVertex-ToVertex, Color):-
  option(graph(Graph), Options),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  rdf_vertex_coloring(Options, Predicate, Color).

%! rdf_edge_naming(+Options:list(nvpair), +Edge:edge, -Name:atom) is det.
% Returns a name for the given edge.
%
% @arg Options A list of name-value pairs. The following options are
%        supported:
%            1. =|edge_labels(oneof([all,replace]))|=

rdf_edge_naming(Options, FromVertex-ToVertex, Name):-
  option(in(rdf), Options), !,
  option(graph(Graph), Options, user),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  % Some edge labels are not displayed.
  % This concerns RDF(S) terminology.
  (
    option(edge_labels(replace), Options),
    rdf_edge_label_replace(Predicate, Name)
  ->
    true
  ;
    rdf_vertex_naming(Options, Predicate, Name)
  ).

rdf_edge_label_replace(rdf:type,           '').
rdf_edge_label_replace(rdfs:label,         '').
rdf_edge_label_replace(rdfs:subClassOf,    '').
rdf_edge_label_replace(rdfs:subPropertyOf, '').

rdf_edge_style1(Predicate, Style):-
  rdf_edge_style(Predicate, Style).
rdf_edge_style1(_OtherPredicate, solid/normal).

% These values come from the GraphViz attributes =arrowhead= and =style=.
rdf_edge_style(rdf:type,           solid/empty).
rdf_edge_style(rdfs:label,         dotted/none).
rdf_edge_style(rdfs:subClassOf,    solid/box).
rdf_edge_style(rdfs:subPropertyOf, solid/diamond).

rdf_edge_styling(Options, FromVertex-ToVertex, Style):-
  option(graph(Graph), Options, user),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  rdf_edge_style1(Predicate, Style),
  !.

%! rdf_graph_naming(+Options:list(nvpair), -Name:atom) is det.
% Returns a name for the given graph.
%
% @arg Options A list of name-value paris.
%        1. =graph(Graph:atom)= The atomic name of a graph.
% @arg Graph The atomic name of a graph.
% @arg Name An atomic name for the given graph.

rdf_graph_naming(Options, GraphName):-
  option(graph(GraphName), Options).

rdf_register_class_color(Graph, Class, Color):-
  assertz(class_color(Graph, Class, Color)).

rdf_register_namespace_color(Graph, Namespace, Color):-
  assertz(namespace_color(Graph, Namespace, Color)).

rdf_resource_naming(List, Name):-
  is_list(List), !,
  maplist(rdf_resource_naming, List, Names),
  atomic_list_concat(Names, ',', NamesAtom),
  format(atom(Name), '{~w}', [NamesAtom]).
rdf_resource_naming(literal(type(Datatype, Value)), Name):-
  rdf_global_id(DatatypeNamespace:DatatypeLocal, Datatype),
  format(atom(Name), '"~w"^^~w:~w', [Value,DatatypeNamespace,DatatypeLocal]),
  !.
rdf_resource_naming(literal(lang(Language, Literal)), Name):-
  format(atom(Name), '"~w"@~w', [Literal,Language]), !.
rdf_resource_naming(literal(Literal), Name):-
  format(atom(Name), '"~w"', [Literal]), !.
rdf_resource_naming(BNode, Name):-
  rdf_is_bnode(BNode), !,
  Name = BNode.
rdf_resource_naming(URI, Name):-
  rdf_global_id(Namespace:Local, URI), !,
  format(atom(Name), '~w:~w', [Namespace,Local]).
% We're out of options.
rdf_resource_naming(Name, Name).

rdf_schema(Graph, Triples):-
  setoff(
    Vertex,
    (
      (
        rdfs_individual_of(Vertex, rdfs:'Class')
      ;
        rdfs_individual_of(Vertex, rdf:'Property')
      ),
      rdf_vertex(Graph, Vertex)
    ),
    Vertices
  ),
  setoff(
    rdf(Subject, Predicate, Object),
    (
      member(Subject, Object, Vertices),
      rdf(Subject, Predicate, Object, Graph)
    ),
    Triples
  ).

rdf_triple_naming(S, P, O, TripleName):-
  maplist(rdf_resource_naming, [S,P,O], [S_Name,P_Name,O_Name]),
  format(atom(TripleName), '<~w,~w,~w>', [S_Name,P_Name,O_Name]).

%! rdf_vertex_color_by_namespace(
%!   +Graph:atom,
%!   +ColorScheme:atom,
%!   +Vertex,
%!   -Color:atom
%! ) is det.
% Returns the automatically assigned color name.
% The color names belong to the given colorscheme.
% The color assignments are based on the RDF node's namespace.
% Note that the same node may have different colors in different graphs.
%
% @arg Graph The atomic name of a graph.
% @arg ColorScheme The atomic name of a colorscheme. Currently supported:
%        1. =svg=
%        2. =x11=
% @arg Vertex A resource.
% @arg Color The atomic name of a color within the colorscheme.

rdf_vertex_color_by_namespace(Graph, _ColorScheme, Vertex, Color):-
  rdf_global_id(Namespace:_, Vertex),
  namespace_color(Graph, Namespace, Color),
  !.
rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color):-
  colorize_namespaces(Graph, ColorScheme),
  rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color).

%! rdf_vertex_coloring(
%!   +Options:list(nvpair),
%!   +Vertex:vertex,
%!   -Color:atom
%! ) is det.
% Returns a color name for the given vertex.
%
% @arg Options A list of name-value pairs.
%        1. =colorscheme(ColorScheme:oneof([none,svg,x11]))= The atomic name
%           of the color scheme from which the color names are drawn.
%        2. =graph(Graph:atom)= The atomic name of a graph.
% @arg Vertex A vertex.
% @arg Color The atomic name of a color for the given vertex.

rdf_vertex_coloring(Options, _Vertex, black):-
  option(colorscheme(none), Options), !.
% Literals.
rdf_vertex_coloring(_Options, literal(lang(_Language, _Label)), blue):- !.
rdf_vertex_coloring(_Options, literal(type(_Datatype, _Value)), blue):- !.
% Individual or subclass of a color-registered class.
rdf_vertex_coloring(Options, Vertex, Color):-
  option(graph(Graph), Options),
  (
    rdfs_individual_of(Vertex, Class)
  ;
    rdfs_subclass_of(Vertex, Class)
  ),
  class_color(Graph, Class, Color),
  !.
% Resource colored based on its namespace.
rdf_vertex_coloring(Options, Vertex, Color):-
  option(colorscheme(ColorScheme), Options, svg),
  option(graph(Graph), Options),
  (
    % URI resources with registered namespace/prefix.
    rdf_global_id(_:_, Vertex)
  ->
    rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color)
  ;
    % URI resources with unregistered namespace/prefix.
    is_uri(Vertex)
  ->
    Color = red
  ;
    % Non-URI resources, e.g., literals.
    Color = purple
  ).

% We prefer labels with the given language code.
vertex_label(RDF_Term, Language, Label):-
  rdfs_label(RDF_Term, Language, Label), !.
% If the preferred language is not available,
% then we look for an arbitrary other language.
vertex_label(RDF_Term, _PreferredLanguage, Label):-
  rdfs_label(RDF_Term, _OtherLanguage, Label).

%! rdf_vertex_naming(+Options:list(nvpair), +RDF_Term, -Name:atom) is det.
% Returns a display name for the given RDF graph vertex.
%
% @arg Options A list of name-value pairs.
%        1. =language(Language:atom)= The atomic tag of the language that is
%           preferred for vertex naming.
%           Defaults to =en=.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
% @arg RDF_Term An RDF term.
% @arg Name The atomic name of an RDF term.

% First we process lists of RDF vertices.
rdf_vertex_naming(Options, RDF_Terms, Name):-
  is_list(RDF_Terms), !,
  maplist(rdf_vertex_naming(Options), RDF_Terms, Names),
  print_list(atom(Name), Names).
% The RDF term is an RDF list.
rdf_vertex_naming(Options, RDF_Term, Name):-
  is_rdf_list(RDF_Term), !,
  % Recursively retrieve the contents of the RDF list.
  rdf_list(RDF_Term, RDF_Terms),
  maplist(rdf_vertex_naming(Options), RDF_Terms, Names),
  print_list(atom(Name), Names).
% The vertex is a label with a language tag.
rdf_vertex_naming(_Options, literal(lang(Language, Name0)), Name):- !,
  format(atom(Name), '~w@~w', [Name0, Language]).
% The vertex is a literal that has an XML Schema 2 datatype.
rdf_vertex_naming(_Options, literal(type(Datatype, CanonicalValue)), Name):-
  rdf_datatype(DatatypeName, _LexicalValue, Datatype, CanonicalValue), !,
  format(atom(Name), '~w^~w', [CanonicalValue, DatatypeName]).
% The RDF term has a label and is set to displaying labels as the only literals.
% Note that `labels_only` is the preferred option.
% If there is no label, then we use a name that is based on the URI of the
% RDF term.
rdf_vertex_naming(Options, RDF_Term, Name):-
  option(literals(labels_only), Options),
  option(language(Language, Options, en), !,
  vertex_label(RDF_Term, Language, Name).
% The RDF term is set to collate all literals that (directly) relate to it.
% Only do this when there is at least one literal.
rdf_vertex_naming(Options, RDF_Term, Name3):-
  option(language(Language), Options, en),
  
  % We first display the name.
  (
    % Use the label as name, if one is present.
    % Also prefer the given language.
    vertex_label(RDF_Term, Language, Name2)
  ->
    true
  ;
    % Otherwise, we use the namespace (if non-graph) name plus the local name.
    rdf_resource_to_namespace(RDF_Term, Namespace, Name1)
  ->
    atomic_list_concat([Namespace, Name1], ':', Name2)
  ;
    % If all else fails...
    term_to_atom(RDF_Term, Name2)
  ),
  
  % Now come the related literals, but only if these are set to be
  % collapsed into the (directly) related RDF term.
  (
    option(literals(collapse), Options, collapse)
  ->
    findall(
      Literal2,
      (
        rdf_has(RDF_Term, _Predicate, Literal1),
        rdf_is_literal(Literal1),
        rdf_vertex_naming(Options, Literal1, Literal2)
      ),
      Names1
    ),
    % In case there are no literals we use the name from the prior procedure.
    % Otherwise, use these literals.
    (
      Names1 == []
    ->
      Names2 = [Name2]
    ;
      Names2 = Names1
    )
  ;
    Names2 = [Name2]
  ),
  
  % Done!
  print_list(atom(Name3), Names2).

rdf_vertex_picturing(Graph, RDF_Term, Picture):-
  % Only display the first picutre that is related to this vertex.
  rdf_datatype(RDF_Term, _Predicate, image, Picture, Graph).

% Non-resource vertex (e.g. literals).
rdf_vertex_shaping(literal(_), [peripheries(0), shape(plaintext)]):- !.
% Class vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(2), shape(octagon)]):-
  rdfs_individual_of(RDF_Term, rdfs:'Class'), !.
% Property vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(hexagon)]):-
  rdfs_individual_of(RDF_Term, rdf:'Property'), !.
% Non-class and non-property resource vertex.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(ellipse)]):-
  rdfs_individual_of(RDF_Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_shaping(RDF_Term, [peripheries(1), shape(circle)]):-
  rdf_is_bnode(RDF_Term), !.
% Catch-all.
rdf_vertex_shaping(_RDF_Term, [peripheries(1), shape(ellipse)]):.

