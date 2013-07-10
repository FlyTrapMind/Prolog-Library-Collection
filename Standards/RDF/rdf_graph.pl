:- module(
  rdf_graph,
  [
    is_rdf_graph_instance_of/3, % +GraphInstance:atom
                                % +Graph:atom
                                % -BNodeMap:list(list)
    is_rdf_graph_proper_instance_of/2, % +GraphInstance:atom
                                       % +Graph:atom
    rdf_bnode/2, % ?Graph:graph
                 % ?BNode:bnode
    rdf_graph_copy/2, % +From:atom
                      % +To:atom
    rdf_graph_equivalence/2, % +Graph1:atom
                             % +Graph2:atom
    rdf_graph_merge/2, % +In:list(atom)
                       % +MergedGraph:atom
    rdf_graph_source_file/2, % +Graph:atom
                             % -File:atom
    rdf_graph_triples/2, % ?Graph:atom
                         % ?Triples:list
    rdf_ground/1, % +Graph:atom
    rdf_name/2, % ?Graph:atom
                % +RDF_Name:oneof([literal,uri])
    rdf_new_graph/2, % +Graph1:atom
                     % -Graph2:atom
    rdf_node/2, % ?Graph:atom
                % ?Node:or([bnode,uri,literal])
    rdf_object/2, % ?Graph:graph
                  % ?Objects:oneof([bnode,literal,uri])
    rdf_pairs/2, % +Resource:uri
                 % -PredicateObjectPairs:list(pair)
    rdf_predicate/2, % ?Graph:atom
                     % ?Predicate:uri
    rdf_predicates/2, % +Graph:atom
                      % -Predicates:ordset(uri)
    rdf_term/2, % ?Graph:atom
                % ?RDF_Term:or([bnode,literal,uri])
    rdf_term_name/2, % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_term_name/3, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_schema/2, % +Graph:atom
                  % -Triples:ordset(rdf_triple)
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:oneof([bnode,uri])
    rdf_triple_name/4, % +S:oneof([bnode,uri])
                       % +P:uri
                       % +O:oneof([bnode,literal,uri])
                       % -T_Name:atom
    rdf_triples/2, % +In:oneof([atom,uri])
                   % -Triples:list(rdf_triple)
    rdf_triples_to_edges/2, % +Triples:list(rdf_triple)
                            % -Edges:ord_set(rdf_term)
    rdf_triples_to_vertices/2, % +Triples:list(rdf_triple)
                               % -Vertices:ord_set(rdf_term)
    rdf_vocabulary/2, % +Graph:atom
                      % -Vocabulary:ordset(oneof([uri,literal]))
    select_shared_properties/5, % +X_Pairs1:ordset(list)
                                % +Y_Pairs1:ordset(list)
                                % -SharedPropertyPairs:ordset(list)
                                % -X_Pairs2:ordset(list)
                                % -Y_Pairs2:ordset(list)
    select_shared_predicates/5 % +X_Pairs1:ordset(list)
                               % +Y_Pairs1:ordset(list)
                               % -SharedPredicateTuples:ordset(list)
                               % -X_Pairs2:ordset(list)
                               % -Y_Pairs2:ordset(list)
  ]
).

/** <module> RDF graph

Predicates that apply to entire RDF graphs.

Graph theory support for RDF is found in module [rdf_graph_theory].

For conversions from/to serialization formats, see module [rdf_serial].

# Abstract syntax

## RDF triple

An RDF triple contains three components:
    * *Subject*, either an RDF URI reference or a blank node.
    * *Predicate*, an RDF URI reference. Alternatively called _property_.
    * *Object*, either an RDF URI reference, a literal, or a blank node.

## RDF graph

A set of RDF triples.

## RDF node

The subject and objects of an RDF graph.

## RDF graph equivalence

RDF graphs G and G' are equivalent if there is a bijection M betweeen the sets
of nodes of G and G' such that:
    * M maps blank nodes to blank nodes.
    * M(lit)=lit for all RDF literals lit which are nodes of G.
    * M(uri)=uri for all RDF URI references uri which are nodes of G.
    * The triple (s,p,o) is in G if and only if the triple (M(s),p,M(o)) is
      in G'.

## RDF URI reference

A Unicode string encoded in UTF-8 (RFC 2279), without control characters
and producing a valid URI character sequence as defined by RFC 2396.

The encoded Unicode string consists of octets for US-ASCII characters and
%-escaping octets for non-US-ASCII characters.

Disallowed octets must be escaped by the URI escaping mechanism =|%HH|=,
where =HH= is a 2-digit hexadecimal numberal corresponding to the octect
value.

### RDF URI reference equivalence

RDF URI references are equivalent iff they compare as equal,
character by character, as Unicode strings.

## RDF literal

There are two types of literals:
    1. *|Plain literal|*s, that have (1) a lexical form and
       (2) an optional language tag (RFC 3066) that is normalized to
       lowercase.
    2. *|Typed literal|*s, that have (1) a lexical form and
       (2) a datatype URI that is an RDF URI reference.

A lexical form is a Unicode string in NFC (Unicode Normalization Form C).

### Literal equality

Two literals are equal iff:
    * The strings of the two lexical forms compare equal, character by
      character.
    * Either both or neither have language tags.
    * The language tags, if any, compare equal.
    * Either both or neither have datatype URIs.
    * The two datatype URIs, if any, compare equal, character by character.

## Blank nodes

The set of blank nodes, the set of literals, and the set of RDF URI references
are pairwise disjoint.

# TODO

## RDF graph equivalence

Any instance of a graph in which a blank node is mapped to a new blank
node not in the original graph is an instance of the original and also
has it as an instance, and this process can be iterated so that any 1:1
mapping between blank nodes defines an instance of a graph which has the
original graph as an instance. Two such graphs, each an instance of the
other but neither a proper instance, which differ only in the identity
of their blank nodes, are considered to be equivalent. We will treat
such equivalent graphs as identical; this allows us to ignore some
issues which arise from 're-naming' nodeIDs, and is in conformance
with the convention that blank nodes have no label. Equivalent graphs
are mutual instances with an invertible instance mapping.

## RDF graph leanness

An RDF graph is lean if it has no instance which is a proper subgraph of the
graph. Non-lean graphs have internal redundancy and express the same content
as their lean subgraphs.

@author Wouter Beek
@version 2012/01-2013/05, 2013/07
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_export)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_read)).

:- rdf_meta(rdf_bnode(?,r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_pairs(r,-)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_subject(?,r)).
:- rdf_meta(rdf_triples(r,-)).



rdf_bnode(Graph, BNode):-
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).

%! rdf_bnode_replace(+SharedBNodes:list, +Triple, -NewTriple) is det.
% Replaces shared bnodes in triples.

rdf_bnode_replace(SharedBNodes, rdf(S, P, O, Graph), rdf(NewS, P, NewO)):-
  maplist(rdf_bnode_replace(SharedBNodes, Graph), [S,O], [NewS, NewO]).

rdf_bnode_replace(SharedBNodes, G, R, NewR):-
  rdf_is_bnode(R),
  memberchk(_/G/R, SharedBNodes),
  !,
  rdf_bnode(NewR).
rdf_bnode_replace(_SharedBNodes, _G, R, R).

%! rdf_graph_copy(+From:atom, +To:atom) is det.
% Copying a graph is the same as merging a single graph
% and storing the result under a new name.

rdf_graph_copy(From, To):-
  From \== To,
  rdf_graph_merge([From], To).

%! rdf_graph_equivalence(+Graph1:atom, +Graph2:atom) is semidet.

rdf_graph_equivalence(Graph1, Graph2):-
  rdf_graph_equivalence0(Graph1, Graph2),
  rdf_graph_equivalence0(Graph2, Graph1).
rdf_graph_equivalence0(Graph1, Graph2):-
  forall(
    rdf(Subject1, Predicate, Object1, Graph1),
    (
      rdf(Subject2, Predicate, Object2, Graph2),
      rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2),
      rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2)
    )
  ).
rdf_graph_equivalence_subject0(_Graph1, Subject, _Graph2, Subject):-
  rdf_is_resource(Subject),
  !.
rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2):-
  bnode_translation0(Graph1, Subject1, Graph2, Subject2).
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_resource(Object),
  !.
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_literal(Object),
  !.
rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2):-
  bnode_translation0(Graph1, Object1, Graph2, Object2).
bnode_translation0(Graph1, Resource1, Graph2, Resource2):-
  rdf_bnode(Graph1, Resource1),
  rdf_bnode(Graph2, Resource2),
  !.

%! rdf_graph_merge(+In:list(atom), +MergedGraph:atom) is det.
% Merges RDF graphs.
% The input is is a (possibly mixed) list of RDF graph names and
% names of files that store an RDF graph.
%
% When merging RDF graphs we have to make sure that their blank nodes are
% standardized apart.

rdf_graph_merge(FilesOrGraphs, MergedGraph):-
  is_list(FilesOrGraphs),
  % Be liberal with respect to the input.
  files_or_rdf_graphs(FilesOrGraphs, Graphs),

  % Type checking.
  maplist(rdf_graph, Graphs),
  atom(MergedGraph),
  !,

  % Collect the shared blank nodes.
  findall(
    Graph1/Graph2/SharedBNode,
    (
      member(Graph1, Graph2, Graphs),
      % Use the natural order of atomic names.
      % The idea is that we only replace shared blank nodes in
      % the latter graph.
      Graph1 @< Graph2,
      rdf_bnode(Graph1, SharedBNode),
      rdf_bnode(Graph2, SharedBNode)
    ),
    SharedBNodes
  ),

  % Replace the blank nodes.
  (
    SharedBNodes == []
  ->
    forall(
      (
        member(Graph, Graphs),
        rdf(S, P, O, Graph)
      ),
      rdf_assert(S, P, O, MergedGraph)
    )
  ;
    forall(
      (
        member(Graph, Graphs),
        rdf(S, P, O, Graph)
      ),
      (
        rdf_bnode_replace(
          SharedBNodes,
          rdf(S, P, O, Graph),
          rdf(NewS, P, NewO)
        ),
        rdf_assert(NewS, P, NewO, MergedGraph)
      )
    )
  ).
rdf_graph(Graph, MergedGraph):-
  rdf_graph(Graph), !,
  rdf_graph([Graph], MergedGraph).

%! rdf_graph_source_file(+Graph:atom, -File:atom) is semidet.
% Returns the name of the file from which the graph with the given name
% was loaded.

rdf_graph_source_file(Graph, File):-
  rdf_graph_property(Graph, source(Source)),
  uri_components(
    Source,
    uri_components(file, _Authority, File, _Search, _Fragments)
  ).

% Instantiations (+,+)-semidet and (-,+)-nondet.
rdf_graph_triples(G, Triples):-
  is_list(Triples),
  !,
  rdf_graph(G),
  forall(
    member(rdf(S, P, O, G), Triples),
    rdf(S, P, O, G)
  ),
  \+ (rdf(S, P, O, G), \+ member(rdf(S, P, O, G), Triples)).
% Instantiation (+,-)-det.
rdf_graph_triples(G, Triples):-
  rdf_graph(G), !,
  findall(
    rdf(S, P, O, G),
    rdf(S, P, O, G),
    Triples
  ).

%! rdf_ground(+Graph:graph) is semidet.
% Succeeds if the given graph is ground, i.e., contains no blank node.
%! rdf_ground(+Triple) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.
% The predicate cannot be a blank node by definition.

rdf_ground(G):-
  rdf_graph(G), !,
  forall(
    rdf(S, P, O, G),
    rdf_ground(rdf(S, P, O))
  ).
rdf_ground(rdf(S, _P, O)):-
  \+ rdf_is_bnode(S),
  \+ rdf_is_bnode(O).

%! is_rdf_graph_instance_of(
%!   +GraphInstance:atom,
%!   +Graph:atom,
%!   -BNodeMap:list(list)
%! ) is nondet.
% # RDF graph instance
% Suppose that M is a mapping from a set of blank nodes to some set of
% literals, blank nodes and URI references; then any graph obtained from
% a graph G by replacing some or all of the blank nodes N in G by M(N)
% is an instance of G. Note that any graph is an instance of itself,
% an instance of an instance of G is an instance of G, and if H is
% an instance of G then every triple in H is an instance of some triple in G.

is_rdf_graph_instance_of(GI, G, BNodeMap):-
  maplist(rdf_graph, [GI,G]),
  rdf_graph_triples(G, Triples),
  rdf_is_graph_instance_of0(GI, Triples, [], BNodeMap),
  \+ (rdf(SI, P, OI, GI),
    \+ ((
      member(rdf(S, P, O, G), Triples),
      rdf_is_instance_of(rdf(SI, P, OI, GI), rdf(S, P, O, G), BNodeMap)
    ))
  ).
rdf_is_graph_instance_of0(_GI, [], BNodeMap, BNodeMap).
rdf_is_graph_instance_of0(GI, [rdf(S, P, O, G) | Triples], BNodeMap, Solution):-
  (rdf_is_instance_of(GI, SI, G, S, BNodeMap) ; NewMap1 = [[S,SI]]), !,
  (rdf_is_instance_of(GI, OI, G, O, BNodeMap) ; NewMap2 = [[O,OI]]), !,
  rdf(SI, P, OI, GI),
  append([NewMap1, NewMap2, BNodeMap], NewBNodeMap),
  rdf_is_graph_instance_of0(GI, Triples, NewBNodeMap, Solution).

%! is_rdf_graph_proper_instance_of(
%!   +GraphProperInstance:atom,
%!   +Graph:atom
%! ) is semidet.
% # RDF graph proper instance
% A proper instance of a graph is an instance in which a blank node
% has been replaced by a name, or two blank nodes in the graph have
% been mapped into the same node in the instance.

is_rdf_graph_proper_instance_of(GI, G):-
  is_rdf_graph_instance_of(GI, G, BNodeMap),
  (
    member([_BNode,Name], BNodeMap),
    rdf_name(GI, Name)
  ;
    member([BNode1,BNode], BNodeMap),
    member([BNode2,BNode], BNodeMap),
    BNode1 \== BNode2
  ),
  !.

%! rdf_is_instance_of(+TripleInstance, +Triple) is semidet.

rdf_is_instance_of(rdf(SI, P, OI, GI), rdf(S, P, O, G), BNodeMap):-
  rdf_is_instance_of(GI, SI, G, S, BNodeMap),
  rdf_is_instance_of(GI, OI, G, O, BNodeMap).

%! rdf_is_instance_of(
%!   +GraphInstance:atom,
%!   +ResourceInstance:oneof([bnode,literal,uri]),
%!   +Graph:atom,
%!   +Resource:oneof([bnode,literal,uri]),
%!   +BNodeMap:list(list)
%! ) is semidet.

rdf_is_instance_of(GI, R, _G, R, _BNodeMap):-
  rdf_name(GI, R), !.
rdf_is_instance_of(_GI, RI, _G, R, BNodeMap):-
  memberchk([R,RI], BNodeMap).

%! test_rdf_is_graph_instance_of(-Maps:list(list)) is det.
% Tests predicate is_rdf_graph_instance_of/2.
% Should return two lists of mappings from blank nodes to uriRefs.

test_rdf_is_graph_instance_of(Maps):-
  maplist(rdf_unload_graph, [g,gi]),
  rdf_bnode(X1), rdf_bnode(X2), rdf_bnode(X3), rdf_bnode(X4),
  rdf_assert(X1, rdf:p, X2, g), rdf_assert(X3, rdf:p, X4, g),
  rdf_assert(rdf:a, rdf:p, rdf:b, gi), rdf_assert(rdf:c, rdf:p, rdf:d, gi),
  findall(Map, is_rdf_graph_instance_of(gi, g, Map), Maps).

%! rdf_name(?G:atom, ?RDF_Name:oneof([literal,uri])) is nondet.
% Succeeds if the given object is an RDF name,
% i.e., an RDF URI reference or an RDF literal.

rdf_name(G, RDF_Name):-
  nonvar_det(rdf_name_(G, RDF_Name)).
rdf_name_(G, RDF_Name):-
  rdf_subject(G, RDF_Name),
  \+ rdf_is_bnode(RDF_Name).
rdf_name_(G, RDF_Name):-
  rdf_predicate(G, RDF_Name).
rdf_name_(G, RDF_Name):-
  rdf_object(G, Object),
  \+ rdf_is_bnode(Object),
  (
    Object = literal(type(Datatype, _LexicalValue))
  ->
    % Specifically include datatypes that are strictly speaking not RDF terms.
    (RDF_Name = Object ; RDF_Name = Datatype)
  ;
    RDF_Name = Object
  ).

%! rdf_new_graph(+Graph1:atom, -Graph2:atom) is det.

rdf_new_graph(Graph, Graph):-
  \+ rdf_graph(Graph), !.
rdf_new_graph(Graph1, Graph3):-
  split_atom_exclusive('_', Graph1, Splits),
  reverse(Splits, [LastSplit | RSplits]),
  (
    atom_number(LastSplit, OldNumber)
  ->
    NewNumber is OldNumber + 1,
    atom_number(NewLastSplit, NewNumber),
    reverse([NewLastSplit | RSplits], NewSplits)
  ;
    reverse(['1', LastSplit | RSplits], NewSplits)
  ),
  atomic_list_concat(NewSplits, '_', Graph2),
  rdf_new_graph(Graph2, Graph3).

rdf_node(Graph, Node):-
  nonvar_det(rdf_node0(Graph, Node)).
rdf_node0(Graph, Node):-
  rdf_subject(Graph, Node).
rdf_node0(Graph, Node):-
  rdf_object(Graph, Node).

rdf_object(G, O):-
  nonvar_det(rdf_object0(G, O)).
rdf_object0(G, O):-
  rdf(_, _, O, G).

rdf_pairs(Resource, PredicateObjectPairs):-
  is_uri(Resource),
  !,
  setoff(
    [P, O],
    rdf(Resource, P, O, _Graph),
    PredicateObjectPairs
  ).

rdf_predicate(G, P):-
  nonvar_det(rdf_predicate0(G, P)).
rdf_predicate0(G, P):-
  rdf(_, P, _, G).

rdf_predicates(Graph, Predicates):-
  setoff(
    Predicate,
    rdf_predicate(Graph, Predicate),
    Predicates
  ).

%! rdf_term(?Graph:graph, ?Term:uri) is nondet.
% Pairs of graphs and terms that occur in that graph.
% A term is either a subject, predicate or object term
% in an RDF triple.
%
% @arg Graph The atomic name of a graph.
% @arg Term A resource.

rdf_term(Graph, Term):-
  rdf_node(Graph, Term).
rdf_term(Graph, Term):-
  rdf_predicate(Graph, Term).

rdf_term_name(RDF_Term, Name):-
  rdf_term_name([], RDF_Term, Name).

%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +RDF_Term:oneof([bnode,literal,uri]),
%!   -Name:atom
%!) is det.
% Returns a display name for the given RDF term.
%
% @arg Options The following options are supported:
%       1. `language(+Language:atom)`
%          The atomic language tag of the language that is preferred for
%          use in the RDF term's name.
%          The default value is `en`.
%       2. `literals(+DisplayLiterals:oneof([collapse,labels_only]))`
%          Whether or not literals are included in the name of the RDF term.
%          The default value is `collapse`.
% @arg RDF_Term An RDF term.
% @arg Name The atomic name of an RDF term.

% This method also works for a list of RDF terms.
% @tbd What's the use case for this?
rdf_term_name(O, RDF_Terms, Name):-
  is_list(RDF_Terms), !,
  maplist(rdf_term_name(O), RDF_Terms, Names),
  print_set(atom(Name), Names).
% An RDF list.
rdf_term_name(O, RDF_Term, Name):-
  is_rdf_list(RDF_Term), !,
  % Recursively retrieve the contents of the RDF list.
  rdf_list(RDF_Term, RDF_Terms),
  maplist(rdf_term_name(O), RDF_Terms, Names),
  print_list(atom(Name), Names).
% A literal with a datatype.
rdf_term_name(_O, literal(type(Datatype,CanonicalValue)), Name):- !,
  (
    % Model RDF_DATATYPE supports this datatype.
    rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue)
  ->
    format(atom(Name), '"~w"^^~w', [LexicalValue,DatatypeName])
  ;
    % The datatype has a registered namespace prefix.
    rdf_global_id(DatatypeNamespace:DatatypeLocal, Datatype)
  ->
    format(atom(Name), '"~w"^^~w:~w', [Value,DatatypeNamespace,DatatypeLocal])
  ;
    % Not all datatypes have their namespace defined.
    format(atom(Name), '"~w"^^~w', [Value,Datatype])
  ).
% A plain literal with a language tag.
rdf_term_name(_O, literal(lang(Language,Literal)), Name):- !,
  format(atom(Name), '"~w"@~w', [Literal,Language]).
% A simple literal / a plain literal without a language tag.
rdf_term_name(_O, literal(Literal), Name):- !,
  format(atom(Name), '"~w"', [Literal]).
% A blank node.
% @tbd Make this less implementation-dependent, e.g. by mapping
%      internal blank nodes to integers.
rdf_term_name(_O, BNode, Name):-
  rdf_is_bnode(BNode), !,
  Name = BNode.
% Now come the various URIs...
% The RDF term has a label and is set to displaying labels
% as the only literals.
% If there is no label, then we use a name that is based on the URI
% of the RDF term.
rdf_term_name(O, RDF_Term, Name):-
  option(literals(labels_only), O, collapse),
  option(language(Lang), O, en), !,
  rdfs_preferred_label(RDF_Term, Lang, Name).
% The RDF term is set to collate all literals that (directly) relate to it.
% Only do this when there is at least one literal that can be collated.
rdf_term_name(O, RDF_Term, Name3):-
  % First display the name.
  (
    % Use the label as the name, if it is present.
    % Also, prefer the given language.
    option(language(Lang), O, en),
    rdfs_preferred_label(RDF_Term, Lang, Name1)
  ->
    % Quite a useless step, I admit.
    Name2 = Name1
  ;
    % Otherwise, compase a name out of the namespace prefix
    % and the local name.
    rdf_resource_to_namespace(RDF_Term, Namespace, Name1)
  ->
    atomic_list_concat([Namespace, Name1], ':', Name2)
  ;
    % If all else fails...
    term_to_atom(RDF_Term, Name2)
  ),

  % Now come the related literals, but only if these are set to be
  % collapsed into the (directly) related RDF term.
  % Note that this is not the default, but has to be set explicitly
  % in the options.
  (
    option(literals(collapse), O, collapse)
  ->
    findall(
      LitName,
      (
        rdf_has(RDF_Term, _P, Lit),
        rdf_is_literal(Lit),
        rdf_term_name(O, Lit, LitName)
      ),
      Names1
    ),
    % In case there are no literals we use the name from the prior procedure.
    % Otherwise, use these literals.
    Names1 \== []
  ->
    Names2 = Names1
  ;
    Names2 = [Name2]
  ),

  % Done!
  print_set(atom(Name3), Names2).
% URI reference.
rdf_term_name(_O, URI, Name):- !,
  (
    rdf_global_id(Namespace:Local, URI)
  ->
    format(atom(Name), '~w:~w', [Namespace,Local])
  ;
    % Not all URIs have a registered namespace prefix.
    Name = URI
  ).
% We're out of options here...
rdf_term_name(_O, Name, Name).



rdf_schema(G, Ts):-
  setoff(
    V,
    (
      (
        rdfs_individual_of(V, rdfs:'Class')
      ;
        rdfs_individual_of(V, rdf:'Property')
      ),
      rdf_vertex(G, V)
    ),
    Vs
  ),
  setoff(
    rdf(S, P, O),
    (
      member(S, O, Vs),
      rdf(S, P, O, G)
    ),
    Ts
  ).

rdf_subject(G, S):-
  nonvar_det(rdf_subject_(G, S)).
rdf_subject_(G, S):-
  rdf(S, _P, _O, G).

rdf_triple_name(S, P, O, T_Name):-
  maplist(rdf_term_name, [S,P,O], [S_Name,P_Name,O_Name]),
  format(atom(T_Name), '<~w,~w,~w>', [S_Name,P_Name,O_Name]).

%! rdf_triples(+In:oneof([atom,uri]) -Triples:list(rdf_triple)) is det.
% Returns an unsorted list containing all the triples in a graph.
%
% @arg In The atomic name of a loaded RDF graph, or a URI.
% @arg Triples A list of triple compound term.

rdf_triples(G, Ts):-
  rdf_graph(G), !,
  findall(
    rdf(S, P, O),
    rdf(S, P, O, G),
    Ts
  ).
% The RDF triples that describe a given URI reference.
rdf_triples(URI, Ts):-
  is_uri(URI), !,
  setoff(
    rdf(URI, P, O),
    rdf(URI, P, O, _G),
    Ts
  ).

rdf_triples_to_edges(Ts, Es):-
  setoff(FromV-ToV, member(rdf(FromV, _P, ToV), Ts), Es).

rdf_triples_to_vertices(Ts, Vs):-
  setoff(
    V,
    (
      member(rdf(V1, _P, V2), Ts),
      (V = V1 ; V = V2)
    ),
    Vs
  ).

rdf_vocabulary(G, Vocabulary):-
  setoff(
    RDF_Name,
    rdf_name(G, RDF_Name),
    Vocabulary
  ).

select_shared_properties(
  X_Pairs1,
  Y_Pairs1,
  SharedPropertyPairs,
  X_Pairs2,
  Y_Pairs2
):-
  ord_intersection(X_Pairs1, Y_Pairs1, SharedPropertyPairs, Y_Pairs2),
  ord_subtract(X_Pairs1, SharedPropertyPairs, X_Pairs2).

select_shared_predicates(
  X_Pairs1,
  Y_Pairs1,
  SharedPredicateTriples,
  X_Pairs2,
  Y_Pairs2
):-
  setoff(
    [Predicate, X_Object, Y_Object],
    (
      member(Predicate-X_Object, X_Pairs1),
      member(Predicate-Y_Object, Y_Pairs1)
    ),
    SharedPredicateTriples
  ),
  ord_subtract(X_Pairs1, SharedPredicateTriples, X_Pairs2),
  ord_subtract(Y_Pairs1, SharedPredicateTriples, Y_Pairs2).

