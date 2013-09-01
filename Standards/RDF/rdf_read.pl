:- module(
  rdf_read,
  [
% LITERALS
    rdf_datatype/5, % ?Subject:oneof([bnode,uri])
                    % ?Predicate:uri
                    % ?Datatype:oneof([atom,uri])
                    % ?Value:atomic
                    % ?Graph:graph
    rdf_has_datatype/4, % ?Subject:oneof([bnode,uri])
                        % ?Predicate:uri
                        % ?DatatypeName:atom
                        % ?Value
    rdf_literal/4, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_literal/5, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Language:atom
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_preferred_literal/5, % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % +LanguageTag:atom
                             % ?PreferredLangTag:atom
                             % ?PreferredLiteral:atom
% LIST MEMBERSHIP
    rdf_member/2, % ?Member:uri
                  % ?Members:list(uri)
    rdf_memberchk/2, % ?Member:uri
                     % ?Members:list(uri)
% PROPERTY
    rdf_property/2, % +Graph:atom
                    % ?Property:iri
% STRUCTURE-BASED READS
    rdf_index/5, % ?Subject:oneof([bnode,uri])
                 % ?Predicate:uri
                 % ?Object:uri
                 % ?Graph:graph
                 % ?Index:term
    rdf_random_term/2, % +Graph:atom
                       % -Term:or([bnode,literal,iri])
    rdf_random_term/3, % +Graph:atom
                       % :Requirement
                       % -Term:or([bnode,literal,iri])
    rdf_valuetype/2 % ?Graph:graph
                    % ?Type:uri
  ]
).

/** <module> RDF read

Predicates for reading from RDF, customized for specific datatypes and
literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04, 2013/07-2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- meta_predicate(rdf_random_term(+,1,-)).

% LITERALS %
:- rdf_meta(rdf_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_has_datatype(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?)).
:- rdf_meta(rdf_preferred_literal(r,r,+,-,?)).
% LIST MEMBERSHIP %
:- rdf_meta(rdf_member(r,+)).
:- rdf_meta(rdf_memberchk(r,+)).
% PROPERTY
:- rdf_meta(rdf_property(+,r)).
% STRUCTURE-BASED READS %
:- rdf_meta(rdf_index(r,r,r,?,?)).
:- rdf_meta(rdf_node(?,r)).
:- rdf_meta(rdf_random_term(+,r)).
:- rdf_meta(rdf_random_term(+,:,r)).
:- rdf_meta(rdf_random_triple(r,r,r,?)).
:- rdf_meta(rdf_term(?,r)).
:- rdf_meta(rdf_valuetype(?,r)).



% LITERALS %

%! rdf_datatype(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?DatatypeName:atom,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.
% @tbd Implement the inverse lexical map to fascilitate search (besides read and write).

rdf_datatype(Subject, Predicate, DatatypeName, Value, Graph):-
  xsd_datatype(DatatypeName, Datatype),
  % Ideally, we would like to interpret all literals, not just the canonical ones.
  % Unfortunately the instantiation pattern for xsd_lexicalMap/3 does not allow this.
  % Interpreting literals could be useful for search, i.e. does a specific value
  % from the value space of the given datatype occur in the currently loaded RDF graph?
  % For this one needs the inverse of the lexical map.
  % In the absence of this inverse lexical map, we have to look for a lexical map
  % of a datatype literal that matches value (this is not so bad as it seems,
  % if subject, predicate, datatype, and graph are specified).
  rdf(Subject, Predicate, literal(type(Datatype, LEX)), Graph),
  % This may be nondet!
  xsd_lexicalMap(Datatype, LEX, Value).

%! rdf_has_datatype(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?DatatypeName:atom,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.

rdf_has_datatype(Subject, Predicate, DatatypeName, Value):-
  xsd_datatype(DatatypeName, Datatype),
  (
    nonvar(Value)
  ->
    % Interpret all literals, not just the canonical ones.
    xsd_lexicalMap(Datatype, LEX, Value),
    rdf_has(Subject, Predicate, literal(type(Datatype, LEX)))
  ;
    rdf_has(Subject, Predicate, literal(type(Datatype, LEX))),
    % This may be nondet!
    xsd_lexicalMap(Datatype, LEX, Value)
  ).

%! rdf_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Literal:atom,
%!   ?Graph:graph
%! ) is nondet.
% The RDF triple for a literal valued property.
%
% @see rdf_literal/5.

rdf_literal(S, P, Lit, G):-
  rdf(S, P, literal(Lit), G).
rdf_literal(S, P, Lit, G):-
  rdf_literal(S, P, _Lang, Lit, G).

%! rdf_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?LanguageTag:atom,
%!   ?Literal:atom,
%!   ?Graph:graph
%! ) is nondet.
% The RDF triple for a literal valued property, encoded in a certain language.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_literal(S, P, LangTag, Lit, G):-
  rdf(S, P, literal(lang(LangTag, Lit)), G).

%! rdf_preferred_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LanguageTag:atom,
%!   ?PreferredLanguageTag:atom,
%!   ?PreferredLiteral:atom
%! ) is det.
% Look for the preferred languages, in order of occurrence in
% the given list of language subtags.
%
% @tbd Make sure the language subtags are standards compliant.

rdf_preferred_literal(S, P, LangTag, PreferredLangTag, PreferredLit):-
  % Takes both atoms and lists of atoms as argument.
  rdf_literal(S, P, LangTag, PreferredLit, _G),
  PreferredLangTag = LangTag, !.
% If the given language tag cannot be matched at all,
% then take an arbitrary literal.
rdf_preferred_literal(S, P, _LangTag, _NoPreferredLangTag, PreferredLit):-
  rdf_literal(S, P, PreferredLit, _G), !.


% LIST MEMBERSHIP %

rdf_member(Member, List):-
  member(Member0, List),
  rdf_global_id(Member0, Member).

rdf_memberchk(Member, List):-
  once(rdf_member(Member, List)).



% PROPERTY %

rdf_property(G, P):-
  rdf_term(G, P),
  rdfs_individual_of(P, rdf:'Property').



% STRUCTURE-BASED READS %

%! rdf_index(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:uri,
%!   ?Graph:graph,
%!   ?Index:integer
%! ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Object A resource.
% @param Index A compound term of the form =Graph:Line= with =Graph= the
%        atomic name of an RDF graph and =Line= an integer.

rdf_index(Subject, Predicate, Object, Graph, Index):-
  rdf_graph:rdf_graph_to_triples(Graph, Triples),
  nth0(Index, Triples, rdf(Subject, Predicate, Object)).

rdf_random_term(G, T):-
  rdf_random_term(G, rdf_term(G), T).

rdf_random_term(G, Requirement, T2):-
  rdf_random_triple(S, P, O, G),
  random_betwixt(2, J),
  nth0(J, [S,P,O], T1),
  (
    call(Requirement, T1)
  ->
    T2 = T1
  ;
    rdf_random_term(G, Requirement, T2)
  ).

%! rdf_random_triple(
%!   -Subject:oneof([bnode,uri]),
%!   -Predicate:uri,
%!   -Object:uri,
%!   ?Graph:graph
%! ) is det.
% Returns a random triple from the given graph.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Object A resource.
% @param Graph The atomic name of a graph.

rdf_random_triple(Subject, Predicate, Object, Graph):-
  rdf_graph_property(Graph, triples(NumberOfTriples)),
  succ(UpperIndex, NumberOfTriples),
  random_betwixt(UpperIndex, RandomIndex),
  rdf_index(Subject, Predicate, Object, Graph, RandomIndex).

rdf_valuetype(Graph, Type):-
  rdf(_Subject, _Predicate, literal(type(Type, _Value)), Graph).

