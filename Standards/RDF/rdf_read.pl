:- module(
  rdf_read,
  [
    rdf_datatype/2, % ?Graph:graph
                    % ?Datatype:uri
    rdf_datatype/5, % ?Subject:oneof([bnode,uri])
                    % ?Predicate:uri
                    % ?Datatype:oneof([atom,uri])
                    % ?Value:atomic
                    % ?Graph:graph
    rdf_find/4, % +Subject:or([bnode,iri]),
                % +Predicate:iri,
                % +Object:or([bnode,iri,literal]),
                % +Graph:atom
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
    rdf_member/2, % ?Member:uri
                  % ?Members:list(uri)
    rdf_memberchk/2, % ?Member:uri
                     % ?Members:list(uri)
    rdf_preferred_literal/5, % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % +LanguageTags:or([atom,list(atom)])
                             % ?PreferredLangTag:atom
                             % ?PreferredLiteral:atom
    rdf_property/2 % +Graph:atom
                   % ?Property:iri
  ]
).

/** <module> RDF read

Predicates for reading from RDF, customized for specific datatypes and
literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04, 2013/07-2013/09
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_has_datatype(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?)).
:- rdf_meta(rdf_member(r,+)).
:- rdf_meta(rdf_memberchk(r,+)).
:- rdf_meta(rdf_preferred_literal(r,r,+,-,?)).
:- rdf_meta(rdf_property(+,r)).



%! rdf_bnode_to_var(
%!   +RDF_Term:or([bnode,iri,literal]),
%!   ?Out:or([iri,literal])
%! ) is det.
% Replaced blank nodes with uninstantiated variables.

rdf_bnode_to_var(X, _):-
  rdf_is_bnode(X), !.
rdf_bnode_to_var(X, X).

%! rdf_both_bnode(
%!   +RDF_Term1:or([bnode,iri,literal]),
%!   +RDF_Term2:or([bnode,iri,literal])
%! ) is semidet.
% Fails if only either of the RDF terms is a blank node.

rdf_both_bnode(X, Y):-
  rdf_is_bnode(X), !,
  rdf_is_bnode(Y).
rdf_both_bnode(_, _).

rdf_datatype(G, Datatype):-
  rdf(_S, _P, literal(type(Datatype, _LitVal)), G).

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

%! rdf_find(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is semidet.
% Finds an RDF triple according to an RDF triple.
% This is different from rdf/[3,4], which are not RDF triples
% (since their parameters may hold argments that are variables).
%
% Since we cannot match blank nodes directly, we replace them with variables.
% This is valid under graph equivalence.

rdf_find(S, P, O, G):-
  maplist(rdf_bnode_to_var, [S,P,O], [SS,PP,OO]),
  rdf(SS, PP, OO, G),
  maplist(rdf_both_bnode, [S,P,O], [SS,PP,OO]).

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
%!   ?LanguageTags:or([atom,list(atom)]),
%!   ?PreferredLanguageTag:atom,
%!   ?PreferredLiteral:atom
%! ) is det.
% Look for the preferred languages, in order of occurrence in
% the given list of language subtags.
%
% @tbd Make sure the language subtags are standards compliant.

rdf_preferred_literal(S, P, LangTags, PreferredLangTag, PreferredLit):-
  % Accept lists of language tags as well as and single language tags.
  (
    is_list(LangTags), !,
    % Backtracking over membership ensures
    % that we try all given language tags.
    member(LangTag, LangTags)
  ;
    LangTag = LangTags
  ),

  % Takes both atoms and lists of atoms as argument.
  rdf_literal(S, P, LangTag, PreferredLit, _G),
  PreferredLangTag = LangTag, !.
% If the given language tag cannot be matched at all,
% then take an arbitrary literal.
rdf_preferred_literal(S, P, _LangTag, _NoPreferredLangTag, PreferredLit):-
  rdf_literal(S, P, PreferredLit, _G), !.

rdf_member(Member, List):-
  member(Member0, List),
  rdf_global_id(Member0, Member).

rdf_memberchk(Member, List):-
  once(rdf_member(Member, List)).

rdf_property(G, P):-
  rdf_term(G, P),
  rdfs_individual_of(P, rdf:'Property').

