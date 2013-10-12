:- module(
  rdf_lit_build,
  [
    rdf_assert_literal/4, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_literal/5, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LanguageTag:atom
                          % +Literal:atom
                          % +Graph:atom
    rdf_retractall_literal/4, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?Literal:atom
                              % ?Graph:atom
    rdf_retractall_literal/5 % ?Subject:oneof([bnode,iri])
                             % ?Predicate:iri
                             % ?Language:atom
                             % ?Literal:atom
                             % ?Graph:atom
  ]
).

/** <module> RDF literal build

Support for constructing/retracting RDF triples containing literals.

Building triples that contain typed literals is convered by
a dedicated module.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_assert_literal(r,r,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+,+)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?,?)).



%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a literal value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_assert_literal/5 also specifies the language.

rdf_assert_literal(S, P, Lit, G):-
  rdf_assert(S, P, literal(Lit), G).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +LanguageTag:atom,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a language-tagged literal value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param LanguageTag An atomic language tag.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_assert_literal(S, P, Lang, Lit, G):-
  nonvar(Lang), !,
  rdf_assert(S, P, literal(lang(Lang, Lit)), G).
rdf_assert_literal(S, P, _Lang, Lit, G):-
  rdf_assert_literal(S, P, Lit, G).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_retractall_literal/5 only retracts triples with literals of
%      a specific name.

rdf_retractall_literal(S, P, Literal, G):-
  rdf_retractall(S, P, literal(Literal), G).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?LanguageTag:atom,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property
% in a specific language.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param LanguageTag The atomic name of a language tag.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_retractall_literal(S, P, Lang, Lit, G):-
  rdf_retractall(S, P, literal(lang(Lang, Lit)), G).

