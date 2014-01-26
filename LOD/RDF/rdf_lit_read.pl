:- module(
  rdf_lit_read,
  [
    rdf_literal/4, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_literal/5, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Language:atom
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_preferred_literal/5 % ?Subject:or([bnode,iri])
                            % ?Predicate:iri
                            % +LanguageTags:or([atom,list(atom)])
                            % ?PreferredLangTag:atom
                            % ?PreferredLiteral:atom
  ]
).

/** <module> RDF literal read

Support for reading triples that contain simple and plain literals.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).



%! rdf_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Literal:atom,
%!   ?Graph:graph
%! ) is nondet.
% The RDF triple for a literal valued property.
%
% @see rdf_literal/5.

:- rdf_meta(rdf_literal(r,r,?,?)).
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
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Language The atomic name of a language.
% @arg Literal An atom.
% @arg Graph The atomic name of an RDF graph.

:- rdf_meta(rdf_literal(r,r,?,?,?)).
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

:- rdf_meta(rdf_preferred_literal(r,r,+,-,?)).
rdf_preferred_literal(S, P, LangTags, PreferredLangTag, PreferredLit):-
  % Accept lists of language tags as well as and single language tags.
  (
    is_list(LangTags),
    % Backtracking over membership ensures
    % that we try all given language tags.
    member(LangTag, LangTags)
  ;
    \+ is_list(LangTags),
    LangTag = LangTags
  ),

  % Takes both atoms and lists of atoms as argument.
  rdf_literal(S, P, LangTag, PreferredLit, _G),
  PreferredLangTag = LangTag, !.
% If the given language tag cannot be matched at all,
% then take an arbitrary literal.
rdf_preferred_literal(S, P, _LangTag, _NoPreferredLangTag, PreferredLit):-
  rdf_literal(S, P, PreferredLit, _G), !.

