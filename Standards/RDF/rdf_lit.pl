:- module(
  rdf_lit,
  [
    rdf_is_plain_literal/1, % ?PlainLiteral:compound
    rdf_is_simple_literal/1, % ?SimpleLiteral:compound
    rdf_is_typed_literal/1, % ?TypedLiteral:compound
    rdf_literal/4, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_literal/5, % ?Subject:oneof([bnode,uri])
                   % ?Predicate:uri
                   % ?Language:atom
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_literal_equality/2, % +Literal1:literal
                            % +Literal2:literal
    rdf_plain_literal/2, % ?Graph:atom
                         % ?PlainLiteral:compound
    rdf_preferred_literal/5, % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % +LanguageTags:or([atom,list(atom)])
                             % ?PreferredLangTag:atom
                             % ?PreferredLiteral:atom
    rdf_simple_literal/2, % ?Graph:atom
                          % ?SimpleLiteral:compound
    rdf_typed_literal/4 % ?Graph:atom
                        % ?TypedLiteral:compound
                        % ?Datatype:iri
                        % ?Value
  ]
).

/** <module> RDF literals

Support for RDF literals.

@author Wouter Beek
@version 2013/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_literal(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?)).
:- rdf_meta(rdf_preferred_literal(r,r,+,-,?)).
:- rdf_meta(rdf_typed_literal(?,o,r,?)).



%! rdf_is_plain_literal(+X) is semidet.
%! rdf_is_simple_literal(+X) is semidet.
%! rdf_is_typed_literal(+X) is semidet.
% Notice that these are complete (succeeds for every plain/simple/typed
% literal, but not sound (succeeds for non-literals.

rdf_is_plain_literal(literal(lang(_,_))).

rdf_is_simple_literal(literal(_)).

rdf_is_typed_literal(literal(type(_,_))).

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

%! rdf_literal_equality(+Literal1:literal, +Literal2:literal) is semidet.
% Succeeds if the given literals are equivalent.
%
% Two literals are equivalent if:
%   1. The strings of the two lexical forms compare equal,
%      character by character.
%   2. Either both or neither have language tags.
%   3. The language tags, if any, compare equal.
%   4. Either both or neither have datatype URIs.
%   5. The two datatype URIs, if any, compare equal, character by character.
%
% @see Resource Description Framework (RDF): Concepts and Abstract Syntax
%      http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/

rdf_literal_equality(
  literal(lang(Lang1,Lit1)),
  literal(lang(Lang2,Lit2))
):- !,
  Lang1 == Lang2,
  Lit1 == Lit2.
rdf_literal_equality(
  literal(type(Type1,LEX1)),
  literal(type(Type2,LEX2))
):- !,
  Type1 == Type2,
  xsd_lexicalMap(Type1, LEX1, Value1),
  xsd_canonicalMap(Type1, Value1, CAN1),
  xsd_lexicalMap(Type2, LEX2, Value2),
  xsd_canonicalMap(Type2, Value2, CAN2),
  CAN1 == CAN2.
rdf_literal_equality(literal(Lit1), literal(Lit2)):- !,
  Lit1 == Lit2.

%! rdf_plain_literal(?Graph:atom, ?PlainLiteral:compound) is nondet.

rdf_plain_literal(G, Lit):-
  % rdf/[3,4] throws an exception for numeric input.
  \+ number(Lit),
  rdf(_, _, Lit, G),
  Lit = literal(lang(Lang,_)),
  % It is apparently a feature of rdf/[3,4] to match
  % =|literal(lang(Language,Literal))|= against =|literal(Literal)|=,
  % so we need to check for the language tag being instantiated.
  nonvar(Lang).
rdf_plain_literal(G, Lit):-
  rdf_simple_literal(G, Lit).

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

%! rdf_simple_literal(?Graph:atom, ?SimpleLiteral:atom) is nondet.

rdf_simple_literal(G, Lit):-
  % rdf/[3,4] throws an exception for numeric input.
  \+ number(Lit),
  rdf(_, _, Lit, G),
  Lit = literal(Lex),
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(Lang,Lexical)` or `type(Type,Lexical)`.
  atomic(Lex).

%! rdf_typed_literal(
%!   ?Graph:atom,
%!   ?TypedLiteral:compound,
%!   ?Datatype:iri,
%!   ?Lexical:atom
%! ) is nondet.

rdf_typed_literal(G, Lit, Datatype, Lex):-
  nonvar(Datatype), nonvar(Lit), !,
  Lit = literal(type(Datatype,Lex)),
  rdf(_, _, Lit, G).
rdf_typed_literal(G, Lit1, Datatype, Lex):-
  nonvar(Lit1), !,
  rdf_global_object(Lit1, Lit2),
  Lit2 = literal(type(Datatype,Lex)),
  rdf(_, _, Lit2, G).

