:- module(
  rdf_lit,
  [
    rdf_is_plain_literal/1, % ?PlainLiteral:compound
    rdf_is_simple_literal/1, % ?SimpleLiteral:compound
    rdf_is_typed_literal/1, % ?TypedLiteral:compound
    rdf_literal_equality/2, % +Literal1:literal
                            % +Literal2:literal
    rdf_plain_literal/2, % ?Graph:atom
                         % ?PlainLiteral:compound
    rdf_simple_literal/2, % ?SimpleLiteral:compound
                          % ?Lexical:atom
    rdf_simple_literal/3, % ?Graph:atom
                          % ?SimpleLiteral:compound
                          % ?Lexical:atom
    rdf_typed_literal/3, % ?TypedLiteral:compound
                         % ?Datatype:iri
                         % ?Value
    rdf_typed_literal/4 % ?Graph:atom
                        % ?TypedLiteral:compound
                        % ?Datatype:iri
                        % ?Value
  ]
).

/** <module> RDF literals

Support for RDF literals.

@author Wouter Beek
@version 2013/09-2013/11
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_typed_literal(o,r,?)).
:- rdf_meta(rdf_typed_literal(?,o,r,?)).



%! rdf_is_plain_literal(+X) is semidet.
%! rdf_is_simple_literal(+X) is semidet.
%! rdf_is_typed_literal(+X) is semidet.
% Notice that these are complete (succeeds for every plain/simple/typed
% literal, but not sound (succeeds for non-literals.

rdf_is_plain_literal(literal(lang(_,_))).

rdf_is_simple_literal(literal(_)).

rdf_is_typed_literal(literal(type(_,_))).

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

%! rdf_simple_literal(+SimpleLiteral:compound, +Lexical:atom) is semidet.
%! rdf_simple_literal(+SimpleLiteral:compound, -Lexical:atom) is det.
%! rdf_simple_literal(-SimpleLiteral:compound, +Lexical:atom) is det.

rdf_simple_literal(literal(LEX), LEX).

%! rdf_simple_literal(
%!   ?Graph:atom,
%!   ?SimpleLiteral:atom,
%!   ?Lexical:atom
%! ) is nondet.

rdf_simple_literal(G, Lit, LEX):-
  % rdf/[3,4] throws an exception for numeric input.
  \+ number(Lit),
  rdf(_, _, Lit, G),
  Lit = literal(LEX),
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(Lang,Lexical)` or `type(Type,Lexical)`.
  atomic(LEX).

%! rdf_typed_literal(
%!   ?TypedLiteral:compound,
%!   ?Datatype:iri,
%!   ?Lexical:atom
%! ) is det.

rdf_typed_literal(Lit, D, LEX):-
  Lit = literal(type(D, LEX)).

%! rdf_typed_literal(
%!   ?Graph:atom,
%!   ?TypedLiteral:compound,
%!   ?Datatype:iri,
%!   ?Lexical:atom
%! ) is nondet.

rdf_typed_literal(G, Lit1, D, LEX):-
  (nonvar(Lit1) -> rdf_global_object(Lit1, Lit2) ; Lit2 = Lit1),
  rdf_typed_literal(Lit2, D, LEX),
  rdf(_, _, Lit2, G).

