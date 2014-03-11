:- module(
  rdf_lit,
  [
    rdf_is_plain_literal/1, % ?PlainLiteral:compound
    rdf_is_simple_literal/1, % ?SimpleLiteral:compound
    rdf_is_typed_literal/1, % ?TypedLiteral:compound
    rdf_literal_equality/2, % +Literal1:literal
                            % +Literal2:literal
    rdf_plain_literal/2, % ?PlainLiteral:compound
                         % ?Value:atom
    rdf_plain_literal/3, % ?Graph:atom
                         % ?PlainLiteral:compound
                         % ?Value:atom
    rdf_simple_literal/2, % ?SimpleLiteral:compound
                          % ?Value:atom
    rdf_simple_literal/3, % ?Graph:atom
                          % ?SimpleLiteral:compound
                          % ?Value:atom
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
@version 2013/09-2013/11, 2014/01
*/

:- use_module(generics(codes_ext)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd_clean)).



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

% Plain literals with the same language tag and value string.
rdf_literal_equality(
  literal(lang(Lang1,Lit1)),
  literal(lang(Lang2,Lit2))
):- !,
  Lang1 == Lang2,
  Lit1 == Lit2.
% Typed literals with equivalent values in the datatype's value space.
rdf_literal_equality(
  literal(type(Type1,Value1)),
  literal(type(Type2,Value2))
):- !,
  Type1 == Type2,
  maplist(atomic_codes, [Value1,Value2], [LEX1,LEX2]),
  xsd_lexical_canonical_map(Type1, LEX1, CAN1),
  xsd_lexical_canonical_map(Type2, LEX2, CAN2),
  CAN1 == CAN2.
% Simple literals that are the same.
rdf_literal_equality(literal(Lit1), literal(Lit2)):- !,
  Lit1 == Lit2.


%! rdf_plain_literal(?PlainLiteral:compound, ?Value:atom) is nondet.
%! rdf_plain_literal(
%!   ?Graph:atom,
%!   ?PlainLiteral:compound,
%!   ?Value:atom
%! ) is nondet.

rdf_plain_literal(Literal, Value):-
  % Enumerates without duplicates.
  rdf_current_literal(Literal),
  
  % It is apparently a feature of rdf/[3,4] to match
  % =|literal(lang(LangTag,Literal))|= against =|literal(Literal)|=,
  % so we do not need a special clause for simple literals.
  Literal = literal(lang(_,Value)).

rdf_plain_literal(Graph, Literal, Value):-
  rdf_plain_literal(Literal, Value),
  
  % Relate to a graph.
  rdf(_, _, Literal, Graph).


%! rdf_simple_literal(?SimpleLiteral:atom, ?Value:atom) is nondet.
%! rdf_simple_literal(
%!   ?Graph:atom,
%!   ?SimpleLiteral:atom,
%!   ?Value:atom
%! ) is nondet.

rdf_simple_literal(Literal, Value):-
  % Enumerates without duplicates.
  rdf_current_literal(Literal),
  
  Literal = literal(Value),
  
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(LangTag,Value)` or `type(Type,Lexical)`.
  atomic(Value).

rdf_simple_literal(Graph, Literal, Value):-
  rdf_simple_literal(Literal, Value),
  
  % Relate to a graph.
  rdf(_, _, Literal, Graph).


%! rdf_typed_literal(
%!   ?TypedLiteral:compound,
%!   ?Datatype:iri,
%!   ?Lexical:atom
%! ) is det.
%! rdf_typed_literal(
%!   ?Graph:atom,
%!   ?TypedLiteral:compound,
%!   ?Datatype:iri,
%!   ?Lexical:atom
%! ) is nondet.

:- rdf_meta(rdf_typed_literal(o,r,?)).
rdf_typed_literal(Lit, D, Lexical):-
  Lit = literal(type(D, Lexical)).

:- rdf_meta(rdf_typed_literal(?,o,r,?)).
rdf_typed_literal(G, Lit1, D, Lexical):-
  (
    nonvar(Lit1)
  ->
    rdf_global_object(Lit1, Lit2)
  ;
    Lit2 = Lit1
  ),
  rdf_typed_literal(Lit2, D, Lexical),
  rdf(_, _, Lit2, G).

