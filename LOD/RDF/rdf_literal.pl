:- module(
  rdf_literal,
  [
    rdf_is_simple_literal/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_simple_literal_value/2 % ?SimpleLiteral:compound
                               % ?LexicalForm:atom
    rdf_simple_literal/1, % ?SimpleLiteral:compound
    rdf_simple_literal/2 % ?Graph:atom
                         % ?SimpleLiteral:compound
  ]
).

/** <module> RDF literal

Support for RDF 1.1 literals.

@author Wouter Beek
@version 2014/03
*/



%! rdf_is_simple_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.
% Succeeds if the given RDF term is a simple literal.

rdf_is_simple_literal(literal(Atom)):-
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(LangTag,Value)` or `type(Type,Lexical)`.
  atom(Atom).


%! rdf_simple_literal(+SimpleLiteral:atom, +Value:atom) is nondet.
%! rdf_simple_literal(?SimpleLiteral:atom, ?Value:atom) is nondet.
% Enumeration is assured to not deliver any duplicates.

rdf_simple_literal(Literal):-
  % Enumerate all literals.
  rdf_current_literal(Literal),
  % Make sure the literal is simple.
  rdf_is_simple_literal(Literal).


%! rdf_simple_literal(+RdfGraph, +SimpleLiteral:compound) is semidet.
%! rdf_simple_literal(+RdfGraph:atom, -SimpleLiteral:atom) is nondet.
%! rdf_simple_literal(-RdfGraph:atom, +SimpleLiteral:atom) is nondet.
%! rdf_simple_literal(-RdfGraph:atom, -SimpleLiteral:atom) is nondet.
% Relates simple literals to the RDF graph in which they occur.
% Enumeration is assured to not deliver any pair duplicates.

rdf_simple_literal(Graph, Literal):-
  % Enumerate all (i.e. any graph) simple literals without duplicates.
  rdf_simple_literal(Literal),
  % Relate the simple literal to a graph.
  rdf_object(Graph, Literal).

