:- module(
  xsd_clean,
  [
    xsd_canonize_graph/1, % +Graph:atom
    xsd_convert_value/4, % +FromDatatype:iri
                         % +FromLexicalExpression:atom
                         % +ToDatatype:iri
                         % -ToLexicalExpression:atom
    xsd_convert_value/4, % +FromDatatype:iri
                         % +FromLexicalExpression:list(code)
                         % +ToDatatype:iri
                         % -ToLexicalExpression:list(code)
    xsd_lexical_canonical_map/3, % +Datatype:iri
                                 % +Lexical:atom
                                 % -CanonicalLexical:atom
    xsd_lexical_canonical_map/3, % +Datatype:iri
                                 % +Lexical:list(code)
                                 % -CanonicalLexical:list(code)
    xsd_value/3, % +Value
                 % +Datatype:or([atom,iri])
                 % -XsdValue
    xsd_value/3 % +Value
                % -Datatype:iri
                % -XsdValue
  ]
).

/** <module> XSD clean

Predicates for cleaning XML Scheme 1.1 datatypes.

@author Wouter Beek
@version 2013/08-2013/10, 2014/01, 2014/03
*/

:- use_module(generics(codes_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(xsd_convert_value(r,+,r,-)).
:- rdf_meta(xsd_value(+,r,-)).



%! xsd_canonize_graph(+Graph:atom) is det.
% Make sure all typed literals in the graph with the given name
% have a lexical value that is a canonical value for its datatype.
%
% This check every RDF triple in the given graph
% that contains a typed literal.
%
% @tbd Use a predicate from module [rdf/rdf_lit_read].

xsd_canonize_graph(Graph):-
  forall(
    rdf(S, P, literal(type(Datatype,Lexical)), Graph),
    xsd_canonize_triple(S, P, Datatype, Lexical, Graph)
  ).


%! xsd_canonize_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   +Lexical:atom,
%!   +Graph:atom
%! ) is det.
% Converts from lexical to value,
% and then from value to canonical lexical.

xsd_canonize_triple(S, P, Datatype, Lexical, Graph):-
  xsd_datatype(Datatype),
  xsd_lexical_canonical_map(Datatype, Lexical, CanonicalLexical),
  
  % Only changes need to be written.
  Lexical \== CanonicalLexical,
  
  rdf_retractall(S, P, literal(type(Datatype,Lexical)), Graph),
  rdf_assert(S, P, literal(type(Datatype,CanonicalLexical)), Graph).


%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalExpression:atom,
%!   +ToDatatype:uri,
%!   -ToLexicalExpression:atom
%! ) is det.
%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalExpression:list(code),
%!   +ToDatatype:uri,
%!   -ToLexicalExpression:list(code)
%! ) is det.

xsd_convert_value(FromDatatype, FromLexical, ToDatatype, ToLexical):-
  atomic_codes_goal(
    xsd_convert_value_codes(FromDatatype, ToDatatype),
    FromLexical,
    ToLexical
  ).

xsd_convert_value_codes(FromDatatype, ToDatatype, FromLexical, ToLexical):-
  xsd_lexical_map(FromDatatype, FromLexical, Value),
  xsd_canonical_map(ToDatatype, Value, ToLexical).


%! xsd_lexical_canonical_map(
%!   +Datatype:iri,
%!   +Lexical:atom,
%!   -CanonicalLexical:atom
%! ) is det.
%! xsd_lexical_canonical_map(
%!   +Datatype:iri,
%!   +Lexical:list(code),
%!   -CanonicalLexical:list(code)
%! ) is det.
% Reads a datatype lexical expression and converts it into its canonical form.

xsd_lexical_canonical_map(Datatype, Lexical, CanonicalLexical):-
  xsd_convert_value(Datatype, Lexical, Datatype, CanonicalLexical).


%! xsd_value(+Value1, +Datatype:or([atom,iri]), -Value2) is det.
%! xsd_value(+Value1, -Datatype:iri, -Value2) is nondet.

% Try out different datatypes.
xsd_value(Lexical, Datatype, CanonicalLexical):-
  var(Datatype), !,
  % Choicepoint.
  xsd_datatype(Datatype),
  xsd_value(Lexical, Datatype, CanonicalLexical).
% Allow datatypes to be denoted by a shortcut name.
xsd_value(PrologValue, Name, CanonicalLexical):-
  xsd_datatype(Name, Datatype), !,
  xsd_value(PrologValue, Datatype, CanonicalLexical).
% The value is not yet in XSD format, but could be converted
% by using the supported mappings.
xsd_value(XsdValue, Datatype, CanonicalLexical):-
  xsd_datatype(Datatype), !,
  xsd_canonical_map(Datatype, XsdValue, CanonicalLexical).
% The value is already in XSD format: recognize that this is the case
% and convert it back and forth to ensure we have the canonical lexical.
xsd_value(Lexical, Datatype, CanonicalLexical):-
  xsd_datatype(Datatype), !,
  xsd_lexical_map(Datatype, Lexical, XsdValue),
  xsd_value(XsdValue, Datatype, CanonicalLexical).

