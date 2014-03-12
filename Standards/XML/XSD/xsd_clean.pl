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
    xsd_value/3, % +Datatype:or([atom,iri])
                 % +Value
                 % -XsdValue
    xsd_value/3 % -Datatype:iri
                % +Value
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
% @tbd Is is not more efficient to canonize per object term?
%      Triples that share the same non-canonical typed literal
%      will be updated at once.

xsd_canonize_graph(Graph):-
  forall(
    rdf(Subject, Predicate, literal(type(Datatype,Lexical)), Graph),
    xsd_canonize_triple(Subject, Predicate, Datatype, Lexical, Graph)
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

xsd_canonize_triple(Subject, Predicate, Datatype, Lexical, Graph):-
  xsd_datatype(Datatype),
  xsd_lexical_canonical_map(Datatype, Lexical, CanonicalLexical),
  
  % Only changes need to be written.
  Lexical \== CanonicalLexical,
  
  % Update the object term.
  rdf_update(
    Subject,
    Predicate,
    literal(type(Datatype,Lexical)),
    Graph,
    object(literal(type(Datatype,CanonicalLexical)))
  ).


%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalExpression:atom,
%!   +ToDatatype:uri,
%!   -ToLexicalExpression:atom
%! ) is semidet.
%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalExpression:list(code),
%!   +ToDatatype:uri,
%!   -ToLexicalExpression:list(code)
%! ) is semidet.

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


%! xsd_value(+Datatype:or([atom,iri]), +Value1, -Value2) is det.
%! xsd_value(-Datatype:iri,            +Value1, -Value2) is nondet.

% Try out different datatypes.
xsd_value(Datatype, Lexical, CanonicalLexical):-
  var(Datatype), !,
  % Choicepoint.
  xsd_datatype(Datatype),
  xsd_value(Datatype, Lexical, CanonicalLexical).
% Allow datatypes to be denoted by a shortcut name.
xsd_value(Name, PrologValue, CanonicalLexical):-
  xsd_datatype(Name, Datatype), !,
  xsd_value(Datatype, PrologValue, CanonicalLexical).
% The value is not yet in XSD format, but could be converted
% by using the supported mappings.
xsd_value(Datatype, XsdValue, CanonicalLexical):-
  xsd_datatype(Datatype),
  xsd_canonical_map(Datatype, XsdValue, CanonicalLexical), !.
% The value is already in XSD format: recognize that this is the case
% and convert it back and forth to ensure we have the canonical lexical.
xsd_value(Datatype, Lexical, CanonicalLexical):-
  xsd_datatype(Datatype),
  xsd_lexical_map(Datatype, Lexical, XsdValue),
  xsd_value(Datatype, XsdValue, CanonicalLexical).

