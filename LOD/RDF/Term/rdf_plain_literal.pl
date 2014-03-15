:- module(
  rdf_plain_literal,
  [
    rdf_is_plain_literal/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_plain_literal/1, % ?PlainLiteral:compound
    rdf_plain_literal/2, % ?Graph:atom
                         % ?PlainLiteral:compound
    rdf_plain_literal_language_tag/2, % ?PlainLiteral:compound
                                      % ?LanguageTag:atom
    rdf_plain_literal_lexical_form/2 % ?PlainLiteral:compound
                                     % ?LexicalForm:atom
  ]
).

/** <module> RDF plain literal

Support for RDF 1.0 plain literals.
Plain literals are obsolete in RDF 1.1.

A plain literal used to be defined as the union of the Unicode strings
in Normal Form C and the cartesian product of the Unicode strings
in Normal Form C with the set of BCP 47 language tags.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_simple_literal)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_is_plain_literal(o)).
:- rdf_meta(rdf_plain_literal(o)).
:- rdf_meta(rdf_plain_literal(?,o)).
:- rdf_meta(rdf_plain_literal_language_tag(o,?)).
:- rdf_meta(rdf_plain_literal_lexical_form(o,?)).



%! rdf_is_plain_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.

rdf_is_plain_literal(SimpleLiteral):-
  rdf_is_simple_literal(SimpleLiteral).
rdf_is_plain_literal(literal(lang(LanguageTag,LexicalForm))):-
  atom(LanguageTag),
  atom(LexicalForm).


%! rdf_plain_literal(+PlainLiteral:compound) is semidet.
%! rdf_plain_literal(-PlainLiteral:compound) is nondet.
% Plain literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal(PlainLiteral):-
  % Enumerate all literals.
  rdf_current_literal(PlainLiteral),
  % Make sure the literal is plain.
  rdf_is_plain_literal(PlainLiteral).


%! rdf_plain_literal(+RdfGraph:atom, +PlainLiteral:compound) is semidet.
%! rdf_plain_literal(+RdfGraph:atom, -PlainLiteral:compound) is nondet.
%! rdf_plain_literal(-RdfGraph:atom, +PlainLiteral:compound) is nondet.
%! rdf_plain_literal(-RdfGraph:atom, -PlainLiteral:compound) is nondet.
% Pairs of RDF graphs to plain literals.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal(Graph, PlainLiteral):-
  rdf_plain_literal(PlainLiteral),
  rdf_object(RdfGraph, PlainLiteral).


%! rdf_plain_literal_language_tag(+PlainLiteral:compound, +LanguageTag:atom) is semidet.
%! rdf_plain_literal_language_tag(+PlainLiteral:compound, -LanguageTag:atom) is semidet.
%! rdf_plain_literal_language_tag(-PlainLiteral:compound, +LanguageTag:atom) is nondet.
%! rdf_plain_literal_language_tag(-PlainLiteral:compound, -LanguageTag:atom) is nondet.

rdf_plain_literal_language_tag(PlainLiteral, LanguageTag):-
  % Enumerate all plain literals.
  rdf_plain_literal(PlainLiteral),
  PlainLiteral = literal(lang(LanguageTag,_)).


%! rdf_plain_literal_lexical_form(+PlainLiteral:compound, +LexicalForm:atom) is semidet.
%! rdf_plain_literal_lexical_form(+PlainLiteral:compound, -LexicalForm:atom) is det.
%! rdf_plain_literal_lexical_form(-PlainLiteral:compound, +LexicalForm:atom) is nondet.
%! rdf_plain_literal_lexical_form(-PlainLiteral:compound, -LexicalForm:atom) is nondet.

rdf_plain_literal_lexical_form(PlainLiteral, LexicalForm):-
  rdf_plain_literal(PlainLiteral),
  PlainLiteral = literal(lang(_,LexicalForm)).

