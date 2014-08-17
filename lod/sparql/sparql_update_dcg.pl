:- module(
  sparql_update_dcg,
  [
  ]
).

/** <module> SPARQL update

Support for the SPARQL 1.1 Update recommendation.

@author Wouter Beek
@version 2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).

:- use_module(sparql(sparql_literal)).
:- use_module(sparql(sparql_word)).



%! 'GraphTerm'(?Term)// .
% ~~~{.ebnf}
% GraphTerm ::= iri |
%               RDFLiteral |
%               NumericLiteral |
%               BooleanLiteral |
%               BlankNode |
%               NIL
% ~~~
%
% @compat SPARQL 1.1 Query [109].

'GraphTerm'(Iri) -->
  iri(Iri).
'GraphTerm'(Literal) -->
  'RDFLiteral'(Literal).
'GraphTerm'(Value) -->
  'NumericLiteral'(Value).
'GraphTerm'(Value) -->
  'BooleanLiteral'(Value).
'GraphTerm'(BNodeLabel) -->
  'BlankNode'(BNodeLabel).
'GraphTerm'(_VAR) -->
  'NIL'.


%! 'QuadData'(Data)// .
% ~~~{.ebnf}
% [49]    QuadData ::= '{' Quads '}'
% ~~~

'QuadData' --> bracketed(curly, 'Quads').


%! 'Quads'(+Data)// .
% ~~~{.ebnf}
% [50]    Quads ::= TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
% ~~~

'Quads' -->
  (`` ; 'TriplesTemplate'),
  'Quads1*'.

'Quads1*' --> [].
'Quads1*' -->
  'QuadsNotTriples',
  (`` ; `.`),
  (`` ; 'TriplesTemplate'),
  'Quads1*'.


% ~~~{.ebfn}
% [52]    TriplesTemplate ::= TriplesSameSubject ( '.' TriplesTemplate? )?
% ~~~

'TriplesTemplate' -->
  'TriplesSameSubject',
  (`` ; `.`, (`` ; 'TriplesTemplate')).
