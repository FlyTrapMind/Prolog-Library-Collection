:- module(
  sparql_update,
  [
    sparql_update/1 % +Graph:atom
  ]
).

/** <module> SPARQL update

Support for the SPARQL 1.1 Update recommendation.

@author Wouter Beek
@version 2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).



%! 'GraphTerm'// .
% ~~~{.ebnf}
% [109]    GraphTerm ::= iri |
%                        RDFLiteral |
%                        NumericLiteral |
%                        BooleanLiteral |
%                        BlankNode |
%                        NIL
% ~~~

'GraphTerm' --> iri.
'GraphTerm' --> 'RDFLiteral'.
'GraphTerm' --> 'NumericLiteral'.
'GraphTerm' --> 'BooleanLiteral'.
'GraphTerm' --> 'BlankNode'.
'GraphTerm' --> 'NIL'.


%! 'QuadData'(Data)// .
% ~~~{.ebnf}
% [49]    QuadData ::= '{' Quads '}'
% ~~~

'QuadData' -->
  bracketed(curly, 'Quads').


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


%! 'TriplesSameSubject'// .
% ~~~{.ebnf}
% [75]    TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty |
%                                TriplesNode PropertyList
% ~~~

'TriplesSameSubject' -->
  'VarOrTerm',
  'PropertyListNotEmpty'.
'TriplesSameSubject' -->
  'TriplesNode',
  'PropertyList'.


% ~~~{.ebfn}
% [52]    TriplesTemplate ::= TriplesSameSubject ( '.' TriplesTemplate? )?
% ~~~

'TriplesTemplate' -->
  'TriplesSameSubject',
  (`` ; `.`, (`` ; 'TriplesTemplate')).


%! 'Var'// .
% ~~~{.ebnf}
% [108]    Var ::= VAR1 | VAR2
% ~~~

'Var' --> 'VAR1'.
'Var' --> 'VAR2'.

%! 'VarOrTerm'// .
% ~~~{.ebnf}
% [106]    VarOrTerm ::= Var | GraphTerm
% ~~~

'VarOrTerm' --> 'Var'.
'VarOrTerm' --> 'GraphTerm'.

