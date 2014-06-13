:- module(
  sparql_update_dcg,
  [
    'BlankNode'//1, % ?BNodeLabel:atom
    'BooleanLiteral'//1 % ?Value:boolean
    'NumbericLiteral'//1, % ?Value:number
    'NumbericLiteralNegative'//1, % ?Value:number
    'NumbericLiteralPositive'//1, % ?Value:number
    'NumbericLiteralUnsigned'//1, % ?Value:number
  ]
).

/** <module> SPARQL update

Support for the SPARQL 1.1 Update recommendation.

@author Wouter Beek
@version 2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(sparql(sparql_word)).


%! 'BaseDecl'(?Iri:iri)// .
% ~~~{.ebnf}
% BaseDecl ::= 'BASE' IRIREF
% ~~~~
%
% @compat SPARQL 1.1 Query [5].

'BaseDecl'(Iri) -->
  `BASE`,
  'IRIREF'(Iri).


%! 'BlankNode'(?BNodeLabel:atom)// .
% ~~~{.ebnf}
% BlankNode ::= BLANK_NODE_LABEL | ANON
% ~~~
%
% @compat SPARQL 1.0 [137].
% @compat SPARQL 1.1 Query [138].
% @compat Turtle 1.1 [137s].

'BlankNode'(BNodeLabel) -->
  'BLANK_NODE_LABEL'(BNodeLabel).
'BlankNode'(_VAR) -->
  'ANON'.


%! 'BooleanLiteral'(?Value:boolean)// .
% ~~~{.ebnf}
% BooleanLiteral ::= 'true' | 'false'
% ~~~
%
% @compat SPARQL 1.0 [133].
% @compat SPARQL 1.1 Query [134].
% @compat Turtle 1.1 [133s].

'BooleanLiteral'(true) -->
  `true`.
'BooleanLiteral'(false) -->
  `false`.


%! 'Expression'// .
% ~~~{.ebnf}
% Expression ::= ConditionalOrExpression
% ~~~
%
% @compat SPARQL 1.1 Query [110].

'Expression' --> 'ConditionalOrExpression'.


%! 'ConditionalOrExpression'// .
% ~~~{.ebnf}
% ConditionalOrExpression ::= ConditionalAndExpression
%                             ( '||' ConditionalAndExpression )*
% ~~~
%
% @compat SPARQL 1.1 Query [110].

'ConditionalOrExpression' -->
  'ConditionalAndExpression',
  'ConditionalOrExpression_disjuncts'.

'ConditionalOrExpression_disjuncts'(or(X,Y)) -->
  `||`,
  'ConditionalAndExpression'(X),
  'ConditionalOrExpression_disjuncts'(Y).
'ConditionalOrExpression_disjuncts'([]) --> [].


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


%! iri(?Iri:atom)// .
% ~~~{.ebnf}
% iri ::= IRIREF | PrefixedName
% ~~~
%
% @compat SPARQL 1.0 [135]
% @compat SPARQL 1.1 Query [136]
% @compat Turtle 1.1 [135a]

iri(Iri) -->
  'IRIREF'(Iri).
iri(Iri) -->
  'PrefixedName'(Iri).


%! 'NumericLiteral'// .
% ~~~{.ebnf}
% NumericLiteral ::= NumericLiteralUnsigned |
%                    NumericLiteralPositive |
%                    NumericLiteralNegative
% ~~~
%
% @compat SPARQL 1.1 Query [130].

'NumericLiteral' -->
  'NumericLiteralUnsigned'.
'NumericLiteral' -->
  'NumericLiteralPositive'.
'NumericLiteral' -->
  'NumericLiteralNegative'.


%! 'NumericLiteralNegative'// .
% ~~~{.ebnf}
% NumericLiteralNegative ::= INTEGER_NEGATIVE |
%                            DECIMAL_NEGATIVE |
%                            DOUBLE_NEGATIVE
% ~~~
%
% @compat SPARQL 1.1 Query [132].

'NumericLiteralNegative' -->
  'INTEGER_NEGATIVE'.
'NumericLiteralNegative' -->
  'DECIMAL_NEGATIVE'.
'NumericLiteralNegative' -->
  'DOUBLE_NEGATIVE'.


%! 'NumericLiteralPositive'// .
% ~~~{.ebnf}
% NumericLiteralPositive ::= INTEGER_POSITIVE |
%                            DECIMAL_POSITIVE |
%                            DOUBLE_POSITIVE
% ~~~
%
% @compat SPARQL 1.1 Query [132].

'NumericLiteralPositive' -->
  'INTEGER_POSITIVE'.
'NumericLiteralPositive' -->
  'DECIMAL_POSITIVE'.
'NumericLiteralPositive' -->
  'DOUBLE_POSITIVE'.


%! 'NumericLiteralUnsigned'// .
% ~~~{.abnf}
% NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ~~~
%
% @compat SPARQL 1.1 Update [131].

'NumericLiteralUnsigned' --> 'INTEGER'.
'NumericLiteralUnsigned' --> 'DECIMAL'.
'NumericLiteralUnsigned' --> 'DOUBLE'.


%! 'PrefixDecl'(?Pefix:compound)// .
% ~~~{.ebnf}
% PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
% ~~~
%
% @compat SPARQL 1.1 Query [6].

'PrefixDecl'(prefix(Namespace,Iri)) -->
  `PREFIX`,
  whites,
  'PNAME_NS'(Namespace),
  whites,
  'IRIREF'(Iri).


%! 'PrefixedName'//(?Iri:iri) .
% ~~~{.EBNF}
% PrefixedName ::= PNAME_LN | PNAME_NS
% ~~~
%
% @compat SPARQL 1.0 [136].
% @compat SPARQL 1.1 Query [137].
% @compat Turtle 1.1 [136s].

'PrefixedName'(Iri) -->
  'PNAME_LN'(Prefix-LocalName),
  {rdf_global_id(Prefix:LocalName, Iri)}.
'PrefixedName'(Iri) -->
  'PNAME_NS'(Prefix),
  {rdf_global_id(Prefix:'', Iri)}.


%! 'Prologue'// .
% ~~~{.ebnf}
% Prologue ::= ( BaseDecl | PrefixDecl )*
% ~~~
%
% @compat SPARQL 1.1 Update [4].

'Prologue'([base(Iri)|T]) -->
  'BaseDecl'(Iri),
  'Prologue'(T).
'Prologue'([Prefix|T]) -->
  'PrefixDecl'(Prefix),
  'Prologue'(T).
'Prologue'([]) --> [].


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


%! 'Query'// .
% ~~~{.ebnf}
% Query ::= Prologue
%           ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery )
%           ValuesClause
% ~~~
%
% @compat SPARQL 1.1 Query [2].

'Query' -->
  'Prologue',
  (
    'SelectQuery'
  ;
    'ConstructQuery'
  ;
    'DescribeQuery'
  ;
    'AskQuery'
  ),
  'ValuesClause'.


%! 'QueryUnit'// .
% ~~~{.ebnf}
% QueryUnit ::= Query
% ~~~
%
% @compat SPARQL 1.1 Query [1].

'QueryUnit' -->
  'Query'.


%! 'RDFLiteral'// .
% ~~~{.ebnf}
% RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ~~~
%
% @compat SPARQL 1.0 [128].
% @compat SPARQL 1.1 Query [129].
% @compat Turtle 1.1 [128s].

'RDFLiteral' -->
  'String',
  (
    'LANGTAG'
  ;
    `^^`,
    iri
  ;
    ``
  ).


%! 'SelectClause'// .
% ~~~{.ebnf}
% SelectClause ::= 'SELECT'
%                  ( 'DISTINCT' | 'REDUCED' )?
%                  ( ( Var |
%                      ( '(' Expression 'AS' Var ')' ) )+ |
%                    '*' )
% ~~~
%
% @compat SPARQL 1.1 Query [9].

'SelectClause' -->
  `SELECT`,
  (`DISTINCT` ; `REDUCED` ; `` ),
  (
    'SelectClause_vars'
  ;
    `*`
  ).

'SelectClause_vars'([H|T]) -->
  'SelectClause_var'(H),
  'SelectClause_vars'(T).
'SelectClause_vars'([H]) -->
  'SelectClause_var'(H).

'SelectClause_var'(var(VarName)) -->
  'Var'(VarName).
'SelectClause_var'(var(VarName)) -->
  bracketed(round, 'SelectClause_var_as').

'SelectClause_var_as'(var(Expression,VarName)) -->
  'Expression'(Expression),
  whites,
  `AS`,
  whites,
  'Var'(VarName).


%! 'SelectQuery'// .
% ~~~{.ebnf}
% SelectQuery ::= SelectClause DatasetClause* WhereClause SolutionModifier
% ~~~
%
% @compat SPARQL 1.1 Query [7].

'SelectQuery' -->
  'SelectClause',
  'DatasetClause*',
  'WhereClause',
  'SolutionModifier'.


%! 'String'// .
% ~~~{.ebnf}
% String ::= STRING_LITERAL1 |
%            STRING_LITERAL2 |
%            STRING_LITERAL_LONG1 |
%            STRING_LITERAL_LONG2
% ~~~
%
% @compat SPARQL 1.1 Query [135].

'String' -->
  'STRING_LITERAL1'.
'String' -->
  'STRING_LITERAL2'.
'String' -->
  'STRING_LITERAL_LONG1'.
'String' -->
  'STRING_LITERAL_LONG2'.


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

