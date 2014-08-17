:- module(
  sparql_select,
  [
    'SelectClause'//3 % ?Distinct:boolean
                      % ?Reduced:boolean
                      % ?Variables:list(or([oneof(['*']),atom,pair(compound,atom)]))
  ]
).

/** <module> SPARQL SELECT

Grammar of the SPARQL SELECT query.

@author Wouter Beek
@version 2014/08
*/

:- use_module(dcg(dcg_content)).

:- use_module(sparql(sparql_char)).
:- use_module(sparql(sparql_dataset)).
:- use_module(sparql(sparql_word)).



%! 'SelectClause'(
%!   ?Distinct:boolean,
%!   ?Reduced:boolean,
%!   ?Variables:list(or([oneof(['*']),atom,pair(compound,atom)]))
%! )// is det.
% ~~~{.ebnf}
% SelectClause ::= 'SELECT'
%                  ( 'DISTINCT' | 'REDUCED' )?
%                  ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )
% ~~~
%
% @compat SPARQL Query 1.1 [9].

'SelectClause'(Distinct, Reduced, Vars) -->
  % Keyword.
  "SELECT", 'WS+',
  
  % Distinct and reduced modifiers.
  (
    {Distinct = true},
    {Reduced = false},
    "DISTINCT", 'WS+'
  ;
    {Distinct = false},
    {Reduced = true},
    "REDUCED", 'WS+'
  ;
    {Distinct = false},
    {Reduced = false},
    ""
  ),
  
  % Specification of return variables.
  (
    {Vars = []},
    "*"
  ;
    'SelectClauseVars'(Vars)
  ).
% A variable based on an expression.
'SelectClauseVars'([Expression-Var|T]) -->
  bracketed(round, (
    'Expression'(Expression), 'WS+',
    "AS", 'WS+',
    'Var'(H),
  ),
  'SelectClauseVars'(T).
% A regular variable.
'SelectClauseVars'([H|T]) -->
  'Var'(H),
  'SelectClauseVars'(T).



%! 'SelectQuery'// is det.
% ~~~{.ebnf}
% SelectQuery ::= SelectClause DatasetClause* WhereClause SolutionModifier
% ~~~
%
% @compat SPARQL Query 1.1 [7].

'SelectQuery'(Distinct, Reduced, Variables, Dataset) -->
  'SelectClause'(Distinct, Reduced, Variables),
  'DatasetClause*'(Dataset),
  'WhereClause',
  'SolutionModifier'.

'DatasetClause*'([H|T]) -->
  'DatasetClause'(H),
  'DatasetClause*'(T).
'DatasetClause*'([]) --> [].



%! 'WhereClause'// is det.
% ~~~{.ebnf}
% 	WhereClause ::= 'WHERE'? GroupGraphPattern
% ~~~
%
% @compat SPARQL 1.1 Query [17].

'WhereClause' -->
  ("WHERE", 'WS+' ; ""),
  'GroupGraphPattern'.
