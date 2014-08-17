:- module(
  sparql_query,
  [
    'BaseDecl'//1, % -Base:iri
    'PrefixDecl'//2, % -PrefixLabel:atom
                     % -Iri:iri
    'QueryUnit'//1 % ?BaseIris:list(iri)
  ]
).

/** <module> SPARQL Query

Grammar of a SPARQL Query expression.

@author Wouter Beek
@version 2014/08
*/

:- use_module(sparql(sparql_iri)).
:- use_module(sparql(sparql_word)).



%! 'BaseDecl'(?Base:iri)// is det.
% Defines the base IRI that is used to resolve relative IRIs.
%
% ~~~{.ebnf}
% BaseDecl ::= 'BASE' IRIREF
% ~~~
%
% @compat SPARQL 1.1 Query [5].
% @tbd Make sure that IRIREF is an absolute IRI.
% @tbd Implement relative IRI resolution per RFC3987.

'BaseDecl'(Base) -->
  alpha_to_lower(base), ws+,
  'IRIREF'(Base).



%! 'PrefixDecl'(-Prefix:atom, -Iri:iri)// is det.
% Associates a prefix label with an IRI.
% This allows prefixed names to be used in the rest of the query.
%
% ~~~{.ebnf}
% PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
% ~~~
%
% @compat SPARQL 1.1 Query [6].
% @tbd Make sure that prefix labels are not redefined in the same query.

'PrefixDecl'(Prefix, Iri) -->
  alpha_to_lower(prefix), ws+,
  'PNAME_NS'(Prefix),
  'IRIREF'(Iri).



%! 'Prologue'(?Bases:list(iri), ?Prefixes:list(pair(atom,iri)))// is det.
% ~~~{.ebnf}
% Prologue ::= ( BaseDecl | PrefixDecl )*
% ~~~
%
% @compat SPARQL 1.1 Query [4].

'Prologue'([Base|T], L) -->
  'BaseDecl'(Base),
  'Prologue'(T, L).
'Prologue'(L, [Prefix-Iri|T]) -->
  'PrefixDecl'(Prefix, Iri),
  'Prologue'(L, T).
'Prologue'([], []) --> [].



%! 'Query'(?BaseIris:list(iri))// is det.
% ~~~{.ebnf}
% Query ::= Prologue
%           ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery )
%           ValuesClause
% ~~~
%
% @compat SPARQL 1.1 Query [2].

'Query'(Bases, Prefixes) -->
  'Prologue'(Bases, Prefixes),
  
  % Query form.
  ('SelectQuery' ; 'ConstructQuery' ; 'DescribeQuery' ; 'AskQuery'),
  
  'ValuesClause'.



%! 'QueryUnit'(?BaseIris:list(iri))// is det.
% ~~~{.ebnf}
% QueryUnit ::= Query
% ~~~
%
% @compat SPARQL 1.1 Query [1].

'QueryUnit'(BaseIris) -->
  'Query'(BaseIris).
