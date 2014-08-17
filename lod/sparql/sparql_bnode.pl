:- module(
  sparql_bnode,
  [
  ]
).

/** <module> SPARQL Blank Node

Grammar for blank nodes in SPARQL.

Blank nodes in graph patterns act as variables,
not as references to specific blank nodes in the data being queried.

--

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).

:- use_module(sparql(sparql_char)).

:- dynamic(bnode_memory(BNodeLabel, BNode)).



%! 'Collection'(-RdfList:bnode)// is det.
% ~~~{.ebnf}
% Collection ::= '(' GraphNode+ ')'
% ~~~
%
% @compat SPARQL 1.1 Query [102].

'Collection'(RdfList) -->
  bracketed(round, 'GraphNode+'(Nodes)),
  {rdf_list(Nodes, RdfList)}.
'GraphNode+'([H|T]) -->
  'GraphNode'(H),
  'GraphNode+'(T).
'GraphNode+'([H]) -->
  'GraphNode'(H).



%! 'GraphNode'// is det.
% ~~~{.ebnf}
% GraphNode ::= VarOrTerm | TriplesNode
% ~~~
%
% @compat SPARQL 1.1 Query [104].

'GraphNode' -->
  'VarOrTerm'.
'GraphNode' -->
  'TriplesNode'.



%! 'Object'(?Object)// is det.
% ~~~{.ebnf}
% Object ::= GraphNode
% ~~~
%
% @compat SPARQL 1.1 Query [80].

'Object'(O) -->
  'GraphNode'(O).



%! 'VerbSimple'(?VarName:atom)// is det.
% ~~~{.ebnf}
% VerbSimple ::= Var
% ~~~
%
% @compat SPARQL 1.1 Query [85].

'VerbSimple'(VarName) -->
  'Var'(VarName).
