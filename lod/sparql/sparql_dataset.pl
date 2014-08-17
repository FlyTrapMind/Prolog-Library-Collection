:- module(
  sparql_dataset,
  [
    'DatasetClause'// % ?Graph:compound
  [
).

/** <module> SPARQL dataset

Grammar of SPARQL datasets.

@author Wouter Beek
@version 2014/08
*/

:- use_module(sparql(sparql_word)).



%! 'DatasetClause'(?Graph:compound)// is det.
% `Graph` is a compound of either of the following two forms:
%   - `default(?iri)`
%   - `named(?iri)`
%
% ~~~{.ebnf}
% DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
% ~~~
%
% @compat SPARQL 1.1 Query [13].

% Default graph.
'DatasetClause'(default(Graph)) -->
  "FROM", 'WS+',
  'DefaultGraphClause'(Graph).
% Named graph.
'DatasetClause'(named(Graph)) -->
  "FROM", 'WS+',
  'NamedGraphClause'(Graph).



%! 'DefaultGraphClause'(?Graph:iri)// is det.
% ~~~{.ebnf}
% DefaultGraphClause ::= SourceSelector
% ~~~

'DefaultGraphClause'(Graph) -->
  'SourceSelector'(Graph).


%! 'NamedGraphClause'(?Graph:iri)// is det.
% ~~~{.ebnf}
% NamedGraphClause ::= 'NAMED' SourceSelector
% ~~~
%
% @compat SPARQL 1.1 Query [15].

'NamedGraphClause'(Graph) -->
  "NAMED", 'WS+',
  'SourceSelector'(Graph).



% 'SourceSelector'(?Graph:iri)// is det.
% ~~~{.ebnf}
% SourceSelector ::= iri
% ~~~
%
% @compat SPARQL 1.1 Query [16].

'SourceSelector'(Graph) -->
  iri(Graph).
