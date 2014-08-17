:- module(
  sparql_triple_pattern,
  [
  ]
).

/** <module> SPARQL Triple Patterns

Grammar for triple patterns in SPARQL.

Object lists can be combined with predicate-object lists.
[1] expresses the same as [1'].

~~~{.sparql}
[1]    ?x  foaf:name  ?name    ;
           foaf:nick  "Alice"  ,
                      "Alice_" .
[1']   ?x  foaf:name  ?name .
       ?x  foaf:nick  "Alice" .
       ?x  foaf:nick  "Alice_" .
~~~

--

@author Wouter Beek
@version 2014/08
*/

:- use_module(dcg(dcg_generic)).



%! 'BlankNodePropertyList'(?BNodePropertyList:compound)// is det.
% The `[:p :v]` construct can be used in triple patterns.
% It creates a blank node label which is used as the subject
% of all contained predicate-object pairs.
% The created blank node can also be used in further triple patterns
% in the subject and object positions.
%
% [1] and [2] both allocate a unique blank node label
% (called `b57`) and are equivalent to [3].
%
% ~~~{.sparql}
% [1]   [ :p "v" ] .
% [2]   [] :p "v" .
% [3]   _:b57 :p "v" .
% ~~~
%
% This allocated blank node label can be used as the subject [4] or object [5]
% of further triple patterns.
%
% ~~~{.sparql}
% [4]   [ :p "v" ] :q "w" .
% [5]   :x :q [ :p "v" ] .
% ~~~
%
% [4] is equivalent to [4a] and [4b].
% [5] is equivalent to [5a] and [5b].
%
% ~~~{.sparql}
% [4a]   _:b57 :p "v" .
% [4b]   _:b57 :q "w" .
% [5a]   :x  :q _:b57 .
% [5b]   _:b57 :p "v" .
% ~~~
%
% ~~~{.ebnf}
% BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
% ~~~
%
% @compat SPARQL 1.1 Query [99].

'BlankNodePropertyList'(rdf(bnode(X), ProperyList)) -->
  bracketed(angular, 'PropertyListNotEmpty'(X, ProperyList)).



%! 'ObjectList'(?Subject, ?Predicate, ?Triples:list(compound))// is det.
% If triple patterns share both subject and predicate,
% the objects may be separated by `,`.
%
% [1] expresses the same as [2].
%
% ~~~{.ebnf}
% [1]   ?x foaf:nick  "Alice" ,
%                     "Alice_" .
% [2]   ?x  foaf:nick  "Alice"  .
%       ?x  foaf:nick  "Alice_" .
% ~~~
%
% ~~~{.ebnf}
% ObjectList ::= Object ( ',' Object )*
% ~~~
%
% @compat SPARQL 1.1 Query [79].

'ObjectList'(S, P, [rdf(S,P,O)]) -->
  'Object'(O).
'ObjectList'(S, P, [rdf(S,P,O)|T]) -->
  comma_separator,
  'Object'(O), ws+,
  'ObjectList'(T).



%! 'PropertyList'(?Subject, ?Triples:list(compound))// is det.
% ~~~{.ebnf}
% PropertyList ::= PropertyListNotEmpty?
% ~~~
%
% @compat SPARQL 1.1 Query [76].

'PropertyList'(_, []) --> [].
'PropertyList'(S, Triples) -->
  'PropertyListNotEmpty'(S, Triples).



%! 'PropertyListNotEmpty'(?Subject, ?Triples:list(compound))// is det.
% Triple patterns with a common subject can be written so that
% the subject is only written once and is used
% for more than one triple pattern by employing the `;` notation.
%
% [1] expresses the same as [1'].
%
% ~~~{.sparql}
% [1]    ?x  foaf:name  ?name ;
%            foaf:mbox  ?mbox .
% [1']   ?x  foaf:name  ?name .
%        ?x  foaf:mbox  ?mbox .
% ~~~
%
% ~~~{.ebnf}
% PropertyListNotEmpty ::= Verb ObjectList ( ';' ( Verb ObjectList )? )*
% ~~~
%
% @compat SPARQL 1.1 Query [77].

'PropertyListNotEmpty'(S, [rdf(S,P,O)|T]) -->
  'PropertyListNotEmpty'(S, P, [rdf(S,P,O)|T]).

'PropertyListNotEmpty'(S, P, [rdf(S,P,O)|T]) -->
  'Verb'(P), ws+,
  'ObjectList'(S, P, [rdf(S,P,O)|T]).
'PropertyListNotEmpty'(S, P, [rdf(S,P,O)|T]) -->
  'Verb'(P), ws+,
  'ObjectList'(S, P, [rdf(S,P,O)|T]),
  semicolon_separator,
  'PropertyListNotEmpty'(S, P, T).



%! 'TriplesNode'
% ~~~{.ebnf}
% TriplesNode ::= Collection | BlankNodePropertyList
% ~~~
%
% @compat SPARQL 1.1 Query [98].

'TriplesNode'(S) -->
  'Collection'.
'TriplesNode' -->
  'BlankNodePropertyList'.



%! 'TriplesSameSubject'// .
% ~~~{.ebnf}
% TriplesSameSubject ::=   VarOrTerm PropertyListNotEmpty
%                        | TriplesNode PropertyList
% ~~~
%
% @compat SPARQL 1.1 Query [75].

'TriplesSameSubject'([rdf(S,P,O)|T]) -->
  'VarOrTerm'(S),
  'PropertyListNotEmpty'(S, [rdf(S,P,O)|T]).
'TriplesSameSubject'(S, [rdf(S,P,O)|T]) -->
  'TriplesNode'(S),
  'PropertyList'(S, [rdf(S,P,O)|T]).
