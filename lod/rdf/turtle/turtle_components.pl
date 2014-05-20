:- module(
  turtle_components,
  [
    iri//
  ]
).

/** <module> Turtle components

Reusable components of the Turtle grammar.

@author Wouter Beek
@version 2014/05
*/

:- use_module(turtle(turtle_terminals)).



%! iri// .
% ~~~{.ebnf}
% [135s]   iri ::= IRIREF | PrefixedName
% ~~~
%
% @compat Turtle 1.1 [135a]
% @compat SPARQL 1.1 Query [136]

iri --> 'IRIREF'.
iri --> 'PrefixedName'.


%! 'PrefixedName'// .
% ~~~{.EBNF}
% [136s]   PrefixedName ::= PNAME_LN | PNAME_NS
% ~~~
%
% @compat Turtle 1.1 [136s].
% @compat SPARQL 1.1 Query [137].

'PrefixedName' --> 'PNAME_LN'.
'PrefixedName' --> 'PNAME_NS'.

