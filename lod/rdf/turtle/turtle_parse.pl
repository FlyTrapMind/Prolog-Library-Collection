:- module(
  turtle_parse,
  [
    statement//0,
    test/0,
    turtle/1, % +File:atom
    turtleDoc//0
  ]
).

/** <module> Turtle

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

:- use_module(turtle(turtle_terminals)).



% PREDICATES

turtle(File):-
  setup_call_cleanup(
    open(File, read, Stream),
    turtle_stream(Stream),
    close(Stream)
  ).

turtle_stream(Stream):-
  read_stream_to_codes(Stream, Codes),
  phrase(turtleDoc, Codes).



% TEST

test:-
  forall(
    between(0, 30, N),
    test(N)
  ).

test(N):-
  (
    N =< 9
  ->
    format(atom(Base), 'test-0~d', [N])
  ;
    format(atom(Base), 'test-~d', [N])
  ),
  absolute_file_name(turtle(Base), File, [access(read),extensions([ttl])]),
  turtle(File),
  print_message(information, test_successful(File)).



% GRAMMAR

%! base// .
% ~~~{.abnf}
% [5]   base ::= '@base' IRIREF '.'
% ~~~

base --> `@base`, b, 'IRIREF', b, `.`.


%! 'BlankNode'// .
% ~~~{.ebnf}
% [137s]   BlankNode ::= BLANK_NODE_LABEL | ANON
% ~~~

'BlankNode' --> 'BLANK_NODE_LABEL'.
'BlankNode' --> 'ANON'.


%! blankNodePropertyList// .
% ~~~{.ebnf}
% [14]   blankNodePropertyList ::= '[' predicateObjectList ']'
% ~~~

blankNodePropertyList --> `[`, b, predicateObjectList, b, `]`.


%! 'BooleanLiteral'// .
% ~~~{.ebnf}
% [133s]   BooleanLiteral ::= 'true' | 'false'
% ~~~

'BooleanLiteral' --> `true`.
'BooleanLiteral' --> `false`.


%! collection// .
% ~~~{.ebnf}
% [15]   collection ::= '(' object* ')'
% ~~~

collection --> `(`, b, 'object*', b, `)`.

'object*' --> object, b, 'object*'.
'object*' --> [].


%! directory// .
% ~~~{.ebnf}
% [3]   directive ::=		prefixID | base | sparqlPrefix | sparqlBase
% ~~~

directive --> prefixID.
directive --> base.
directive --> sparqlPrefix.
directive --> sparqlBase.


%! iri// .
% ~~~{.ebnf}
% [135s]   iri ::= IRIREF | PrefixedName
% ~~~

iri --> 'IRIREF'.
iri --> 'PrefixedName'.


%! literal// .
% ~~~{.ebnf}
% [13]   literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
% ~~~

literal --> 'RDFLiteral'.
literal --> 'NumericLiteral'.
literal --> 'BooleanLiteral'.


%! 'NumericLiteral'// .
% ~~~{.ebnf}
% [16]   NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
% ~~~

'NumericLiteral' --> 'DOUBLE'.
'NumericLiteral' --> 'DECIMAL'.
'NumericLiteral' --> 'INTEGER'.


%! object// .
% ~~~{.ebnf}
% [12]   object ::= iri | BlankNode | collection | blankNodePropertyList |
%                   literal
% ~~~

object --> iri.
object --> 'BlankNode'.
object --> collection.
object --> blankNodePropertyList.
object --> literal.


%! objectList// .
% ~~~{.ebnf}
% [8]   objectList ::= object (',' object)*
% ~~~

objectList --> object, 'objectList_2*'.

'objectList_2*' --> b, `,`, b, object, 'objectList_2*'.
'objectList_2*' --> [].


%! predicate// .
% ~~~{.ebnf}
% [11]   predicate ::= iri
% ~~~

predicate --> iri.


%! predicateObjectList// .
% ~~~{.ebnf}
% [7]   predicateObjectList ::= verb objectList (';' (verb objectList)?)*
% ~~~

predicateObjectList --> verb, b, objectList, 'predicateObjectList_3*'.

'predicateObjectList_3*' --> b, `;`, (`` ; b, verb, b, objectList), 'predicateObjectList_3*'.
'predicateObjectList_3*' --> [].


%! 'PrefixedName'// .
% ~~~{.EBNF}
% [136s]   PrefixedName ::= PNAME_LN | PNAME_NS
% ~~~

'PrefixedName' --> 'PNAME_LN'.
'PrefixedName' --> 'PNAME_NS'.


%! prefixID// .
% ~~~{.abnf}
% [4]			prefixID ::= '@prefix' PNAME_NS IRIREF '.'
% ~~~

prefixID --> `@prefix`, b, 'PNAME_NS', b, 'IRIREF', b, `.`.


%! 'RDFLiteral'// .
% ~~~{.ebnf}
% [128s]   RDFLiteral ::= String (LANGTAG | '^^' iri)?
% ~~~

'RDFLiteral' --> 'String', (`` ; 'LANGTAG' ; `^^`, iri).


%! sparqlBase// .
% ~~~{.ebnf}
% [5s]   sparqlBase ::= "BASE" IRIREF
% ~~~
%
% @tbd Make `BASE` case-insensitive.

sparqlBase --> `BASE`, b, 'IRIREF'.


%! sparqlPrefix// .
% ~~~{.ebnf}
% [6s]   sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ~~~
%
% @tbd Make `PREFIX` case-insensitive.

sparqlPrefix --> `PREFIX`, b, 'PNAME_NS', b, 'IRIREF'.


%! statement// .
% ~~~{.ebnf}
% [2]		  statement ::=		directive | triples '.'
% ~~~

statement --> comment.
statement --> directive.
statement --> triples, b, `.`.


%! subject// .
% ~~~{.ebnf}
% [10]   subject ::= iri | BlankNode | collection
% ~~~

subject --> iri.
subject --> 'BlankNode'.
subject --> collection.


%! 'String'// .
% ~~~{.ebnf}
% [17]   String ::= STRING_LITERAL_QUOTE |
%                   STRING_LITERAL_SINGLE_QUOTE |
%                   STRING_LITERAL_LONG_SINGLE_QUOTE |
%                   STRING_LITERAL_LONG_QUOTE
% ~~~

'String' --> 'STRING_LITERAL_QUOTE'.
'String' --> 'STRING_LITERAL_SINGLE_QUOTE'.
'String' --> 'STRING_LITERAL_LONG_SINGLE_QUOTE'.
'String' --> 'STRING_LITERAL_LONG_QUOTE'.


%! triples// .
% ~~~{.ebnf}
% [6]   triples ::= subject predicateObjectList |
%                   blankNodePropertyList predicateObjectList?
% ~~~

triples --> subject, b, predicateObjectList.
triples --> blankNodePropertyList, (b, 'predicateObjectList'; ``).


%! turtleDoc// .
% ~~~{.ebnf}
% [1]   turtleDoc ::=		statement*
% ~~~

turtleDoc --> 'statement*'.

'statement*' --> statement, !, b, 'statement*'.
'statement*' --> [].


%! verb// .
% ~~~{.ebnf}
% [9]   verb ::= predicate | 'a'
% ~~~

verb --> `a`.
verb --> predicate.



% HELPERS

b --> blanks, comment, !, b.
b --> blanks.

comment --> `#`, string(_), newline.

newline --> [10].

:- multifile(prolog:message//1).

prolog:message(test_successful(File)):-
  ['Test successful for file ',File].

