:- module(
  sparql_expression,
  [
  ]
).

/** <module> SPARQL Expression

Grammar for SPARQL expressions.

@author Wouter Beek
@version 2014/08
*/

:- use_module(dcg(dcg_content)).

:- use_module(sparql(sparql_char)).
:- use_module(sparql(sparql_literal)).
:- use_module(sparql(sparql_word)).



%! 'AdditiveExpression'// is det.
% ~~~{.ebnf}
% AdditiveExpression ::= MultiplicativeExpression
%                        ( '+' MultiplicativeExpression
%                        | '-' MultiplicativeExpression
%                        | ( NumericLiteralPositive | NumericLiteralNegative )
%                          ( ( '*' UnaryExpression )
%                            | ( '/' UnaryExpression )
%                          )*
%                        )*
% ~~~
%
% @compat SPARQL 1.1 Query [116].

'AdditiveExpression' -->
  'MultiplicativeExpression'.
  additive_expression0.

additive_expression0 --> [].
additive_expression0 -->
  ws+, "+", ws+,
  'MultiplicativeExpression',
  additive_expression0.
additive_expression0 -->
  ws+, "-", ws+,
  'MultiplicativeExpression',
  additive_expression0.
additive_expression0 -->
  ws+, ('NumericLiteralPositive' ; 'NumericLiteralNegative'),
  unary_expressions,
  additive_expression0.

unary_expressions --> [].
unary_expressions -->
  ws+, "*", ws+,
  'UnaryExpression',
  unary_expressions.
unary_expressions -->
  ws+, "/", ws+,
  'UnaryExpression',
  unary_expressions.



%! 'Aggregate'// is det.
% ~~~{.ebnf}
% Aggregate ::=   'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')'
%               | 'SUM' '(' 'DISTINCT'? Expression ')'
%               | 'MIN' '(' 'DISTINCT'? Expression ')'
%               | 'MAX' '(' 'DISTINCT'? Expression ')'
%               | 'AVG' '(' 'DISTINCT'? Expression ')'
%               | 'SAMPLE' '(' 'DISTINCT'? Expression ')'
%               | 'GROUP_CONCAT'
%                 '('
%                 'DISTINCT'?
%                 Expression
%                 ( ';' 'SEPARATOR' '=' String )?
%                 ')' 
% ~~~
%
% @compat SPARQL 1.1 Query [127].

'Aggregate' -->
  "COUNT", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    ( "*" ; 'Expression' )
  )).
'Aggregate' -->
  "SUM", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression'
  )).
'Aggregate' -->
  "MIN", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression'
  )).
'Aggregate' -->
  "MAX", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression'
  )).
'Aggregate' -->
  "AVG", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression'
  )).
'Aggregate' -->
  "SAMPLE", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression'
  )).
'Aggregate' -->
  "GROUP_CONCAT", ws+,
  bracketed(round, (
    ("DISTINCT", ws+ ; ""),
    'Expression', ws+,
    ((";", ws+, "SEPARATOR", ws*, "=", ws*, 'String') ; "")
  )).



%! 'BuiltInCall'// is det.
% ~~~{.ebnf}
% BuiltInCall ::=   Aggregate
%                 | 'STR' '(' Expression ')'
%                 | 'LANG' '(' Expression ')'
%                 | 'LANGMATCHES' '(' Expression ',' Expression ')'
%                 | 'DATATYPE' '(' Expression ')'
%                 | 'BOUND' '(' Var ')'
%                 | 'IRI' '(' Expression ')'
%                 | 'URI' '(' Expression ')'
%                 | 'BNODE' ( '(' Expression ')' | NIL )
%                 | 'RAND' NIL
%                 | 'ABS' '(' Expression ')'
%                 | 'CEIL' '(' Expression ')'
%                 | 'FLOOR' '(' Expression ')'
%                 | 'ROUND' '(' Expression ')'
%                 | 'CONCAT' ExpressionList
%                 | SubstringExpression
%                 | 'STRLEN' '(' Expression ')'
%                 | StrReplaceExpression
%                 | 'UCASE' '(' Expression ')'
%                 | 'LCASE' '(' Expression ')'
%                 | 'ENCODE_FOR_URI' '(' Expression ')'
%                 | 'CONTAINS' '(' Expression ',' Expression ')'
%                 | 'STRSTARTS' '(' Expression ',' Expression ')'
%                 | 'STRENDS' '(' Expression ',' Expression ')'
%                 | 'STRBEFORE' '(' Expression ',' Expression ')'
%                 | 'STRAFTER' '(' Expression ',' Expression ')'
%                 | 'YEAR' '(' Expression ')'
%                 | 'MONTH' '(' Expression ')'
%                 | 'DAY' '(' Expression ')'
%                 | 'HOURS' '(' Expression ')'
%                 | 'MINUTES' '(' Expression ')'
%                 | 'SECONDS' '(' Expression ')'
%                 | 'TIMEZONE' '(' Expression ')'
%                 | 'TZ' '(' Expression ')'
%                 | 'NOW' NIL
%                 | 'UUID' NIL
%                 | 'STRUUID' NIL
%                 | 'MD5' '(' Expression ')'
%                 | 'SHA1' '(' Expression ')'
%                 | 'SHA256' '(' Expression ')'
%                 | 'SHA384' '(' Expression ')'
%                 | 'SHA512' '(' Expression ')'
%                 | 'COALESCE' ExpressionList
%                 | 'IF' '(' Expression ',' Expression ',' Expression ')'
%                 | 'STRLANG' '(' Expression ',' Expression ')'
%                 | 'STRDT' '(' Expression ',' Expression ')'
%                 | 'sameTerm' '(' Expression ',' Expression ')'
%                 | 'isIRI' '(' Expression ')'
%                 | 'isURI' '(' Expression ')'
%                 | 'isBLANK' '(' Expression ')'
%                 | 'isLITERAL' '(' Expression ')'
%                 | 'isNUMERIC' '(' Expression ')'
%                 | RegexExpression
%                 | ExistsFunc
%                 | NotExistsFunc
% ~~~
%
% @compat SPARQL 1.1 Query [121].

'BuiltInCall' -->
  'Aggregate'.
'BuiltInCall' -->
  "STR", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' --> 
  "LANG", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "LANGMATCHES", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "DATATYPE", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "BOUND", ws+,
  bracketed(round, 'Var').
'BuiltInCall' -->
  "IRI", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "URI", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "BNODE", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "BNODE", ws+,
  'NIL'.
'BuiltInCall' -->
  "RAND", ws+,
  'NIL'.
'BuiltInCall' -->
  "ABS", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "CEIL", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "FLOOR", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "ROUND", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "CONCAT", ws+,
  'ExpressionList'.
'BuiltInCall' -->
  'SubstringExpression'.
'BuiltInCall' -->
  "STRLEN", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  'StrReplaceExpression'.
'BuiltInCall' -->
  "UCASE", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "LCASE", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "ENCODE_FOR_URI", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "CONTAINS", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "STRSTARTS", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "STRENDS", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "STRBEFORE", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "STRAFTER",
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "YEAR", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "MONTH", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "DAY", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "HOURS", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "MINUTES", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "SECONDS", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "TIMEZONE", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "TZ", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "NOW", ws+,
  'NIL'.
'BuiltInCall' -->
  "UUID", ws+,
  'NIL'.
'BuiltInCall' -->
  "STRUUID", ws+,
  'NIL'.
'BuiltInCall' -->
  "MD5", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "SHA1", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "SHA256", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "SHA384", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "SHA512", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "COALESCE", ws+,
  'ExpressionList'.
'BuiltInCall' -->
  "IF", ws+,
  bracketed(round, (
    'Expression', comma_separator, 'Expression', comma_separator, 'Expression'
  )).
'BuiltInCall' -->
  "STRLANG", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "STRDT", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "sameTerm", ws+,
  bracketed(round, ('Expression', comma_separator, 'Expression')).
'BuiltInCall' -->
  "isIRI", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "isURI", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "isBLANK", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "isLITERAL", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  "isNUMERIC", ws+,
  bracketed(round, 'Expression').
'BuiltInCall' -->
  'RegexExpression'.
'BuiltInCall' -->
  'ExistsFunc'.
'BuiltInCall' -->
  'NotExistsFunc'.



%! 'BrackettedExpression'// is det.
% ~~~{.ebnf}
% BrackettedExpression ::= '(' Expression ')'
% ~~~
%
% @compat SPARQL 1.1 Query [120].

'BrackettedExpression' -->
  bracketed(round, 'Expression').



%! 'ConditionalAndExpression'// is det.
% ~~~{.ebnf}
%   	ConditionalAndExpression	  ::=  	ValueLogical ( '&&' ValueLogical )*
% ~~~
%
% @compat SPARQL 1.1 Query [112].

'ConditionalAndExpression' -->
  'ValueLogical'.
'ConditionalAndExpression' -->
  ws+, "&&", ws+,
  'ValueLogical',
  'ConditionalAndExpression'.



%! 'ConditionalOrExpression'// is det.
% ~~~{.ebnf}
% ConditionalOrExpression ::= ConditionalAndExpression
%                             ( '||' ConditionalAndExpression )*
% ~~~
%
% @compat SPARQL 1.1 Query [111].

'ConditionalOrExpression' -->
  'ConditionalAndExpression'.
'ConditionalOrExpression' -->
   ws+,"||", ws+,
  'ConditionalAndExpression',
  'ConditionalOrExpression'.


%! 'ExistsFunc'// is det.
% ~~~{.ebnf}
% ExistsFunc ::= 'EXISTS' GroupGraphPattern
% ~~~
%
% @compat SPARQL 1.1 Query [125].

'ExistsFunc' -->
  "EXISTS",
  'GroupGraphPattern'.



%! 'Expression'// .
% ~~~{.ebnf}
% Expression ::= ConditionalOrExpression
% ~~~
%
% @compat SPARQL 1.1 Query [110].

'Expression' --> 'ConditionalOrExpression'.



%! 'ExpressionList'// is det.
% ~~~{.ebnf}
% ExpressionList ::= NIL | '(' Expression ( ',' Expression )* ')'
% ~~~
%
% @compat SPARQL 1.1 Query [72].

'ExpressionList' -->
  'NIL'
'ExpressionList' -->
  bracketed(round, comma_separated('Expression')).



%! 'MultiplicativeExpression'// is det.
% ~~~{.ebnf}
% MultiplicativeExpression ::= UnaryExpression
%                              ( '*' UnaryExpression | '/' UnaryExpression )*
% ~~~
%
% @compat SPARQL 1.1 Query [117].

'MultiplicativeExpression' -->
  'UnaryExpression'.
'MultiplicativeExpression' -->
  'UnaryExpression', ws+,
  ( "*" ; "/"), ws+,
  'MultiplicativeExpression'.



%! 'NotExistsFunc'// is det.
% ~~~{.ebnf}
% NotExistsFunc ::= 'NOT' 'EXISTS' GroupGraphPattern
% ~~~
%
% @compat SPARQL 1.1 Query [126].

'NotExistsFunc' -->
  "NOT", 'WS+',
  'ExistsFunc'.



%! 'NumericExpression'// is det.
% ~~~{.ebnf}
% NumericExpression ::= AdditiveExpression
% ~~~
%
% @compat SPARQL 1.1 Query [115].

'NumericExpression' --> 'AdditiveExpression'.



%! 'PrimaryExpression'// is det.
% ~~~{.ebnf}
% PrimaryExpression ::=   BrackettedExpression
%                       | BuiltInCall
%                       | iriOrFunction
%                       | RDFLiteral
%                       | NumericLiteral
%                       | BooleanLiteral
%                       | Var
% ~~~
%
% @compat SPARQL 1.1 Query [119].

'PrimaryExpression' --> 'BrackettedExpression'.
'PrimaryExpression' --> 'BuiltInCall'.
'PrimaryExpression' -->  iriOrFunction.
'PrimaryExpression' --> 'RDFLiteral'.
'PrimaryExpression' --> 'NumericLiteral'.
'PrimaryExpression'(boolean(Value)) --> 'BooleanLiteral'(Value).
'PrimaryExpression'(var(VarName)) --> 'Var'(VarName).



%! 'RegexExpression'// is det.
% ~~~{.ebnf}
% RegexExpression ::= 'REGEX' '('
%                       Expression ',' Expression ( ',' Expression )?
%                     ')'
% ~~~
%
% @compat SPARQL 1.1 Query [122].

'RegexExpression' -->
  "REGEX", 'WS+',
  bracketed(round, (
    'Expression', comma_separator, 'Expression'
    (comma_separator, 'Expression' ; "")
  )).



%! 'RelationalExpression''/ is det.
% ~~~{.ebnf}
% RelationalExpression ::= NumericExpression
%                          ( '=' NumericExpression
%                          | '!=' NumericExpression
%                          | '<' NumericExpression
%                          | '>' NumericExpression
%                          | '<=' NumericExpression
%                          | '>=' NumericExpression
%                          | 'IN' ExpressionList
%                          | 'NOT' 'IN' ExpressionList )?
% ~~~
%
% @compat SPARQL 1.1 Update [114].

'RelationalExpression' -->
  'NumericExpression', ws+,
  (
    "=", ws+,
    'NumericExpression'
  ;
    "!=", ws+,
    'NumericExpression'
  ;
    "<", ws+,
    'NumericExpression'
  ;
    ">", ws+,
    'NumericExpression'
  ;
    "<=", ws+,
    'NumericExpression'
  ;
    ">=", ws+,
    'NumericExpression'
  ;
    "IN", ws+,
    'ExpressionList'
  ;
    "NOT", ws+, "IN", ws+,
    'ExpressionList'
  ;
    ""
  ).



%! 'UnaryExpression'// is det.
% ~~~{.ebnf}
% UnaryExpression ::=   '!' PrimaryExpression
%                     | '+' PrimaryExpression
%                     | '-' PrimaryExpression
%                     |     PrimaryExpression
% ~~~
%
% @compat SPARQL 1.1 Query [118].

'UnaryExpression' -->
  "!", ws+,
  'PrimaryExpression'.
'UnaryExpression' -->
  "+", ws+,
  'PrimaryExpression'.
'UnaryExpression' -->
  "-", ws+,
  'PrimaryExpression'.
'UnaryExpression' -->
  'PrimaryExpression'.



%! 'ValueLogical'// is det.
% ~~~{.ebnf}
% ValueLogical ::= RelationalExpression
% ~~~
%
% @compat SPARQL 1.1 Query [113].

'ValueLogical' -->
  'RelationalExpression'.
