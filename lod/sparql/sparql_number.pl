:- module(
  sparql_number,
  [
    'DECIMAL'//1, % ?Value:float
    'DECIMAL_NEGATIVE'//1, % ?Value:float
    'DECIMAL_POSITIVE'//1, % ?Value:float
    'DOUBLE'//1, % ?Value:float
    'DOUBLE_NEGATIVE'//1, % ?Value:float
    'DOUBLE_POSITIVE'//1, % ?Value:float
    'EXPONENT'//1, % ?Value:integer
    'INTEGER'//1, % ?Value:integer
    'INTEGER_POSITIVE'//1, % ?Value:integer
    'INTEGER_NEGATIVE'//1 % ?Value:integer
  ]
).

/** <module> SPARQL number

DCGs for number-denoting expressions defined in SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(generics(typecheck)).
:- use_module(math(float_ext)).



%! 'DECIMAL'(?Value:float)// .
% ~~~{.ebnf}
% DECIMAL ::= [0-9]* '.' [0-9]+
% ~~~
%
% @compat SPARQL 1.1 Query [147].

'DECIMAL'(N) -->
  {nonvar(N)},
  {float_parts(N, N1, N2)},
  'integer?'(N1),
  `.`,
  integer(N2).
'DECIMAL'(N) -->
  {var(N)},
  'integer?'(N1),
  `.`,
  decimal_fraction(N2),
  N is N1 + N2.



%! 'DECIMAL_NEGATIVE'(?Value:float)// .
% ~~~{.ebnf}
% DECIMAL_NEGATIVE ::= '-' DECIMAL
% ~~~
%
% @compat SPARQL 1.1 Query [153].

'DECIMAL_NEGATIVE'(N) -->
  {negative_float(N)},
  `-`,
  'DECIMAL'(N).
'DECIMAL_NEGATIVE'(N) -->
  {var(N)},
  `-`,
  'DECIMAL'(N1),
  {negative_float(N1)}.



%! 'DECIMAL_POSITIVE'(?Value:float)// .
% ~~~{.ebnf}
% DECIMAL_POSITIVE ::= '+' INTEGER
% ~~~
%
% @compat SPARQL 1.1 Query [149].

'DECIMAL_POSITIVE'(N) -->
  {positive_float(N)},
  `+`,
  'DECIMAL'(N).
'DECIMAL_POSITIVE'(N) -->
  {var(N)},
  `+`,
  'DECIMAL'(N1),
  {positive_float(N1)}.



%! 'DOUBLE'(?Value:float)// .
% ~~~{.ebnf}
% DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT |
%            '.' ([0-9])+ EXPONENT |
%            ([0-9])+ EXPONENT
% ~~~
%
% @compat SPARQL 1.1 Query [148].
% @tbd Add (+) case.

'DOUBLE'(N) -->
  {nonvar(N)},
  ('DOUBLE_1'(N1) ; 'DOUBLE_2'(N1) ; 'DOUBLE_3'(N1)),
  'EXPONENT'(N2),
  {N is N1 * N2}.

'DOUBLE_1'(N) -->
  {nonvar(N)},
  {float_parts(N, N1 N2)},
  integer(N1),
  `.`,
  'integer?'(N2).
'DOUBLE_1'(N) -->
  {var(N)},
  integer(N1),
  `.`,
  'integer?'(N2),
  {float_parts(N, N1 N2)}.

'DOUBLE_2'(N) -->
  {nonvar(N)},
  {float_parts(N, 0, N1)},
  `.`,
  integer(N1).
'DOUBLE_2'(N) -->
  {var(N)},
  `.`,
  integer(N1),
  {float_parts(N, 0, N1)}.

'DOUBLE_3'(N) -->
  integer(N).



%! 'DOUBLE_POSITIVE'(?Value:float)// .
% ~~~{.ebnf}
% DOUBLE_POSITIVE ::= '+' DOUBLE
% ~~~
%
% @compat SPARQL 1.1 Query [151].

'DOUBLE_POSITIVE'(N) -->
  {positive_float(N)},
  `+`,
  'DOUBLE'(N).
'DOUBLE_POSITIVE'(N) -->
  {var(N)},
  `+`,
  'DOUBLE'(N1),
  {positive_float(N1)}.



%! 'DOUBLE_NEGATIVE'(?Value:float)// .
% ~~~{.ebnf}
% DOUBLE_NEGATIVE ::= '-' DOUBLE
% ~~~
%
% @compat SPARQL 1.1 Query [154].

'DOUBLE_NEGATIVE'(N) -->
  {negative_float(N)},
  `-`,
  {N1 is abs(N)}.
  'DOUBLE'(N1),
'DOUBLE_NEGATIVE'(N) -->
  {var(N)},
  `-`,
  'DOUBLE'(N1),
  {N is -1 * N1}.



%! 'EXPONENT'(?Value:float)// .
% ~~~{.ebnf}
% EXPONENT ::= [eE] [+-]? [0-9]+
% ~~~
%
% @compat SPARQL 1.0 [154].
% @compat SPARQL 1.1 Query [155].
% @compat Turtle 1.1 [154s].

'EXPONENT'(N) -->
  {nonvar(N)},
  exponent_sign,
  {Sign is sign(N)},
  'sign?'(Sign),
  {I is abs(N)},
  integer(I).
'EXPONENT'(N) -->
  {var(N)},
  exponent_sign,
  'sign?'(Sign),
  integer(I),
  {N is 10 ** (Sign * I)}.



%! 'INTEGER'(?Value:integer)// .
% ~~~{.ebnf}
% INTEGER ::= [0-9]+
% ~~~
%
% @compat SPARQL 1.1 Query [146].

'INTEGER'(N) -->
  integer(N).



%! 'INTEGER_POSITIVE'(?Value:integer)// .
% ~~~{.ebnf}
% INTEGER_POSITIVE ::= '+' INTEGER
% ~~~
%
% @compat SPARQL 1.1 Query [149].

'INTEGER_POSITIVE'(N) -->
  {positive_integer(N)},
  `+`,
  'INTEGER'(N).
'INTEGER_POSITIVE'(N) -->
  {var(N)},
  `+`,
  'INTEGER'(N1),
  {positive_integer(N1)}.



%! 'INTEGER_NEGATIVE'(?Value:integer)// .
% ~~~{.ebnf}
% INTEGER_NEGATIVE ::= '-' INTEGER
% ~~~
%
% @compat SPARQL 1.1 Query [152].

'INTEGER_NEGATIVE'(N) -->
  {negative_integer(N)},
  `-`,
  {N1 is abs(N)}.
  'INTEGER'(N1),
'INTEGER_NEGATIVE'(N) -->
  {var(N)},
  `-`,
  'INTEGER'(N1),
  {N is -1 * N1}.
