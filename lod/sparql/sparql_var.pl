:- module(
  sparql_var,
  [
    'Var'//1, % ?VarName:atom
    'VAR1'//1, % ?VarName:atom
    'VAR2'//1, % ?VarName:atom
    'VARNAME'//1 % ?VarName:atom
  ]
).

/** <module. SPARQL Variable

Grammar for variables in SPARQL.

@author Wouter Beek
@version 2014/08
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).

:- use_module(sparql(sparql_char)).



%! 'VAR1'(?VarName:atom)// .
% ~~~{.ebnf}
% VAR1 ::= '?' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [143].

'VAR1'(VarName) -->
  "?",
  'VARNAME'(VarName).



%! 'VAR2'(?VarName:atom)// .
% ~~~{.ebnf}
% VAR2 ::= '$' VARNAME
% ~~~
%
% @compat SPARQL 1.1 Query [144].

'VAR2'(VarName) -->
  "$",
  'VARNAME'(VarName).



%! 'VARNAME'(?VarName:atom)// .
% ~~~{.ebnf}
% VARNAME ::= ( PN_CHARS_U | [0-9] )
%             ( PN_CHARS_U | [0-9] | #x00B7 |
%               [#x0300-#x036F] | [#x203F-#x2040] )*
% ~~~
%
% @compat SPARQL 1.1 Query [166].

'VARNAME'(VarName) -->
  dcg_atom_codes('VARNAME_codes', VarName).

'VARNAME_codes'([H|T]) -->
  'VARNAME_first'(H),
  'VARNAME_rest*'(T).

'VARNAME_first'(C) -->
  'PN_CHARS_U'(C).
'VARNAME_first'(C) -->
  decimal_digit(C).

'VARNAME_rest*'([H|T]) -->
  'VARNAME_rest'(H),
  'VARNAME_rest*'(T).

'VARNAME_rest'(C) --> 'PN_CHARS_U'(C).
'VARNAME_rest'(C) --> decimal_digit(C).
'VARNAME_rest'(C) --> hex_code('B7', C).
'VARNAME_rest'(C) --> between_hex('300', '36F', C).
'VARNAME_rest'(C) --> between_hex('203F', '2040', C).



%! 'Var'(?VarName:atom)// is det.
% A query variable is marked by the use of either `?` or `$`.
% The `?` or `$` is not part of the variable name.
%
% In a query, `$abc` and `?abc` identify the same variable.
%
% ~~~{.ebnf}
% Var ::= VAR1 | VAR2
% ~~~
%
% @compat SPARQL 1.1 Query [108].

'Var'(VarName) --> 'VAR1'(VarName).
'Var'(VarName) --> 'VAR2'(VarName).
