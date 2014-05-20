:- module(
  sparql_update,
  [
    sparql_update/1 % +Graph:atom
  ]
).

/** <module> SPARQL update

Support for the SPARQL 1.1 Update recommendation.

@author Wouter Beek
@version 2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).



%! 'GraphTerm'// .
% ~~~{.ebnf}
% [109]    GraphTerm ::= iri |
%                        RDFLiteral |
%                        NumericLiteral |
%                        BooleanLiteral |
%                        BlankNode |
%                        NIL
% ~~~

'GraphTerm' --> iri.
'GraphTerm' --> 'RDFLiteral'.
'GraphTerm' --> 'NumericLiteral'.
'GraphTerm' --> 'BooleanLiteral'.
'GraphTerm' --> 'BlankNode'.
'GraphTerm' --> 'NIL'.


%! 'PN_CHARS_U'// .
% ~~~{.ebnf}
% [165]    PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ~~~

'PN_CHARS_U' --> 'PN_CHARS_BASE'.
'PN_CHARS_U' --> `_`.


%! 'QuadData'(Data)// .
% ~~~{.ebnf}
% [49]    QuadData ::= '{' Quads '}'
% ~~~

'QuadData' -->
  bracketed(curly, 'Quads').


%! 'Quads'(+Data)// .
% ~~~{.ebnf}
% [50]    Quads ::= TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
% ~~~

'Quads' -->
  (`` ; 'TriplesTemplate'),
  'Quads1*'.

'Quads1*' --> [].
'Quads1*' -->
  'QuadsNotTriples',
  (`` ; `.`),
  (`` ; 'TriplesTemplate'),
  'Quads1*'.


%! 'TriplesSameSubject'// .
% ~~~{.ebnf}
% [75]    TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty |
%                                TriplesNode PropertyList
% ~~~

'TriplesSameSubject' -->
  'VarOrTerm',
  'PropertyListNotEmpty'.
'TriplesSameSubject' -->
  'TriplesNode',
  'PropertyList'.


% ~~~{.ebfn}
% [52]    TriplesTemplate ::= TriplesSameSubject ( '.' TriplesTemplate? )?
% ~~~

'TriplesTemplate' -->
  'TriplesSameSubject',
  (`` ; `.`, (`` ; 'TriplesTemplate')).


%! 'Var'// .
% ~~~{.ebnf}
% [108]    Var ::= VAR1 | VAR2
% ~~~

'Var' --> 'VAR1'.
'Var' --> 'VAR2'.

%! 'VAR1'// .
% ~~~{.ebnf}
% [143]    VAR1 ::= '?' VARNAME
% ~~~

'VAR1' --> `?`, 'VARNAME'.


%! 'VAR2'// .
% ~~~{.ebnf}
% [144]    VAR2 ::= '$' VARNAME
% ~~~

'VAR2' --> `$`, 'VARNAME'.

%! 'VARNAME'// .
% ~~~{.ebnf}
% [166]    VARNAME ::= ( PN_CHARS_U |
%                        [0-9] )
%                      ( PN_CHARS_U |
%                        [0-9] |
%                        #x00B7 |
%                        [#x0300-#x036F] |
%                        [#x203F-#x2040] )*
% ~~~

'VARNAME' -->
  (
    'PN_CHARS_U'
  ;
    decimal_digit
  ),
  (
    'PN_CHARS_U'
  ;
    decimal_digit
  ;
    hex_code('B7')
  ;
    between_hex('300', '36F')
  ;
    between_hex('203F', '2040')
  ).


%! 'VarOrTerm'// .
% ~~~{.ebnf}
% [106]    VarOrTerm ::= Var | GraphTerm
% ~~~

'VarOrTerm' --> 'Var'.
'VarOrTerm' --> 'GraphTerm'.

