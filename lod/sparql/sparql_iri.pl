:- module(
  sparql_iri,
  [
    'IRIREF'//1, % ?Iri:atom
    'PN_LOCAL'//1, % ?LocalPart:atom
    'PN_PREFIX'//1, % ?Prefix:atom
    'PNAME_LN'//1, % ?FullName:pair(atom)
    'PNAME_NS'//1 % ?Prefix:atom
  ]
).

/** <module> SPARQL IRI

Grammar for IRIs in SPARQL.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).

:- use_module(sparql(sparql_char)).



%! iri(?Iri:atom)// is det.
%
%
% ~~~{.ebnf}
% iri ::= IRIREF | PrefixedName
% ~~~
%
% @compat SPARQL 1.0 [135]
% @compat SPARQL 1.1 Query [136]
% @compat Turtle 1.1 [135a]

iri(Iri) -->
  'IRIREF'(Iri).
iri(Iri) -->
  'PrefixedName'(Iri).



%! 'IRIREF'(?Iri:atom)// .

% ~~~{.ebnf}
% IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ~~~
%
% @compat SPARQL 1.1 Query [139].
% @compat RFC3987 IRI
% @tbd What about DELETE (decimal 127)?

'IRIREF'(Iri) -->
  dcg_atom_codes(bracketed(angular, 'IRIREF_char*'(Codes)), Iri).

'IRIREF_char*'([H|T]) -->
  'IRIREF_char'(H),
  'IRIREF_char*'(T).
'IRIREF_char*'([]) --> [].

'IRIREF_char'(C) --> control(C), !, {fail}.
'IRIREF_char'(C) --> angular_bracket(C), !, {fail}.
'IRIREF_char'(C) --> double_quote(C), !, {fail}.
'IRIREF_char'(C) --> curly_bracket(C), !, {fail}.
'IRIREF_char'(C) --> vertical_bar(C), !, {fail}.
'IRIREF_char'(C) --> carret(C), !, {fail}.
'IRIREF_char'(C) --> tilde(C), !, {fail}.
'IRIREF_char'(C) --> backslash(C), !, {fail}.
'IRIREF_char'(C) --> [C].



%! 'PN_LOCAL'(?LocalPart:atom)// is det.
% The **local part** of a prefixed name.
%
% ~~~{.ebnf}
% PN_LOCAL ::= ( PN_CHARS_U | ':' | [0-9] | PLX )
%              (
%                ( PN_CHARS | '.' | ':' | PLX )*
%                ( PN_CHARS | ':' | PLX )
%              )?
% ~~~
%
% @compat SPARQL 1.0 [168].
% @compat SPARQL 1.1 Query [169].
% @compat Turtle 1.1 [168s].

'PN_LOCAL'(LocalPart) -->
  dcg_atom_codes('PN_LOCAL_codes', LocalPart).

'PN_LOCAL_codes'([H|T]) -->
  'PN_LOCAL_1'(H),
  ('PN_LOCAL_2'(T) ; {T = []}).

'PN_LOCAL_1'(C) --> 'PN_CHARS_U'(C).
'PN_LOCAL_1'(C) --> colon(C).
'PN_LOCAL_1'(C) --> decimal_digit(C).
'PN_LOCAL_1'(C) --> 'PLX'(C).

'PN_LOCAL_2'([H|T]) -->
  'PN_LOCAL_2a'(H),
  'PN_LOCAL_2'(T).
'PN_LOCAL_2'([Last]) -->
  'PN_LOCAL_2b'(Last).

'PN_LOCAL_2a'(C) --> dot(C).
'PN_LOCAL_2a'(C) --> 'PN_LOCAL_2b'(C).

'PN_LOCAL_2b'(C) --> 'PN_CHARS'(C).
'PN_LOCAL_2b'(C) --> colon(C).
'PN_LOCAL_2b'(C) --> 'PLX'(C).



%! 'PN_PREFIX'(?Prefix:atom)// .
% The **prefix label** used in *prefixed names*.
%
% ~~~{.ebnf}
% PN_PREFIX ::= PN_CHARS_BASE ( ( PN_CHARS | '.' )* PN_CHARS )?
% ~~~
%
% @compat SPARQL 1.0 [167].
% @compat SPARQL 1.1 Query [168].
% @compat Turtle 1.1 [167s].

'PN_PREFIX'(Prefix) -->
  dcg_atom_codes('PN_PREFIX_codes', Prefix).

'PN_PREFIX_codes'([H|T]) -->
  'PN_CHARS_BASE'(H),
  ("" ; 'PN_PREFIX_rest'(T)).

'PN_PREFIX_rest'([H|T]) -->
  ('PN_CHARS'(H) ; dot(H)),
  'PN_PREFIX_rest'(T).
'PN_PREFIX_rest'([Last]) -->
  'PN_CHARS'(Last).



%! 'PNAME_LN'(?FullName:pair(atom))// .
% ~~~{.ebnf}
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ~~~
%
% @compat SPARQL 1.0 [140].
% @compat SPARQL 1.1 Query [141].
% @compat Turtle 1.1 [140s].

'PNAME_LN'(Prefix-Local) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local).



%! 'PNAME_NS'(?Prefix:atom)// is det.
% The empty string is also a prefix label.
%
% ~~~{.ebnf}
% PNAME_NS ::= PN_PREFIX? ':'
% ~~~
%
% @compat SPARQL 1.0 [139].
% @compat SPARQL 1.1 Query [140].
% @compat Turtle 1.1 [139s].

'PNAME_NS'(Prefix) -->
  'PN_PREFIX'(Prefix),
  ":".
'PNAME_NS'('') -->
  ":".



%! 'PrefixedName'(?Iri:iri)// is det.
% A **prefixed name** is a *prefix label* and a *local part*,
% separated by a colon.
% The prefixed name is mapped to an IRI
% by concatenating the IRI associated by the prefix
% and the local part.
%
% ~~~{.ebnf}
% PrefixedName ::= PNAME_LN | PNAME_NS
% ~~~
%
% @compat SPARQL 1.0 [136].
% @compat SPARQL 1.1 Query [137].
% @compat Turtle 1.1 [136s].

'PrefixedName'(Iri) -->
  {var(Iri)}, !,
  (
    'PNAME_LN'(Prefix-LocalName), !
  ;
    'PNAME_NS'(Prefix),
    {LocalName = ''}
  ),
  {rdf_global_id(Prefix:LocalName, Iri)}, !
'PrefixedName'(Iri) -->
  {rdf_global_id(Prefix:LocalName, Iri)},
  (
    {LocalName == ''}
  ->
    'PNAME_NS'(Prefix)
  ;
    'PNAME_LN'(Prefix-LocalName)
  ).
