:- module(
  xml_word,
  [
    'EmptyElemTag'//1, % ?Name:atom
    'ETag'//1, % ?Name:atom
    'Name'//1, % ?Name:atom
    'Names'//1, % ?Names:list(atom)
    'Nmtoken'//1, % ?Token:atom
    'Nmtokens'//1, % ?Tokens:list(atom)
    'STag'//1 % ?Name:atom
  ]
).

/** <module> XML word

@author Wouter Beek
@see http://www.w3.org/TR/REC-xml/
@version 2014/03-2014/05
*/

:- use_module(xml(xml_datatypes)).



%! 'EmptyElemTag'(?Name:atom)// .
% An empty XML element.
% ~~~{.ebnf}
% [44]    EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%         [WFC: Unique Att Spec]
% ~~~
%
% @tbd Add support for attributes.

'EmptyElemTag'(Name) -->
  `<`,
  'Name'(Name),
  (`` ; 'S'),
  `/>`, !.


%! 'ETag'(Name)// .
% The end tag of an XML element.
%
% ~~~{.ebnf}
% [42]    ETag ::= '</' Name S? '>'
% ~~~

'ETag'(Name) -->
  `</`,
  'Name'(Name),
  (`` ; 'S'),
  `>`, !.


%! 'Name'(?Name:atom)//
% A **XML Name** is an Nmtoken with a restricted set of initial characters.
%
% Disallowed initial characters for names include digits, diacritics,
% the full stop and the hyphen.
%
% ~~~{.bnf}
% Name ::= NameStartChar (NameChar)*
% ~~~
%
% ## Reserved names
%
% Names beginning with `(x,m,l)` are reserved for standardization in this
% or future versions of this specification.
%
% ## XML Namespaces
%
% The Namespaces in XML Recommendation assigns a meaning to names containing
% colon characters. Therefore, authors should not use the colon in XML names
% except for namespace purposes, but XML processors must accept the colon as
% a name character.
%
% @compat XML 1.0.5 [5].
% @compat XML 1.1.2 [5].

'Name'(Name) -->
  {nonvar(Name)}, !,
  {atom_codes(Name, Codes)},
  'Name'_(Codes).
'Name'(Name) -->
  'Name'_(Codes),
  {atom_codes(Name, Codes)}.

'Name'_([H|T]) -->
  'NameStartChar'(H),
  'NameChar*'([T]).


%! 'Names'(?Names:list(atom))// .
% ~~~{.ebnf}
% Names ::= Name (#x20 Name)*
% ~~~
%
% @compat XML 1.0.5 [6].
% @compat XML 1.1.2 [6].

'Names'([H|T]) -->
  'Name'(H),
  '(#x20 Name)*'(T).

'(#x20 Name)*'([]) --> [].
'(#x20 Name)*'([H|T]) -->
  ` `,
  'Name'(H),
  '(#x20 Name)*'(T).


%! 'Nmtoken'(?Token:atom)// .
% ~~~{.ebnf}
% Nmtoken ::= (NameChar)+
% ~~~
%
% @compat XML 1.0.5 [7].
% @compat XML 1.1.2 [7].

'Nmtoken'(Token) -->
  {nonvar(Token)}, !,
  {atom_codes(Token, Codes)},
  'Nmtoken_'(Codes).
'Nmtoken'(Token) -->
  'Nmtoken_'(Codes),
  {atom_codes(Token, Codes)}.

'Nmtoken_'([H|T]) -->
  'NameChar'(H),
  'NameChar*'(T).

'NameChar*'([]) --> [].
'NameChar*'([H|T]) -->
  'NameChar'(H),
  'NameChar*'(T).


%! 'Nmtokens'(?Tokens:list(atom))// .
% ~~~{.ebnf}
% Nmtokens ::= Nmtoken (#x20 Nmtoken)*
% ~~~
%
% @compat XML 1.0.5 [8].
% @compat XML 1.1.2 [8].

'Nmtokens'([H|T]) -->
  'Nmtoken'(H),
  '(#x20 Nmtoken)*'(T).

'(#x20 Nmtoken)*'([]) --> [].
'(#x20 Nmtoken)*'([H|T]) -->
  ` `,
  'Nmtoken'(H),
  '(#x20 Nmtoken)*'(T).


% 'STag'(?Name:atom)// .
% The start tag of an XML element.
%
% ~~~{.ebnf}
% [40]    STag ::= '<' Name (S Attribute)* S? '>'
%         [WFC: Unique Att Spec]
% ~~~
%
% @tbd Add support for attributes.

'STag'(Name) -->
  `<`,
  'Name'(Name),
  (`` ; 'S'),
  `>`, !.

