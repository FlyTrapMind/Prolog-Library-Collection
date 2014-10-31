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

/** <module> XML: Word

@author Wouter Beek
@compat http://www.w3.org/TR/REC-xml/
@version 2014/03-2014/05, 2014/10
*/

:- use_module(xml(xml_char)).
:- use_module(xml(xml_datatypes)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_meta)).



%! 'EmptyElemTag'(?Name:atom)// .
% An empty XML element.
% ~~~{.ebnf}
% [44]    EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%         [WFC: Unique Att Spec]
% ~~~
%
% @tbd Add support for attributes.

'EmptyElemTag'(Name) -->
  "<",
  'Name'(Name),
  '?'('S', []),
  "/>".



%! 'ETag'(Name)// .
% The end tag of an XML element.
%
% ~~~{.ebnf}
% [42]    ETag ::= '</' Name S? '>'
% ~~~

'ETag'(Name) -->
  "</",
  'Name'(Name),
  '?'('S', []),
  ">".



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
  dcg_atom_codes('Name_codes', Name).

'Name_codes'([H|T]) -->
  'NameStartChar'(H),
  '*'('NameChar', T, []).



%! 'Names'(?Names:list(atom))// .
% ~~~{.ebnf}
% Names ::= Name (#x20 Name)*
% ~~~
%
% @compat XML 1.0.5 [6].
% @compat XML 1.1.2 [6].

'Names'(Names) -->
  '+'('Name', Names, [separator(space)]).



%! 'Nmtoken'(?Token:atom)// .
% ~~~{.ebnf}
% Nmtoken ::= (NameChar)+
% ~~~
%
% @compat XML 1.0.5 [7].
% @compat XML 1.1.2 [7].

'Nmtoken'(Token) -->
  dcg_atom_codes('Nmtoken_codes', Token).

'Nmtoken_codes'(Codes) -->
  '+'('NameChar', Codes, []).



%! 'Nmtokens'(?Tokens:list(atom))// .
% ~~~{.ebnf}
% Nmtokens ::= Nmtoken (#x20 Nmtoken)*
% ~~~
%
% @compat XML 1.0.5 [8].
% @compat XML 1.1.2 [8].

'Nmtokens'(Tokens) -->
  '+'('Nmtoken', Tokens, [separator(space)]).



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
  "<",
  'Name'(Name),
  '?'('S', []),
  ">", !.

