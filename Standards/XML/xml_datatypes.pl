:- module(
  xml_datatypes,
  [
    xml_boolean//2, % -Tree:compound
                    % ?Value:boolean
    xml_name//1, % ?Name:atom
    xml_namespaced_name//2, % :DCG_Namespace
                            % :DCG_Name
    xml_yes_no//2 % -Tree:compound
                  % ?Boolean:boolean
  ]
).

/** <module> XML_DATATYPES

DCG rules for XML datatypes.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_unicode)).
:- use_module(xml(xml)).

:- meta_predicate(xml_namespaced_name(//,//,?,?)).



xml_boolean(xml_boolean(false), false) --> "false".
xml_boolean(xml_boolean(true), true) --> "true".

%! xml_char
% An **XML Character** is an atomic unit of text specified by ISO/IEC 10646.
%
% ~~~{.bnf}
% Char ::= #x9 |   // Horizontal tab
%          #xA |   // Line feed
%          #xD |   // Carriage return
%          [#x20-#xD7FF] | // Space, punctuation, numbers, letters.
%          [#xE000-#xFFFD] |
%          [#x10000-#x10FFFF]
% ~~~
%
% Avoid comapatibility characters [Unicode, section 2.3].
% Avoid the following characters (control characters,
% permanently undefined Unicode characters):
%
% ~~~{.txt}
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ~~~
%
% @tbd Add Unicode support and make sure the right character ranges
%      are selected.

xml_char(C) --> horizontal_tab(C).
xml_char(C) --> line_feed(C).
xml_char(C) --> carriage_return(C).
xml_char(C) --> dcg_graph(C).

%! xml_name(?Name:atom)//
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
% @see http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name

xml_name(Name) -->
  {nonvar(Name)}, !,
  {atom_codes(Name, Codes)},
  xml_name_(Codes).
xml_name(Name) -->
  xml_name_(Codes),
  {atom_codes(Name, Codes)}.

xml_name_([H1,H2,H3|T]) -->
  xml_name_start_char(H1),
  xml_name_chars([H2,H3|T]),
  {\+ phrase((x,m,l), [H1,H2,H3])}.
xml_name_([H1,H2]) -->
  xml_name_start_char(H1),
  xml_name_char(H2).
xml_name_([H]) -->
  xml_name_start_char(H).

%! xml_name_char(?Char:code)//
% ~~~{.bnf}
% NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
%              [#x203F-#x2040]
% ~~~

xml_name_char(C) --> xml_name_start_char(C).
xml_name_char(C) --> hyphen_minus(C).
xml_name_char(C) --> dot(C).
xml_name_char(C) --> decimal_digit(_N, C).
% #xB7
xml_name_char(C) --> middle_dot(C).
% #x0300-#x036F
xml_name_char(C) --> {between(768, 879, C)}.
% #x203F
xml_name_char(C) --> undertie(C).
% #x2040
xml_name_char(C) --> character_tie(C).

xml_name_chars([H|T]) -->
  xml_name_char(H),
  xml_name_chars(T).
xml_name_chars([]) --> [].

%! xml_name_start_char(?Code:code)//
% ~~~{.bnf}
% NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] |
%                   [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
%                   [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
%                   [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
%                   [#x10000-#xEFFFF]
% ~~~

xml_name_start_char(C) --> colon(C).
xml_name_start_char(C) --> letter(C).
xml_name_start_char(C) --> underscore(C).
% #xC0-#xD6
xml_name_start_char(C) --> {between(192, 214, C)}.
% #xD8-#xF6
xml_name_start_char(C) --> {between(216, 246, C)}.
% #xF8-#x2FF
xml_name_start_char(C) --> {between(248, 767, C)}.
% #x370-#x37D
xml_name_start_char(C) --> {between(880, 893, C)}.
% #x37F-#x1FFF
xml_name_start_char(C) --> {between(895, 8191, C)}.
% #x200C-#x200D
xml_name_start_char(C) --> zero_width_non_joiner(C).
xml_name_start_char(C) --> zero_width_joiner(C).
% #x2070-#x218F
xml_name_start_char(C) --> {between(8304, 8591, C)}.
% #x2C00-#x2FEF
xml_name_start_char(C) --> {between(11264, 12271, C)}.
% #x3001-#xD7FF
xml_name_start_char(C) --> {between(12289, 55295, C)}.
% #xF900-#xFDCF
xml_name_start_char(C) --> {between(63744, 64975, C)}.
% #xFDF0-#xFFFD
xml_name_start_char(C) --> {between(65008, 65533, C)}.
% #x10000-#xEFFFF
xml_name_start_char(C) --> {between(65536, 983039, C)}.

%! xml_namespaced_name(:DCG_Namespace, :DCG_Name)//

xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  {phrase(DCG_Namespace, "")},
  DCG_Name.
xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  DCG_Namespace,
  colon,
  DCG_Name.

%! xml_restricted_char(?Char:code)//
% ~~~{.bnf}
% RestrictedChar ::= [#x1-#x8] |
%                    [#xB-#xC] |
%                    [#xE-#x1F] |
%                    [#x7F-#x84] |
%                    [#x86-#x9F]
% ~~~

xml_restricted_char(C) -->
  xml_char(C),
  % Not a start of heading, start of text, end of text, end of transmission,
  % enquiry, positive_acknowledgement, bell, backspace.
  {\+ between(1, 8, C)},
  % Not a vertical tab, form feed.
  {\+ between(11, 12, C)},
  % Not a shift out, shift in, data link escape, device control (1, 2, 3, 4),
  % negative acknowledgement, synchronous idle, end of transmission block,
  % cancel, end of medium, substitute, escape, file separator,
  % group separator, record separator, unit separator.
  {\+ between(14, 31, C)},
  % Not delete, ...
  {\+ between(127, 132, C)},
  % Not ..
  {\+ between(134, 159, C)}.

%! xml_space
% White space.
%
% ~~~{.bnf}
% S ::= (#x20 | #x9 | #xD | #xA)+   // Any consecutive number of spaces,
%                                   // carriage returns, line feeds, and
%                                   // horizontal tabs.
% ~~~
%
% The presence of carriage_return// in the above production is maintained
% purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either removed
% or replaced by line_feed// (i.e., `#xA`) characters before any other
% processing is done.

xml_space(Space) -->
  {nonvar(Space)}, !,
  {atom_codes(Space, Codes)},
  xml_space_(Codes).
xml_space(Space) -->
  xml_space_(Codes),
  {atom_codes(Space, Codes)}.

xml_space_([H|T]) -->
  xml_space__(H),
  xml_space_(T).
xml_space_([H]) -->
  xml_space__(H).

xml_space__(C) --> carriage_return(C).
xml_space__(C) --> horizontal_tab(C).
xml_space__(C) --> line_feed(C).
xml_space__(C) --> space(C).

xml_yes_no(xml_yes_no(no), false) --> "no".
xml_yes_no(xml_yes_no(yes), true) --> "yes".
