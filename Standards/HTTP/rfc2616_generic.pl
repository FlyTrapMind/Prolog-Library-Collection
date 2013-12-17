:- module(
  rfc2616_generic,
  [
    comment//2, % -Tree:compound
                % ?Codes:list(code)
    'quoted-string'//2, % -Tree:compound
                        % ?Codes:list(code)
    separator//0,
    token//2 % -Tree:compound
             % ?Token:atom
  ]
).

/** <module> HTTP basic rules

Some basic DCG rules that are too specific to be reused outside of
  the HTTP specification, but that are too generic to be included in
  the main DCGs.

@author Wouter Beek
@version 2013/12
*/

:- use_module(generics(codes_ext)). % Used in dcg_multi1//4.
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).



%! charset(-Tree:compound, ?Charset:atom)//
% HTTP character sets are identified by case-insensitive tokens.
%
% ~~~{.abnf}
% charset = token
% ~~~

charset(T0, X) -->
  token(T0, X).

%! comment(-Tree:compound, ?Codes:list(code))//
% Comments can be included in some HTTP header fields by surrounding
% the comment text with parentheses (singular parenthesis//1).
% Comments are only allowed in fields containing `“comment”` as part of
% their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.abnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~

comment(comment(Comment), Codes) -->
  bracketed(dcg_multi1(comment_, _-_, CodeLists)),
  {flatten(CodeLists, Codes)},
  {atom_codes(Comment, Codes)}.
comment_([Code]) -->
  ctext(Code).
comment_([Code]) -->
  'quoted-pair'(Code).
comment_(Codes) -->
  comment(_, Codes).



%! ctext(?Code:code)//
% Maybe this stands for 'comment text'.
%
% ~~~{.abnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~

ctext(Code) -->
  'TEXT'(Code),
  {Code \= 40},
  {Code \= 41}.



%! qdtext(?Code:code)//
% ~~~{.abnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(C) -->
  'TEXT'(C),
  {C \= 34}.



%! 'quoted-pair'(?Code:code)//
% The backslash//1 character (=|\|=) MAY be used as a single-character
% quoting mechanism only within 'quoted-string'// and comment// constructs.
% ~~~{.abnf}
% quoted-pair = "\" CHAR
% ~~~

'quoted-pair'(C) -->
  "\\",
  'CHAR'(C).



%! 'quoted-string'(-Tree:compound, ?Codes:list(code))//
% A string of text is parsed as a single word if it is quoted using
%  double-quote marks.
%
% ~~~{.abnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~
%
% @RFC 2616

'quoted-string'('quoted-string'(Atom), Atom) -->
  '"',
  dcg_multi1('qdtex_or_quoted', Codes),
  '"',
  {atom_codes(Atom, Codes)}.

'qdtex_or_quoted-pair'(C) -->
  qdtext(C).
'qdtex_or_quoted'(C) -->
  'quoted-pair'(C).



%! separator//
% ~~~{.abnf}
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ~~~

separator --> bracket. % 40,41,91,93,123,125
separator --> greater_than_sign. % 62
separator --> less_than_sign. % 60
separator --> at_sign. % 64
separator --> comma. %44
separator --> semi_colon. % 59
separator --> colon. % 58
separator --> backslash. % 92
separator --> '"'. %34
separator --> slash. % 47, 92
separator --> question_mark. % 63
separator --> equals_sign. % 61
separator --> 'SP'. % 32
separator --> 'HT'. % 9



%! token(-Tree:compound, ?Token:atom)//
% Many HTTP/1.1 header field values consist of words separated by
% 'LWS'// or special characters.
% These special characters MUST be in a 'quoted-string'//
% to be used within a parameter value (as defined in section 3.6).
%
% ~~~{.bnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~

token(token(Token), Token) -->
  dcg_multi1(token_, 1-_, Token, [convert(codes_to_atom)]).

token_(C) -->
  'CHAR'(C),
  {\+ phrase('CTL', [C]), \+ phrase(separator, [C])}.

