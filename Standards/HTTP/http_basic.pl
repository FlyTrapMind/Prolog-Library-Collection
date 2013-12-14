:- module(
  http_basic,
  [
    comment//2, % -Tree:compound
                % ?Codes:list(code)
    'quoted-string'//2, % -Tree:compound
                        % ?Codes:list(code)
    separator//1, % ?Code:code
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
:- use_module(http(http_abnf)).



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

qdtext(Code) -->
  'TEXT'(Code),
  {Code \= 34}.

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
% double_quote//1 marks.
%
% ~~~{.abnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~

'quoted-string'('quoted-string'(Atom), Codes) -->
  '"',
  dcg_multi2('quoted-string_', _Ts, Codes),
  '"',
  {atom_codes(Atom, Codes)}.
'quoted-string_'('quoted-string'(C), C) -->
  qdtext(C).
'quoted-string_'('quoted-pair'(C), C) -->
  'quoted-pair'(C).

%! separator(?Code:code)//
% ~~~{.abnf}
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ~~~

separator(C) --> bracket(C). % 40,41,91,93,123,125
separator(C) --> greater_than_sign(C). % 62
separator(C) --> less_than_sign(C). % 60
separator(C) --> at_sign(C). % 64
separator(C) --> comma(C). %44
separator(C) --> semi_colon(C). % 59
separator(C) --> colon(C). % 58
separator(C) --> backslash(C). % 92
separator(C) --> '"'(C). %34
separator(C) --> slash(C). % 47, 92
separator(C) --> question_mark(C). % 63
separator(C) --> equals_sign(C). % 61
separator(C) --> 'SP'(C). % 32
separator(C) --> 'HT'(C). % 9

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
  {\+ 'CTL'(C, _, _), \+ separator(C, _, _)}.

