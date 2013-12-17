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
%  the comment text with parentheses.
% Comments are only allowed in fields containing `"comment"` as part of
%  their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.abnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~
%
% @see RFC 2616

comment(comment(Comm), Cs) -->
  bracketed(dcg_multi1('ctext_or_quoted-pair_or_comment', _-_, Css)),
  {flatten(Css, Cs)},
  {atom_codes(Comm, Cs)}.

'ctext_or_quoted-pair_or_comment'([C]) -->
  ctext(C).
'ctext_or_quoted-pair_or_comment'([C]) -->
  'quoted-pair'(C).
'ctext_or_quoted-pair_or_comment'(Cs) -->
  comment(_, Cs).



%! ctext(?Code:code)//
% Maybe this stands for 'comment text'.
%
% ~~~{.abnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~
%
% Notice that the round brackets are used to indicate
%  the begin and end of an HTTP comment.
%
% @see RFC 2616

ctext(C) -->
  'TEXT'(C),
  {C \= 40},
  {C \= 41}.



%! qdtext(?Code:code)//
% ~~~{.abnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(C) -->
  'TEXT'(C),
  {C \= 34}.



%! 'quoted-pair'(?Code:code)//
% The backslash character MAY be used as a single-character quoting mechanism
%  only within `quoted-string` and `comment` constructs.
%
% ~~~{.abnf}
% quoted-pair = "\" CHAR
% ~~~
%
% @see RFC 2616

'quoted-pair'(C) -->
  "\\",
  'CHAR'(C).



%! 'quoted-string'(-Tree:compound, ?String:atom)//
% A string of text is parsed as a single word if it is quoted using
%  double-quote marks.
%
% ~~~{.abnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~
%
% @see RFC 2616

'quoted-string'('quoted-string'(String), String) -->
  quoted(dcg_multi1('qdtex_or_quoted-pair', Cs)),
  {atom_codes(String, Cs)}.

'qdtex_or_quoted-pair'(C) -->
  qdtext(C).
'qdtex_or_quoted'(C) -->
  'quoted-pair'(C).



%! separator//
% Many HTTP/1.1 header field values consist of words separated by `LWS`
%  or special characters.
% These special characters MUST be in a `quoted-string`
%  to be used within a parameter value.
%
% ~~~{.abnf}
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ~~~
%
% @see RFC 2616

separator --> bracket. % 40,41,91,93,123,125
separator --> ">". % 62
separator --> "<". % 60
separator --> "@". % 64
separator --> ",". %44
separator --> ";". % 59
separator --> ":". % 58
separator --> "\\". % 92
separator --> '"'. %34
separator --> "/". % 47, 92
separator --> "?". % 63
separator --> "=". % 61
separator --> 'SP'. % 32
separator --> 'HT'. % 9



%! token(-Tree:compound, ?Token:atom)//
% ~~~{.abnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~
%
% @see RFC 2616

token(token(Token), Token) -->
  dcg_multi1(token_, 1-_, Token, [convert(codes_to_atom)]).

token_(C) -->
  'CHAR'(C),
  {\+ phrase('CTL', [C]), \+ phrase(separator, [C])}.

