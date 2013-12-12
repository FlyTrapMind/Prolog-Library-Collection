:- module(
  http_basic,
  [
    separator//0,
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
:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_abnf)).



%! separator//
% @see separator//1

separator -->
  separator(_C).

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

