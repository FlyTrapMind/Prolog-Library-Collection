:- module(
  dcg_uri,
  [
    'pct-encoded'//1 % ?Code:between(0,255)
  ]
).

/** <module> URI: DCG grammar

Uniform Resource Identifier (URI): Generic Syntax

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2014/10
*/

:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg_rfc(rfc2234)).



%! 'pct-encoded'(?Code:between(0,255))// is det.
%
% ~~~{.abnf}
% pct-encoded = "%" HEXDIG HEXDIG
% ~~~
%
% ## Normalization
%
% Uppercase hexadecimal digits.

'pct-encoded'(Code) -->
  "%",
  '#'(2, hexadecimal_digit, _, [H1,H2], []),
  {digits_decimal([H1,H2], 16, Code)}.
