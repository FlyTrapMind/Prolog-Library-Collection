:- module(abnf2dcg, []).

/** <module> ABNF-2-DCG

Converts ABNF grammars to DCGs.

@author Wouter Beek
@version 2013/08, 2014/03, 2014/10
*/

:- use_module(library(pio)).

:- use_module(flp(rfc4234_basic)).
:- use_module(generics(db_ext)).
:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).
:- use_module(plDcg(dcg_code)).



abnf -->
  '*'(rule).

%! base(?Radix:oneof([2,10,16]))//

base(2)  --> b.
base(10) --> d.
base(16) --> x.

elements(Name) -->
  element(Name).

element(Name) -->
  single_terminal_value(Code),
  {db_add_dcg_rule(Name, [Code])}.

name(Name) -->
  '*'(code_ci, Name, []).

rule -->
  name(Name),
  blanks, equals_sign, blanks,
  elements(Name),
  'CRLF'.

%! single_terminal_value(-Code:positive_integer)//
% Reads a character code from the input stream.
% The character code is described in a given radix or base
% (either `2`, `10`, or `16`).

single_terminal_value(Code) -->
  percent_sign,
  base(Radix),
  hexadecimal_digit(_, H1),
  hexadecimal_digit(_, H2),
  {digits_decimal([H1,H2], Radix, Code)}.

