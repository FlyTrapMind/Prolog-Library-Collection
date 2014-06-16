:- module(
  rfc3986,
  [
    uri_encode//0,
    uri_search//1 % +Params:list(nvpair)
  ]
).

/** <module> RFC 3986

@author Wouter Beek
@version 2014/06
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(flp(rfc2234)).



%! uri_encode// .
% Apply URI encoding in-stream.

uri_encode, [C] -->
  'ALPHA'(C), !,
  uri_encode.
uri_encode, [C] -->
  'DIGIT'(C), !,
  uri_encode.
uri_encode, [37|Cs] -->
  between_dec(0, 256, N), !,
  {dec_to_hex_codes(N, Cs)},
  uri_encode.
uri_encode --> [].

dec_to_hex_codes(Value, [C1,C2]):-
  Value1 is Value div 16,
  hexadecimal_digit(C1, Value1, _, _),
  Value2 is Value mod 16,
  hexadecimal_digit(C2, Value2, _, _).


%! uri_search(+Params:list(nvpair))// .

uri_search([]) --> [].
uri_search([N=V|T]) -->
  uri_search_name(N),
  `=`,
  uri_search_value(V),
  uri_search(T).

uri_search_name(N) -->
  atom(N).

uri_search_value(V1) -->
  {once(dcg_phrase(uri_encode, V1, V2))},
  codes(V2).

