:- module(
  rfc3986,
  [
    uri_encode//0,
    uri_encoded_search//1 % +Parameters:list(nvpair)
  ]
).

/** <module> RFC 3986

@author Wouter Beek
@version 2014/06, 2014/08-2014/09
*/

:- use_module(generics(nvpair_ext)).

:- use_module(plDcg(dcg_cardinal)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg_rfc(rfc2234)).



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



%! uri_encoded_search(+Parameters:list(nvpair))// is det.

uri_encoded_search([]) --> !, [].
uri_encoded_search([NVPair]) --> !,
  uri_encoded_nvpair(NVPair).
uri_encoded_search([NVPair|T]) -->
  uri_encoded_nvpair(NVPair),
  "&",
  uri_encoded_search(T).


uri_encoded_nvpair(NVPair) -->
  {nvpair(N, V, NVPair)},
  uri_encoded_atom(N),
  "=",
  uri_encoded_atom(V).


uri_encoded_atom(Atom1) -->
  {
    atom_codes(Atom1, Codes1),
    once(phrase(uri_encode, Codes1, Codes2))
  },
  codes(Codes2).

