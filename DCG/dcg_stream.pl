:- module(
  dcg_stream,
  [
    dcg_with_output_to/1, % +OutputStream
    dcg_with_output_to/2 % +OutputStream
                         % :DCG
  ]
).

/** <module> DCG stream

Streaming support for DCGs.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(generics(codes_ext)).

:- meta_predicate(dcg_with_output_to(//)).
:- meta_predicate(dcg_with_output_to(+,//)).



dcg_with_output_to(DCG):-
  dcg_with_output_to(current_output, DCG).

dcg_with_output_to(Out, DCG):-
  once(phrase(DCG, Codes)),
  put_codes(Out, Codes).
