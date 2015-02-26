:- module(
  dcg_bracket,
  [
    bracketed//1, % :Dcg
    bracketed//2 % +Type:oneof([angular,curly,round,square])
                 % :Dcg
  ]
).

/** <module> DCG: Bracket

Grammar for processing bracketed phrases.

@author Wouter Beek
@version 2014/11
*/

:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generics)).

:- meta_predicate(bracketed(//,?,?)).
:- meta_predicate(bracketed(+,//,?,?)).



%! bracketed(:Dcg)// .
%! bracketed(+Type:oneof([angular,curly,round,square]), :Dcg)// .

bracketed(Dcg) -->
  bracketed(round, Dcg).

bracketed(Type, Dcg) -->
  dcg_between(
    opening_bracket(_, Type),
    Dcg,
    closing_bracket(_, Type)
  ),
  % Remove choicepoints for brackets of other types in [dcg_ascii].
  !.
