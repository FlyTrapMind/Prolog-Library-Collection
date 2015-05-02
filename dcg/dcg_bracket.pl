:- module(
  dcg_bracket,
  [
    bracketed//1, % :Dcg
    bracketed//2 % +Type:oneof([angular,curly,langular,round,square])
                 % :Dcg
  ]
).

/** <module> DCG: Bracket

Grammar for processing bracketed phrases.

@author Wouter Beek
@version 2014/11-2014/12
*/

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_unicode)).

:- meta_predicate(bracketed(//,?,?)).
:- meta_predicate(bracketed(+,//,?,?)).





%! bracketed(:Dcg)// .
%! bracketed(+Type:oneof([angular,curly,langular,round,square]), :Dcg)// .

bracketed(Dcg) -->
  bracketed(round, Dcg).

bracketed(Type, Dcg) -->
  dcg_between(
    opening_bracket(Type, _),
    Dcg,
    closing_bracket(Type, _)
  ),
  % Remove choicepoints for brackets of other types in dcg_ascii.pl.
  !.
