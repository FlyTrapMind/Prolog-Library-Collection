:- module(
  dcg_quote,
  [
    quoted//1, % :Dcg
    quoted//2, % :Quote
               % :Dcg
    quoted//3 % ?Length:positive_number
              % :Quote
              % :Dcg
  ]
).

/** <module> DCG: Quote

Grammar for quoted phrases.

@author Wouter Beek
@version 2014/10-2014/12
*/

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).

:- meta_predicate(quoted(//,?,?)).
:- meta_predicate(quoted(//,//,?,?)).
:- meta_predicate(quoted(?,//,//,?,?)).





%! quoted(:Dcg)// .
%! quoted(:Quote, :Dcg)// .
%! quoted(?Length:positive_integer, :Quote, :Dcg)// .
% Supported values for Quote:
%   - `double_quote`
%   - `single_quote`

quoted(Dcg) -->
  quoted(double_quote, Dcg).

quoted(Quote, Dcg) -->
  quoted(1, Quote, Dcg).

quoted(Length, Quote, Dcg) -->
  {(  Quote \= _:double_quote,
      Quote \= _:single_quote
  ->  type_error(quote, Quote)
  ;   true
  )},
  dcg_between('#'(Length, Quote, []), Dcg).

