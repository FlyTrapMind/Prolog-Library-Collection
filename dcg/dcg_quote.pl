:- module(
  dcg_quote,
  [
    quoted//1, % :Dcg
    quoted//3 % ?Length:positive_number
              % ?Type:oneof([double,single])
              % :Dcg
  ]
).

/** <module> DCG: Quote

Grammar for quoted phrases.

@author Wouter Beek
@version 2014/10-2014/11
*/

:- use_module(library(aggregate)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generics)).

:- meta_predicate(quoted(//,?,?)).
:- meta_predicate(quoted(?,?,//,?,?)).

error:has_type(quote, Term):-
  aggregate_all(
    set(Quote),
    quote(Quote),
    Quotes
  ),
  error:has_type(oneof(Quotes), Term).



%! quoted(:Dcg)// .
%! quoted(?Length:positive_integer, ?Type:oneof([double,single]), :Dcg)// .
% Typical values for `Quote` are:
%   * `double_quote`
%     Result: `"..."`
%   * `single_quote`
%     Result: `'...'`
%   * `triple_quote(double_quote)`
%     Result: `"""..."""`
%   * `triple_quote(single_quote)`
%     Result: `'''...'''`

quoted(Dcg) -->
  quoted(1, double, Dcg).

quoted(Length, Type, Dcg) -->
  dcg_between(quote(Length, Type), Dcg).



% HELPERS

quote(Length, double) -->
  '#'(Length, double_quote, []).
quote(Length, single) -->
  '#'(Length, single_quote, []).
