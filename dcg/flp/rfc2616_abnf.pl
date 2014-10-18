:- module(
  rfc2616_abnf,
  [
    'm#n'//4, 'm#n'//5, 'm#n'//6, 'm#n'//7, 'm#n'//8, 'm#n'//9
  ]
).

/** <module> RFC 2616: ABNF

DCG implementation of the ABNF extension rules defined by RFC 2616 (HTTP 1.1).

@author Wouter Beek
@see RFC 2616
@version 2013/12, 2014/10
*/

:- use_module(library(apply)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg_rfc(rfc2616_basic)).

:- use_module(plDcg(dcg_meta)).

:- meta_predicate('m#n'(?,?,//,+,?,?)).
:- meta_predicate('m#n'(?,?,3,+,?,?,?)).
:- meta_predicate('m#n'(?,?,4,+,?,?,?,?)).
:- meta_predicate('m#n'(?,?,5,+,?,?,?,?,?)).
:- meta_predicate('m#n'(?,?,6,+,?,?,?,?,?,?)).
:- meta_predicate('m#n'(?,?,7,+,?,?,?,?,?,?,?)).

:- predicate_options('m#n'//4, 4, [
     pass_to('m*n'//4, 4)
   ]).
:- predicate_options('m#n'//5, 5, [
     pass_to('m*n'//5, 5)
   ]).
:- predicate_options('m#n'//6, 6, [
     pass_to('m*n'//6, 6)
   ]).
:- predicate_options('m#n'//7, 7, [
     pass_to('m*n'//7, 7)
   ]).
:- predicate_options('m#n'//8, 8, [
     pass_to('m*n'//8, 8)
   ]).
:- predicate_options('m#n'//9, 9, [
     pass_to('m*n'//9, 9)
   ]).



%! 'm#n'(?M:nonneg, ?N:nonneg, :Dcg, +Options:list(nvpair))// .
% Implements the ABNF `#rule`, as defined in RFC 2616 (HTTP 1.1).
%
% A construct `<m>#<n>` is defined, similar to `<m>*<n>`,
%  for defining lists of elements.
% The full form is `<n>#<m>element` indicating at least `n`
%  and at most `m` elements, each separated by one or more commas
%  and OPTIONAL `LWS`.
%
% ### Motivation & example
%
% This makes the usual form of lists very easy; a rule such as
% ~~~{.abnf}
% ( *LWS element *( *LWS "," *LWS element ))
% ~~~
% can be shown as
% ~~~{.abnf}
% 1#element
% ~~~
%
% ### Null elements
%
% Wherever this construct is used, null elements are allowed,
%  but do not contribute to the count of elements present.
% That is, `(element), , (element)` is permitted,
%  but counts as only two elements.
% Therefore, where at least one element is required,
%  at least one non-null element MUST be present.
%
% ### Default values
%
% Default values are 0 and infinity so that `#element` allows any number,
%  including zero;
%  `1#element` requires at least one;
%  and `1#2element` allows one or two.
%
% @see RFC 2616
% @see This grammatical construct is *not* defined in RFC 4234 (ABNF).

% The full form is `<n>#<m>element` indicating at least `n`
%  and at most `m` elements, each separated by one or more commas
%  and OPTIONAL `LWS`.

'm#n'(M, N, Dcg, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, Options2).

'm#n'(M, N, Dcg, L1, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, L1, Options2).

'm#n'(M, N, Dcg, L1, L2, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, L1, L2, Options2).

'm#n'(M, N, Dcg, L1, L2, L3, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, L1, L2, L3, Options2).

'm#n'(M, N, Dcg, L1, L2, L3, L4, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, L1, L2, L3, L4, Options2).

'm#n'(M, N, Dcg, L1, L2, L3, L4, L5, Options1) -->
  {merge_options([separator('m#n_separator')], Options1, Options2)},
  'm*n'(M, N, Dcg, L1, L2, L3, L4, L5, Options2).

'm#n_separator' -->
  '*'('LWS', []),
  ",",
  '*'('LWS', []).
