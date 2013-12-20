:- module(
  rfc2616_abnf,
  [
    % One argument
    abnf_list1//3, % :ElementDCG
                   % ?Repetition:pair(nonneg,or([nonneg,oneof([inf])]))
                   % ?Arguments:list
    abnf_list1//4, % :ElementDCG
                   % ?Repetition:pair(nonneg,or([nonneg,oneof([inf])]))
                   % ?Arguments:list
                   % -Count:nonneg
    % Two arguments
    abnf_list2//4, % :ElementDCG
                   % ?Repetition:pair(nonneg,or([nonneg,oneof([inf])]))
                   % ?Arguments1:list
                   % ?Arguments2:list
    abnf_list2//5 % :ElementDCG
                  % ?Repetition:pair(nonneg,or([nonneg,oneof([inf])]))
                  % ?Arguments1:list
                  % ?Arguments2:list
                  % -Count:nonneg
  ]
).

/** <module> RFC 2616 ABNF

DCGs implementing the ABNF grammar rules defined in RFC 2616 (HTTP 1.1).

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).
:- use_module(library(apply)).



%! abnf_list1(
%!   :ElementDCG,
%!   ?Repetitions:pair(nonneg,or([nonneg,oneof([inf])])),
%!   ?Arguments:list,
%!   -Count:nonneg
%! )// .
%! abnf_list2(
%!   :ElementDCG,
%!   ?Repetitions:pair(nonneg,or([nonneg,oneof([inf])])),
%!   ?Arguments1:list,
%!   ?Arguments2:list,
%!   -Count:nonneg
%! )// .
% Implements the ABNF `#rule`, as defined in RFC 2616 (HTTP 1.1).
%
% A construct `<m>#<n>` is defined, similar to `<m>*<n>`,
%  for defining lists of elements.
% The full form is `<n>#<m>element` indicating at least `n`
%  and at most `m` elements, each separated by one or more commas
%  and OPTIONAL `LWS`.
%
% # Motivation & example
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
% # Null elements
%
% Wherever this construct is used, null elements are allowed,
%  but do not contribute to the count of elements present.
% That is, `(element), , (element)` is permitted,
%  but counts as only two elements.
% Therefore, where at least one element is required,
%  at least one non-null element MUST be present.
%
% # Default values
%
% Default values are 0 and infinity so that `#element` allows any number,
%  including zero;
%  `1#element` requires at least one;
%  and `1#2element` allows one or two.
%
% @see RFC 2616
% @see This grammatical construct is *not* defined in RFC 4234 (ABNF).

abnf_list1(DCG, Rep, L) -->
  abnf_list1(DCG, Rep, L, _C).

abnf_list1(DCG, Rep, L, C) -->
  {nonvar(L)}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_nonvar(DCG, Max, 0, C, L),
  {dcg_multi:in_between(Min, Max, C)}, !.
abnf_list1(DCG, Rep, L, C) -->
  {var(L)}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_var(DCG, Min, Max, L),
  {length(L, C), dcg_multi:in_between(Min, Max, C)}.

abnf_list2(DCG, Rep, L1, L2) -->
  abnf_list2(DCG, Rep, L1, L2, _C).

abnf_list2(DCG, Rep, L1, L2, C) -->
  {maplist(nonvar, [L1,L2])}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_nonvar(DCG, Max, 0, C, L1, L2),
  {dcg_multi:in_between(Min, Max, C)}, !.
abnf_list2(DCG, Rep, L1, L2, C) -->
  {maplist(var, [L1,L2])}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_var(DCG, Min, Max, L1, L2),
  {length(L1, C), dcg_multi:in_between(Min, Max, C)}.

% One argument.
abnf_list_nonvar(_DCG, _Max, C, C, []) -->
  [].
abnf_list_nonvar(DCG, Max, C1, C3, [H|T]) -->
  abnf_list_separator(C1),
  dcg_call(DCG, H),
  % Check that counter does not exceed maximum.
  {succ(C1, C2), dcg_multi:greater_than_or_equal_to(Max, C2)},
  abnf_list_nonvar(DCG, Max, C2, C3, T).

% Two arguments.
abnf_list_nonvar(_DCG, _Max, C, C, [], []) -->
  [].
abnf_list_nonvar(DCG, Max, C1, C3, [H1|T1], [H2|T2]) -->
  abnf_list_separator(C1),
  dcg_call(DCG, H1, H2),
  % Check that counter does not exceed maximum.
  {succ(C1, C2), dcg_multi:greater_than_or_equal_to(Max, C2)},
  abnf_list_nonvar(DCG, Max, C2, C3, T1, T2).

abnf_list_separator -->
  dcg_multi('LWS'),
  ",",
  dcg_multi('LWS').

abnf_list_separator(C) -->
  {C == 0}, !,
  dcg_multi('LWS').
abnf_list_separator(_C) -->
  abnf_list_separator.


% One agument.

abnf_list_var(DCG, Min, Max, L) -->
  dcg_multi('LWS'),
  abnf_list_var_(DCG, Min, Max, L).

abnf_list_var_(_DCG, _Min, inf, []) -->
  [].
% Last non-null element.
abnf_list_var_(DCG, Min, Min, [H]) -->
  dcg_call(DCG, H).
% Last null element.
abnf_list_var_(_DCG, Min, Min, []) -->
  abnf_list_separator.
% Non-last non-null element.
abnf_list_var_(DCG, Min, Max1, [H|T]) -->
  dcg_call(DCG, H),
  abnf_list_separator,
  {dcg_multi:count_down(Max1, Max2)},
  abnf_list_var_(DCG, Min, Max2, T).
% Null element.
abnf_list_var_(DCG, Min, Max, L) -->
  abnf_list_separator,
  abnf_list_var_(DCG, Min, Max, L).


% Two arguments.

abnf_list_var(DCG, Min, Max, L1, L2) -->
  dcg_multi('LWS'),
  abnf_list_var_(DCG, Min, Max, L1, L2).

abnf_list_var_(_DCG, _Min, inf, [], []) -->
  [].
% Last non-null element.
abnf_list_var_(DCG, Min, Min, [H1], [H2]) -->
  dcg_call(DCG, H1, H2).
% Last null element.
abnf_list_var_(_DCG, Min, Min, [], []) -->
  abnf_list_separator.
% Non-last non-null element.
abnf_list_var_(DCG, Min, Max1, [H1|T1], [H2|T2]) -->
  dcg_call(DCG, H1, H2),
  abnf_list_separator,
  {dcg_multi:count_down(Max1, Max2)},
  abnf_list_var_(DCG, Min, Max2, T1, T2).
% Null element.
abnf_list_var_(DCG, Min, Max, L1, L2) -->
  abnf_list_separator,
  abnf_list_var_(DCG, Min, Max, L1, L2).

