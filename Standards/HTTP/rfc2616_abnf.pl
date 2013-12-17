:- module(
  rfc2616_abnf,
  [
    abnf_list//4 % :ElementDCG
                 % ?Repetitions:pair
                 % -List:list
                 % -Count:nonneg
  ]
).

/** <module> RFC 2616 ABNF

DCGs implementing the ABNF grammar rules defined in RFC 2616 (HTTP 1.1).

--

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).



%! abnf_list(:ElementDCG, ?Repetitions:pair, -List:list, -Count:nonneg)//
% Implements the ABNF =|#rule|=, as defined in RFC 2616 (HTTP 1.1).
%
% A construct =|#|= is defined, similar to =|*|=,
%   for defining lists of elements.
% The full form is =|<n>#<m>element|= indicating
%   at least _n_ and at most _m_ elements,
%   each separated by one or more commas
%   and OPTIONAL linear white space ('LWS'//).
%
% # Motivation & example
%
% This makes the usual form of lists very easy;
% a rule such as
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
%   but do not contribute to the count of elements present.
% That is, =|(element), , (element) |= is permitted,
%   but counts as only two elements.
% Therefore, where at least one element is required,
%   at least one non-null element MUST be present.
%
% # Default values
%
% Default values are 0 and infinity so that =|#element|= allows any number,
%   including zero; =|1#element|= requires at least one;
%   and =|1#2element|= allows one or two.
%
% # Differences from RFC 4234 (ABNF)
%
% This grammatical construct is not defined in RFC 4234 (ABNF).
%
% @see RFC 2616

:- meta_predicate(abnf_list(3,?,-,-,?,?)).
abnf_list(DCG, Rep, L, C) -->
  dcg_multi1('LWS_and_element'(DCG), Rep, L, [separator('LWS_and_comma')], C).

:- meta_predicate('LWS_and_element'(3,-,?,?)).
'LWS_and_element'(DCG, X) -->
  dcg_multi('LWS'),
  dcg_call(DCG, X).

%! 'LWS_and_comma'//

'LWS_and comma' -->
  dcg_multi('LWS'),
  ",".

