:- module(
  http_abnf,
  [
    '"'//0,
    '"'//1, % ?Code:code
    abnf_list//2, % :ElementDCG
                  % -List:list
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'HT'//0,
    'HT'//1, % ?Code:code
    'LOALPHA'//0,
    'LOALPHA'//1, % ?Code:code
    'LWS'//0,
    'TEXT'//0,
    'TEXT'//1, % ?Code:code
    'UPALPHA'//0,
    'UPALPHA'//1 % ?Code:code
  ]
).
:- reexport(
  standards(rfc4234_abnf),
  [
    'CR'//0,
    'CR'//1, % ?Code:code
    'CRLF'//0,
    'CRLF'//1, % ?Code:code
    'CRLF'//2, % ?Code:code
    'CTL'//0,
    'CTL'//1,
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?Code:code
                % ?DecimalDigit:integer
    'LF'//0,
    'LF'//1, % ?Code:code
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1 % ?Code:code
  ]
).

/** <module> RFC 2616 ABNF rules

Basic ABNF rules defined in RFC 2616 (HTTP 1.1).

# RFC 2616 (HTTP 1.1)

The following basic rules are specified in RFC 2616 (HTTP 1.1).

## Basic rules that are conforming with RFC 4234 (ABNF)

~~~{.abnf}
CR      = <US-ASCII CR, carriage return (13)>
CRLF    = CR LF
CTL     = <any US-ASCII control character
          (octets 0 - 31) and DEL (127)>
DIGIT   = <any US-ASCII digit "0".."9">
LF      = <US-ASCII LF, linefeed (10)>
OCTET   = <any 8-bit sequence of data>
SP      = <US-ASCII SP, space (32)>
~~~

## Basic rules that are deviating from RFC 4234 (ABNF)

~~~{.abnf}
<">     = <US-ASCII double-quote mark (34)>
ALPHA   = UPALPHA | LOALPHA
CHAR    = <any US-ASCII character (octets 0 - 127)>
HT      = <US-ASCII HT, horizontal-tab (9)>
LOALPHA = <any US-ASCII lowercase letter "a".."z">
LWS     = [CRLF] 1*(SP|HT)
UPALPHA = <any US-ASCII uppercase letter "A".."Z">
TEXT    = <any OCTET except CTLs, but including LWS>

~~~

# Differences with RFC 4234 (ABNF)

In RFC 2616 but not in RFC 4234:
  * =|#rule|=
  * 'TEXT'

Same or similar rule in RFC 4234, but slightly different in RFC 2616:
  * 'ALPHA' is split in 'UPALPHA' and 'LOALPHA'
  * 'CHAR' includes the NULL character.
  * Horizontal tab is called 'HT' instead of 'HTAB'.
  * Linear white space is called 'LWS' instead of 'LWSP'.

--

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).

:- meta_predicate(abnf_list(3,-)).



%! '"'//
% @see '"'//1

'"' -->
  double_quote.

%! '"'(?Code:code)//
% US-ASCII double-quote mark.
%
% ~~~{.abnf}
% <"> = <US-ASCII double-quote mark (34)>
% ~~~

'"'(C) -->
  double_quote(C).

%! abnf_list(:ElementDCG, ?Repetitions:pair, -List:list)//
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

abnf_list(ElementDCG, Rep, L) -->
  {nonvar(L)}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_nonvar(ElementDCG, Max, 0, C, L),
  {in_between(Min, Max, C)}, !.
abnf_list(ElementDCG, Rep, L) -->
  {var(L)}, !,
  {dcg_multi:repetition(Rep, Min, Max)},
  abnf_list_var(ElementDCG, Min, Max, L),
  {length(L, C)},
  {in_between(Min, Max, C)}.

abnf_list_nonvar(ElementDCG, [H|T]) -->
  dcg_multi('LWS'),
  dcg_multi(comma_LWS),
  phrase(ElementDCG, H),
  abnf_list_(ElementDCG, T).
abnf_list_(_ElementDCG, []) --> [].

comma_LSW -->
  ",",
  dcg_multi('LWS').

%! 'ALPHA'//
% @see 'ALPHA'//1

'ALPHA' -->
  'UPALPHA'.
'ALPHA' -->
  'LOALPHA'.

%! 'ALPHA'(?Code:code)//
% Definition in RFC 2616:
% ~~~{.abnf}
% ALPHA = UPALPHA | LOALPHA
% ~~~

'ALPHA'(C) -->
  'UPALPHA'(C).
'ALPHA'(C) -->
  'LOALPHA'(C).

%! 'CHAR'//
% @see 'CHAR'//1

'CHAR' -->
  ascii.

%! 'CHAR'(?Code:code)//
% US-ASCII character (including NULL).
%
% ~~~{.abnf}
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ~~~

'CHAR'(C) -->
  ascii(C).

%! 'HT'//
% @see 'HT'//1

'HT' -->
  horizontal_tab.

%! 'HT'(?Code:code)//
% Horizontal tab.
%
% ~~~{.abnf}
% HT = <US-ASCII HT, horizontal-tab (9)>
% ~~~

'HT'(C) -->
  horizontal_tab(C).

%! 'LOALPHA'//
% @see 'LOALPHA'//1

'LOALPHA' -->
  ascii_letter_lowercase.

%! 'LOALPHA'(?Code:code)//
% US-ASCII lowercase letter.
%
% ~~~
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ~~~

'LOALPHA'(C) -->
  ascii_letter_lowercase(C).

%! 'LWS'(?Code:code)//
% Linear white space.
%
% ~~~{.abnf}
% LWS = [CRLF] 1*(SP|HT)
% ~~~

'LWS' -->
  dcg_multi('CRLF', 0-1),
  dcg_multi(('SP' ; 'HT'), 1-_).

%! 'TEXT'//
% @see 'TEXT'//1

'TEXT' -->
  'TEXT'(_C).

%! 'TEXT'(?Code:code)//
% ~~~{.abnf}
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~

'TEXT'(C) -->
  'OCTET'(C),
  {\+ code_type(C, cntrl)}.

%! 'UPALPHA'//
% @see 'UPALPHA'//1

'UPALPHA' -->
  ascii_letter_uppercase.

%! 'UPALPHA'(?Code:code)//
% US-ASCII uppercase letter.
%
% ~~~
% UPALPHA = <any US-ASCII uppercase letter "A".."Z">
% ~~~

'UPALPHA'(C) -->
  ascii_letter_uppercase(C).

