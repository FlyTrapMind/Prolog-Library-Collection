:- module(
  rfc2616_basic,
  [
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'HEX'//0,
    'HEX'//1, % ?Code:code
    'HEX'//2, % ?Code:code
              % ?DecimalDigit:between(0,15)
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
  flp(rfc2234),
  [
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?Code:code
                % ?DecimalDigit:between(0,9)
    'DQUOTE'//0 as '"',
    'HTAB'//0 as 'HT',
    'HTAB'//1 as 'HT', % ?Code:code
    'LF'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).

/** <module> RFC 2616 ABNF rules

DCGs for the basic rules defined in RFC 2616 (HTTP 1.1).

@author Wouter Beek
@see [RFC 2616](http://tools.ietf.org/html/rfc2616)
@see US-ASCII is defined in ANSI X3.4-1986
@version 2013/12, 2014/05-2014/06
*/

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).



%! '"'// .
% Double quote.
%
% ~~~
% <"> = <US-ASCII double-quote mark (34)>
% ~~~
%
% @see RFC 2234 named this 'DQUOTE'//0 and defined it in ABNF.



%! 'ALPHA'// .
%! 'ALPHA'(?Code:code)// .
%
% ~~~{.abnf}
% ALPHA = UPALPHA | LOALPHA
% ~~~
%
% @see RFC 2234 defined this differently, but produced the same result.



%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% US-ASCII character (including NULL).
%
% ~~~
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ~~~
%
% @see RFC 2234 defined this differently, i.e. excluding the NULL character.

'CHAR' -->
  'CHAR'(_).

'CHAR'(C) -->
  ascii(C).



%! 'CR'// .
% The carriage return.
%
% ~~~
% CR = <US-ASCII CR, carriage return (13)>
% ~~~
%
% @see RFC 2234 defined this in ABNF.


%! 'CRLF'// .
% ### Syntax
%
% ~~~{.abnf}
% CRLF = CR LF
% ~~~
%
% The end-of-line marker within an entity-body is defined by
%  its associated media type, as described in section 3.7.
%
% A `CRLF` is allowed in the definition of `TEXT` only as part of a header
%  field continuation.
% It is expected that the folding `LWS` will be replaced with a single `SP`
%  before interpretation of the `TEXT` value.
%
% ### Semantics
%
% HTTP/1.1 defines the sequence `CR LF` as the end-of-line marker for
%  all protocol elements except the entity-body (see appendix 19.3 for
%  tolerant applications).
%
% @see RFC 2234



%! 'CTL'// .
%! 'CTL'(?Code:code)// .
% Control character.
%
% ~~~
% CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
% ~~~
%
% @see RFC 2234 defines this in ABNF.



%! 'DIGIT'// .
%! 'DIGIT'(?Code:code)// .
%! 'DIGIT'(?Code:code, ?DecimalDigit:between(0,9))// .
% Decimal digit.
%
% ~~~
% DIGIT = <any US-ASCII digit "0".."9">
% ~~~
%
% @see RFC 2234 defined this in ABNF.



%! 'HEX'// .
%! 'HEX'(?Code:code)// .
%! 'HEX'(?Code:code, ?DecimalNumber:between(0.15))// .
% Hexadecimal numeric characters are used in several protocol elements.
%
% ~~~{.abnf}
% HEX = "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f" | DIGIT
% ~~~
%
% @compat RFC 2616
% @compat SPARQL 1.0 [171].
% @compat SPARQL 1.1 Query [172].
% @compat Turtle 1.1 [171s].
% @see RFC 2234 defined 'HEXDIG'//[0,1,2], leaving out the lowercase letters.

'HEX' -->
  'HEX'(_).

'HEX'(C) -->
  'HEX'(C, _).

'HEX'(C, D) -->
  hexadecimal_digit(C, D).



%! 'HT'// .
%! 'HT'(?Code:code)// .
% The horizontal tab.
%
% ~~~
% HT = <US-ASCII HT, horizontal-tab (9)>
% ~~~
%
% @see RFC 2234 named this 'HTAB'//[0,1] and defined it in ABNF.


%! 'LF'// .
% The linefeed.
%
% ~~~
% LF = <US-ASCII LF, linefeed (10)>
% ~~~
%
% @see RFC 2234 defined this in ABNF.


%! 'LOALPHA'// .
%! 'LOALPHA'(?Code:code)// .
% US-ASCII lowercase letter.
%
% ~~~
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ~~~

'LOALPHA' -->
  'LOALPHA'(_).

'LOALPHA'(C) -->
  ascii_letter_lowercase(C).


%! 'LWS'// .
% Linear white space.
%
% ### Syntax
%
% ~~~{.abnf}
% LWS = [CRLF] 1*(SP|HT)
% ~~~
%
% HTTP/1.1 header field values can be folded onto multiple lines if
%  the continuation line begins with a space or horizontal tab.
%
% ### Semantics
%
% All linear white space, including folding, has the same semantics as `SP`.
%
% ### Pragmatics
%
% A recipient MAY replace any linear white space with a single `SP`
%  before interpreting the field value or forwarding the message downstream.

'LWS' -->
  '?'('CRLF'),
  '+'(('SP' ; 'HT')).


%! 'OCTET'// .
%! 'OCTET'(?Code:code)// .
% ~~~
% OCTET = <any 8-bit sequence of data>
% ~~~
%
% @see RFC 2234 defined this.


%! 'SP'// .
%! 'SP'(?Code:code)// .
% The space.
%
% ~~~
% SP = <US-ASCII SP, space (32)>
% ~~~
%
% @see RFC 2234 defined this in ABNF.


%! 'TEXT'// .
%! 'TEXT'(?Code:code)// .
%
% ### Syntax
%
% ~~~
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~
%
% The `TEXT` rule is only used for descriptive field contents and
%  values that are not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
%  ISO-8859-1 only when encoded according to the rules of RFC 2047.
%
% @see ISO-8859-1
% @see RFC 2047

'TEXT' -->
  'TEXT'(_).

'TEXT'(C) -->
  'OCTET'(C),
  {\+ code_type(C, cntrl)}.


%! 'UPALPHA'// .
%! 'UPALPHA'(?Code:code)// .
% US-ASCII uppercase letter.
%
% ~~~
% UPALPHA = <any US-ASCII uppercase letter "A".."Z">
% ~~~

'UPALPHA' -->
  'UPALPHA'(_).

'UPALPHA'(C) -->
  ascii_letter_uppercase(C).

