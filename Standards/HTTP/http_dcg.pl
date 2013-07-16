:- module(
  http_dcg,
  [
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

Tag suggested by Anne Ogborn on the SWI-Prolog mailing list: `doc-needs-help`.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).



%! comment(+Codes:list(code))//
% Comments can be included in some HTTP header fields by surrounding
% the comment text with parentheses (singular parenthesis//1).
% Comments are only allowed in fields containing `“comment”` as part of
% their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.bnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~

comment(L) -->
  opening_round_bracket(H),
  comment_(T),
  closing_round_bracket(X),
  {append([H|T], [X], L)}.
comment_([]) --> [].
comment_([H|T]) -->
  ctext(H),
  comment_(T).
comment_([H1,H2|T]) -->
  quoted_pair([H1,H2]),
  comment_(T).
comment_(L) -->
  comment(L1),
  comment_(L2),
  {append(L1, L2, L)}.

%! ctext(+Code:code)//
% ~~~{.bnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~

ctext(C) -->
  text(C),
  {\+ memberchk(C, [40,41])}.

%! crlf(+Codes:list(code))//
% HTTP/1.1 defines the sequence carriage_return//1 line_feed//1
% as the end-of-line marker for all protocol elements except
% the entity-body (see appendix 19.3 for tolerant applications).
% The end-of-line marker within an entity-body is defined by its
% associated media type, as described in section 3.7.

crlf([CR,LF]) -->
  carriage_return(CR),
  line_feed(LF).

%! http_url(
%!   +Codes:list(code),
%!   -Host:aton,
%!   -Port:integer,
%!   -AbsolutePath:atom,
%!   -Search:atom,
%!   -Fragment:atom
%! )//

/*
http_url(L, Host, Port, AbsolutePath, Search, Fragment) -->
  {
    uri_authority_components(
      Authority,
      uri_authority_components(_User, Password, Host, Port)
    ),
    uri_components(
      URI,
      uri_components(http, Authority, AbsolutePath, Search, Fragment)
    )
  }.
*/

%! http_version(-Major:integer, -Minor:integer) is det.
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.bnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~

http_version(Major, Minor) -->
  "HTTP",
  forward_slash,
  decimal_number(Major),
  dot,
  decimal_number(Minor).

%! linear_white_space(+Codes:list(code))//
% HTTP/1.1 header field values can be folded onto multiple lines if the
% continuation line begins with space//1 or horizontal_tab//1.
% All linear white space, including folding, has the same semantics as
% space//1.
% A recipient MAY replace any linear_white_space//1 with a single space//1
% before interpreting the field value or forwarding the message downstream.

linear_white_space([CR,LF,H|T]) -->
  crlf([CR,LF]),
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).
linear_white_space([H|T]) -->
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).
linear_white_space_([]) --> [].
linear_white_space_([H|T]) -->
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).

%! qdtext(+Code:code)//
% ~~~{.bnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(C) -->
  text(C),
  {\+ C == 34}.

%! quoted_pair(+Codes:list(code))//
% The backslash//1 character (`“\”`) MAY be used as a single-character
% quoting mechanism only within quoted_string//1 and comment//1 constructs.
% ~~~{.bnf}
% quoted-pair = "\" CHAR
% ~~~

quoted_pair([Backslash,Char]) -->
  backslash(Backslash),
  [Char].

%! quoted_string(+Codes:list(code))//
% A string of text is parsed as a single word if it is quoted using
% double_quote//1 marks.
% ~~~{.bnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~

quoted_string(L) -->
  double_quote(H),
  quoted_string_(T),
  double_quote(X),
  {append([H|T], [X], L)}.
quoted_string_([]) --> [].
quoted_string_([H|T]) -->
  qdtext(H),
  quoted_string_(T).
quoted_string_([H1,H2|T]) -->
  quoted_pair([H1,H2]),
  quoted_string_(T).

%! separator(+Code:code)//
% ~~~{.bnf}
% separators = "(" | ")" | "<" | ">" | "@"
%              | "," | ";" | ":" | "\" | <">
%              | "/" | "[" | "]" | "?" | "="
%              | "{" | "}" | SP | HT
% ~~~

separator(C) --> at_sign(C). % 64
separator(C) --> bracket(C). % 40,41,91,93,123,125
separator(C) --> colon(C). % 58
separator(C) --> comma(C). %44
separator(C) --> double_quote(C). %34
separator(C) --> equals_sign(C). % 61
separator(C) --> greater_than_sign(C). % 62
separator(C) --> horizontal_tab(C). % 9
separator(C) --> question_mark(C). % 63
separator(C) --> semi_colon(C). % 59
separator(C) --> slash(C). % 47, 92
separator(C) --> less_than_sign(C). % 60
separator(C) --> space(C). % 32

%! text(+Code:code)//
% The text//1 rule is only used for descriptive field contents and values
% that are not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
% ISO-8859-1 [22] only when encoded according to the rules of RFC 2047 [14].
%
% ~~~{.bnf}
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~
%
% A crlf//1 is allowed in the definition of text//1 only as part of
% a header field continuation.
% It is expected that the folding linear_white_space//1 will be replaced
% with a single space//1 before interpretation of the text//1 value.

text(C) -->
  [C],
  {\+ code_type(C, cntrl)}.

%! token(+Codes:list(code))//
% Many HTTP/1.1 header field values consist of words separated by
% linear_white_space//1 or special//1 characters.
% These special//1 characters MUST be in a quoted_string//1
% to be used within a parameter value (as defined in section 3.6).
%
% ~~~{.bnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~

token([C]) -->
  token_(C).
token([H|T]) -->
  token_(H),
  token(T).
token_(C) -->
  [C],
  {
    \+ code_type(C, cntrl),
    \+ memberchk(
      C,
      [9,32,34,40,41,44,47,58,59,60,61,62,63,64,91,92,93,123,125]
    )
  }.
