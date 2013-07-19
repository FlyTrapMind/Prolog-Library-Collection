:- module(
  http_dcg,
  [
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

Tag suggested by Anne Ogborn on the SWI-Prolog mailing list: `doc-needs-help`.

# Concepts

  * **Age**
    The age of a response is the time since it was sent by, or
    successfully validated with, the origin server.
  * **Expiration date**
    * **Explicit expiration time**
      The time at which the origin server intends that an entity should
      no longer be returned by a cache without further validation.
    * **Heuristic expiration time**
      An expiration time assigned by a cache when no explicit expiration
      time is available.
  * **First-hand**
    A response is first-hand if it comes directly and without
    unnecessary delay from the origin server, perhaps via one or more
    proxies, or if its validity has just been checked directly with
    the origin server.
  * **Fresh**
    The property of a response for which `age < freshness lifetime`.
  * **Freshness lifetime**
    The length of time between the generation of a response and its
    expiration time.
  * **Inbound/outbound**
    Traveling toward the origin server. / Traveling toward the user agent.
  * **Intermediary**
    * **Gateway**
      A receiving agent.
      Acting as if it were the origin server for the requested resource.
    * **Proxy**
      A forwarding agent, making requests on behalf of other clients.
      * **Transparent proxy**
        A proxy that does not modify the request or response beyond
        what is required for proxy authentication and identification.
    * **Tunnel**
      An intermediary program which is acting as a blind relay between
      two connections.
      A tunnel is not considered a party to the HTTP communication.
  * **Representation**
    An entity included with a response that is subject to content negotiation.
  * **Resource**
    A network data object or service that can be identified by a URI.
  * **Stale**
    The property of a response for which `age >= freshness lifetime`.
  * **Upstream/downstream**
    All messages flow from upstream to downstream.
  * **Validator**
    A protocol element (e.g., an entity tag or a Last-Modified time)
    that is used to find out whether a cache entry is an equivalent
    copy of an entity.
  * **Variant**
    A resource may have one, or more than one, representation(s)
    associated with it at any given instant.
    Use of the term 'variant' does not necessarily imply that the resource
    is subject to content negotiation.

# Client request

Components:
  * Method
  * URI
  * Protocol version
  * MIME-like message
    * Request modifiers
    * Client information
    * Body content

# Server response

Components:
  * Protocol version
  * Success or error code
  * MIME-like message
    * Server information
    * Entity meta-information
    * Entity-body content

# Caching

All non-tunnel parties (i.e., client, server, gateway, proxy)
may employ an internal cache.

@author Wouter Beek
@see Based on RFC 2616, http://tools.ietf.org/html/rfc2616
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(rfc(rfc_2396)).



:- setting(default_port, integer, 80, 'The default TCP port.').

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
%!   -Tree:compound,
%!   ?Host:list(atomic),
%!   ?Port:integer,
%!   ?Path:list(list(atom)),
%!   ?Query:atom
%! )//
% ~~~{.bnf}
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ~~~
%
% @arg Tree A parse tree.
% @arg Host
% @arg Port An integer representing a port.
%      Defaults to the value for setting `default_port`.
% @arg Path
% @arg Query

http_url(T, Host, Port, Path, Query) -->
  "http://",
  host(T1, Host),
  (
    "", {var(Port), setting(default_port, Port)}
  ;
    ":'", port(T2, Port)
  ),
  (
    "", {var(Path), var(Query)}
  ;
    absolute_path(T3, Path),
    (
      "", {var(Query)}
    ;
      "?", query(T4, Query)
    )
  ),
  {parse_tree(http_url, [T1,T2,T3,T4], T)}.

%! http_version(-Tree:compound, ?Major:integer, ?Minor:integer) is det.
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.bnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~

http_version(http_version('HTTP','/',Major,'.',Minor), Major, Minor) -->
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
  % Exclude double quote.
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
