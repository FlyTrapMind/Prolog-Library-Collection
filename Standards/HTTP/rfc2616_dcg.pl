:- module(
  rfc2616_dcg,
  [
    http_to_gv/1, % +Tree:compound
    'Request'//6, % -Tree:compound
                  % ?Method:atom
                  % ?URI:compound
                  % ?Version:compound
                  % ?MessageHeaders:list(pair)
                  % ?MessageBody:list(code)
    'Response'//5 % -Tree:compound
                  % ?Version:compound
                  % ?Status:compound
                  % ?MessageHeaders:list(pair)
                  % ?MessageBody:list(code)
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

# ABNF reuse

## CRLF

HTTP/1.1 defines the sequence carriage_return//1 line_feed//1
as the end-of-line marker for all protocol elements except
the entity-body (see appendix 19.3 for tolerant applications).
The end-of-line marker within an entity-body is defined by its
associated media type, as described in section 3.7.

# Concepts

  * **Age**
    The age of a response is the time since it was sent by, or
    successfully validated with, the origin server.
  * **Entity**
    The payload of the message.
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

# On implementing BNFs as DCGs

In BNFs there are (at least) two ways for writing an optional component.
  1. Using quare brackets.
  2. Using counters.

We give a simple example of the first variant in [1] and a simple example of
the second variant in [2].

~~~{.abnf}
[1] a = b [c]
~~~

~~~{.abnf}
[2] a = b *c
~~~

The question is: how should the parse tree be drawn in these two cases?
The first intuition I had was that in the case of [1] the parse tree for `a`
might exclude a parse tree for `c`, wehereas in the case of [2] the parse tree
for `a` always includes a parse tree for `c` (possibly with void content).

Sometimes a mix of the two variants occurs. Example [3] comes from
the HTTP 1.1 standard.

~~~{.abnf}
[3] message-header = field-name ":" [ field-value ]
    field-value = *( field-content | LWS )
~~~

Example [3] invalidates my former intuition, since a message header with no
field value would have two parse trees (ambiguity in the grammar).

From this I conclude that the BNF notation should not always be directly
reflected in the way the parse tree is constructed.

BNFs are very much directed towards their end result (producing all and only
conforming strings), and not so much to the way in which the end result is
constructed. (Decision procedure versus structural analysis.)

@author Wouter Beek
@see Based on RFC 2616, http://tools.ietf.org/html/rfc2616
@version 2013/07, 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_control)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(gv(gv_file)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic)).
:- use_module(http(rfc2616_version)).
:- use_module(math(math_ext)).
:- use_module(math(radix)).
:- use_module(uri(rfc2396_dcg)).



%! 'Reason-Phrase'(-Tree:compound, ?ReasonPhrase:atom)//
% The 'Reason-Phrase'//2 is intended to give a short textual description
% of the 'Status-Code'//3.
%
% The 'Status-Code'//3 is intended for use by automata and the 'Reason-Phrase'//2
% is intended for the human user.
%
% The client is not required to examine or display the 'Reason-Phrase'//2.

'Reason-Phrase'('Reason-Phrase'(ReasonPhrase), ReasonPhrase) -->
  {nonvar(ReasonPhrase)}, !,
  {atom_codes(ReasonPhrase, Codes)},
  reason_phrase_(Codes).
'Reason-Phrase'('Reason-Phrase'(ReasonPhrase), ReasonPhrase) -->
  reason_phrase_(Codes),
  {atom_codes(ReasonPhrase, Codes)}.
reason_phrase_([H|T]) -->
  [H],
  {\+ code_type(H, cntrl)},
  {\+ memberchk(H, [13,10])},
  reason_phrase_(T).
reason_phrase_([]) --> [].

%! 'Response'(
%!   -Tree:compound,
%!   ?Version:compound,
%!   ?Status:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% After receiving and interpreting a request message, a server responds
% with an HTTP response message.
%
% ~~~{.abnf}
% Response = Status-Line
%            *(( general-header | response-header | entity-header ) CRLF)
%            CRLF
%            [ message-body ]
% ~~~

'Response'(T0, Version, Status, MessageHeaders, MessageBody) -->
  'Status-Line'(T1, Version, Status),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  'CRLF',
  ('message-body'(T3, MessageBody) ; "", {MessageBody = []}),
  {parse_tree(response, [T1,T2,T3], T0)}.

%! 'response-header'//
% ~~~{.abnf}
% response-header = Accept-Ranges
%                 | Age
%                 | ETag
%                 | Location
%                 | Proxy-Authenticate
%                 | Retry-After
%                 | Server
%                 | Vary
%                 | WWW-Authenticate
% ~~~
%
% 'response-header'// field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields MAY be given the semantics of response-
% header fields if all parties in the communication recognize them to
% be 'response-header'// fields. Unrecognized header fields are treated as
% 'entity-header'// fields.
/*
'response-header' --> 'Accept-Ranges'.
'response-header' --> 'Age'.
'response-header' --> etag.
'response-header' --> location.
'response-header' --> proxy_authenticate.
'response-header' --> retry_after.
'response-header' --> server.
'response-header' --> vary.
'response-header' --> www_authenticate.
*/

%! 'Status-Code'(?Status:integer, ?Reason:atom) is nondet.
% The first digit of the 'Status-Code'/2 defines the class of response.
% The last two digits do not have any categorization role.
%
% There are 5 values for the first digit:
%   * `1xx`
%     Informational - Request received, continuing process.
%   * `2xx`
%     Success - The action was successfully received, understood,
%     and accepted.
%   * `3xx`
%     Redirection - Further action must be taken in order to complete
%     the request.
%   * `4xx`
%     Client Error - The request contains bad syntax or cannot be fulfilled.
%   * `5xx`
%     Server Error - The server failed to fulfill an apparently valid request.
%
% The individual values of the numeric status codes defined for
% HTTP/1.1, and an example set of corresponding Reason-Phrase's, are
% presented below. The reason phrases listed here are only
% recommendations -- they MAY be replaced by local equivalents without
% affecting the protocol.

'Status-Code'(100, 'Continue').
'Status-Code'(101, 'Switching Protocols').
'Status-Code'(200, 'OK').
'Status-Code'(201, 'Created').
'Status-Code'(202, 'Accepted').
'Status-Code'(203, 'Non-Authoritative Information').
'Status-Code'(204, 'No Content').
'Status-Code'(205, 'Reset Content').
'Status-Code'(206, 'Partial Content').
'Status-Code'(300, 'Multiple Choices').
'Status-Code'(301, 'Moved Permanently').
'Status-Code'(302, 'Found').
'Status-Code'(303, 'See Other').
'Status-Code'(304, 'Not Modified').
'Status-Code'(305, 'Use Proxy').
'Status-Code'(307, 'Temporary Redirect').
'Status-Code'(400, 'Bad Request').
'Status-Code'(401, 'Unauthorized').
'Status-Code'(402, 'Payment Required').
'Status-Code'(403, 'Forbidden').
'Status-Code'(404, 'Not Found').
'Status-Code'(405, 'Method Not Allowed').
'Status-Code'(406, 'Not Acceptable').
'Status-Code'(407, 'Proxy Authentication Required').
'Status-Code'(408, 'Request Timeout').
'Status-Code'(409, 'Conflict').
'Status-Code'(410, 'Gone').
'Status-Code'(411, 'Length Required').
'Status-Code'(412, 'Precondition Failed').
'Status-Code'(413, 'Request Entity Too Large').
'Status-Code'(414, 'Request URI Too Large').
'Status-Code'(415, 'Unsupported Media Type').
'Status-Code'(416, 'Requested Range not Satisfiable').
'Status-Code'(417, 'Expectation Failed').
'Status-Code'(500, 'Internal Server Error').
'Status-Code'(501, 'Not Implemented').
'Status-Code'(502, 'Bad Gateway').
'Status-Code'(503, 'Service Unavailable').
'Status-Code'(504, 'Gateway Timeout').
'Status-Code'(505, 'HTTP Version not supported').

%! 'Status-Code'(-Tree:compound, ?Status:integer, ?Reason:atom)//
% A 3-digit integer result code of the attempt to understand and satisfy
% the request.
%
% HTTP status codes are extensible. HTTP applications are not required
% to understand the meaning of all registered status codes, though such
% understanding is obviously desirable. However, applications MUST
% understand the class of any status code, as indicated by the first
% digit, and treat any unrecognized response as being equivalent to the
% `x00` status code of that class, with the exception that an
% unrecognized response MUST NOT be cached.

'Status-Code'('Status-Code'(Status), Status, Reason1) -->
  {nonvar(Status)}, !,
  {
    'Status-Code'(Status, Reason2),
    decimal_to_digits(Status, [D1,D2,D3])
  },
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3),
  % Use the default reason for the given status code.
  {(var(Reason1) -> Reason1 = Reason2 ; true)}.
'Status-Code'('Status-Code'(Status), Status, _Reason1) -->
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3),
  {
    digits_to_decimal([D1,D2,D3], Status),
    'Status-Code'(Status, _Reason2)
  }.
'Status-Code'('Status-Code'(Status), Status, _Reason) -->
  extension_code(Status).

%! 'Status-Line'(-Tree:compound, ?Version:compound, ?Status:compound)//
% The first line of a response// message.

'Status-Line'(
  'Status-Line'(T1,T2,T3),
  version(Major, Minor),
  status(Status, Reason)
) -->
  'HTTP-Version'(T1, Major, Minor),
  'SP',
  'Status-Code'(T2, Status, Reason),
  'SP',
  'Reason-Phrase'(T3, Reason),
  'CRLF'.

