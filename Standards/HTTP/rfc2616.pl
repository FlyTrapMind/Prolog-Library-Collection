:- module(
  rfc2616,
  [
    'HTTP-message'//1 % -ParseTree:compound
  ]
).

/** <module> RFC 2616

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
    * Client information
    * Request modifiers
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

--

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic)).



%! 'field-content'(-ParseTree:compound)//
% # Syntax
%
% ~~~{.abnf}
% field-content = <the OCTETs making up the field-value and consisting of
%                 either *TEXT or combinations of token, separators,
%                 and quoted-string>
% ~~~
%
% ## White-space
%
% The `field-content` does not include any leading or trailing `LWS`:
%  linear white space occurring before the first non-whitespace character
%  of the `field-value` or after the last non-whitespace character
%  of the `field-value`.
% Such leading or trailing `LWS` MAY be removed without changing
%  the semantics of the field value.
% Any `LWS` that occurs between `field-content` MAY be replaced with
%  a single `SP` before interpreting the field value or forwarding
%  the message downstream.
%
% @tbd This BNF rules is **very** unclear!

'field-content'('field-content'(T), [FieldContent]) -->
  dcg_multi1('TEXT', _-_, TEXTs),
  {atom_codes(FieldContent, TEXTs)}.
'field-content'(T0, FieldContent) -->
  dcg_multi2('_field-content', _-_, Ts, FieldContent),
  dcg_multi(separator),
  {parse_tree('field-content', Ts, T0)}.
'_field-content'(T1, FieldContent) -->
  dcg_multi(separator),
  token(T1, FieldContent).
'_field-content'(T1, FieldContent) -->
  dcg_multi(separator),
  'quoted-string'(T1, FieldContent).



%! 'field-name'(-ParseTree:compound, ?FieldName:atom)//
% ~~~{.abnf}
% field-name = token
% ~~~

'field-name'('field-name'(T1), FieldName) -->
  token(T1, FieldName).



%! 'field-value'(-ParseTree:compound, ?FieldValue:atom)//
% ~~~{.abnf}
% field-value = *( field-content | LWS )
% ~~~

'field-value'(T0, FieldContents) -->
  dcg_multi('_field-value'),
  dcg_multi('LWS', Ts, FieldContents),
  {parse_tree('field-value', Ts, T0)}.
'_field-value'(T1, FieldContents) -->
   dcg_multi('LWS'),
  'field-content'(T1, FieldContents).



%! 'general-header'(-ParseTree:compound)//
% # Syntax
% 'general-header' = 'Cache-Control'       ; Section 14.9
%                  | 'Connection'          ; Section 14.10
%                  | 'Date'                ; Section 14.18
%                  | 'Pragma'              ; Section 14.32
%                  | 'Trailer'             ; Section 14.40
%                  | 'Transfer-Encoding'   ; Section 14.41
%                  | 'Upgrade'             ; Section 14.42
%                  | 'Via'                 ; Section 14.45
%                  | 'Warning'             ; Section 14.46
%
% # Semantics
%
% Header fields that apply to the message, not to the entity transferred.
%
% There are a few header fields which have general applicability for both
%  request and response messages, but which do not apply to
%  the entity being transferred.
% These header fields apply only to the message being transmitted.
%
% # Pragmatics
%
% ## Extensions
%
% General-header field names can be extended reliably only in combination with
%  a change in the protocol version.
% However, new or experimental header fields may be given the semantics of
%  general header fields if all parties in the communication recognize them
%  to be general-header fields.
% Unrecognized header fields are treated as entity-header fields.

'general-header'(T1) -->
  'Cache-Control'(T1).
'general-header'(T1) -->
  'Connection'(T1).
'general-header'(T1) -->
  'Date'(T1).
'general-header'(T1) -->
  'Pragma'(T1).
'general-header'(T1) -->
  'Trailer'(T1).
'general-header'(T1) -->
  'Transfer-Encoding'(T1).
'general-header'(T1) -->
  'Upgrade'(T1).
'general-header'(T1) -->
  'Via'(T1).
'general-header'(T1) -->
  'Warning'(T1).



%! 'generic-message'(
%!   -ParseTree:compound,
%!   ?MessageHeaders:list(pair(atom,atom)),
%!   ?Body:list(octec)
%! )//
% `Request` and `Response` messages use the generic message format
%  of RFC 822 for transferring entities (the payload of the message).
% Both types of message consist of
%  - a start-line,
%  - zero or more header fields (also known as "headers"),
%  - an empty line (i.e., a line with nothing preceding the `CRLF`)
%    indicating the end of the header fields, and
%  - possibly a message-body.

% ~~~{.abnf}
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ~~~

'generic-message'(
  'generic-message'(T1,'message-headers'(T2s),T3),
  MessageHeaders,
  MessageBody
) -->
  start_line(T1),
  dcg_multi('_generic-message', T2s, MessageHeaders),
  'CRLF',
  (
    'message-body'(T3, MessageBody)
  ;
    "",
    MessageBody = []
  ),
  {parse_tree(generic_message, [T1,T2,T3], T0)}.
'_generic-message'(T1, Header) -->
  'message-header'(T1, Header),
  'CRLF'.



%! 'HTTP-message'(-Tree:compound)//
% HTTP messages consist of requests from client to server
%  and responses from server to client.
%
% ~~~{.abnf}
% HTTP-message = Request | Response   ; HTTP/1.1 messages
% ~~~

'HTTP-message'('HTTP-message'(T1)) -->
  'Request'(T1, _Method, _URI, _Version, _MessageHeaders, _MessageBody).
'HTTP-message'('HTTP-message'(T1)) -->
  'Response'(T1, _Version, _Status, _MessageHeaders, _MessageBody).



%! 'message-body'(-ParseTree:compound, ?Body)//
% The message-body (if any) of an HTTP message is used to carry
%  the entity-body associated with the request or response.
%
% The message-body differs from the entity-body only when
%  a transfer-coding has been applied,
%  as indicated by the `Transfer-Encoding` header field.
%
% # Syntax
%
% ~~~{.abnf}
% message-body = entity-body | <entity-body encoded as per Transfer-Encoding>
% ~~~
%
% # Pragmatics
%
% `Transfer-Encoding` MUST be used to indicate any transfer-codings applied by
%  an application to ensure safe and proper transfer of the message.
% `Transfer-Encoding` is a property of the message, not of the entity,
%  and thus MAY be added or removed by any application
%  along the request/response chain.
% However, section 3.6 places restrictions on when
%  certain transfer-codings may be used.
%
% The rules for when a message-body is allowed in a message differ
%  for requests and responses.
%
% ## Request
%
% The presence of a message-body in a request is signaled by
%  the inclusion of a `Content-Length` or `Transfer-Encoding` header field
%  in the request's message-headers.
% A message-body MUST NOT be included in a request if
%  the specification of the request method does not allow
%  sending an entity-body in requests.
% A server SHOULD read and forward a message-body on any request;
%  if the request method does not include defined semantics
%  for an entity-body, then the message-body SHOULD be ignored
%  when handling the request.
%
% ## Response
%
% For response messages, whether or not a message-body is included
%  with a message is dependent on both the request method
%  and the response status code.
% All responses to the HEAD request method MUST NOT include a message-body,
%  even though the presence of entity-header fields might lead one to believe
%  they do.
% All `1xx` (informational), `204` (no content), and `304` (not modified)
%  responses MUST NOT include a message-body.
% All other responses do include a message-body,
%  although it MAY be of zero length.
%
% ## Message length
%
% The transfer-length of a message is the length of the message-body as
%  it appears in the message; that is, after any transfer-codings
%  have been applied.
% When a message-body is included with a message,
%  the transfer-length of that body is determined by one of the following
%  (in order of precedence):
%    1. Any response message which "MUST NOT" include a message-body (such
%       as the `1xx`, `204`, and `304` responses and any response to a `HEAD`
%       request) is always terminated by the first empty line after the
%       header fields, regardless of the entity-header fields present in
%       the message.
%    2. If a `Transfer-Encoding` header field (section 14.41) is present and
%       has any value other than "identity", then the transfer-length is
%       defined by use of the "chunked" transfer-coding (section 3.6),
%       unless the message is terminated by closing the connection.
%    3. If a `Content-Length` header field (section 14.13) is present, its
%       decimal value in OCTETs represents both the entity-length and the
%       transfer-length. The `Content-Length` header field MUST NOT be sent
%       if these two lengths are different (i.e., if a `Transfer-Encoding`
%       header field is present). If a message is received with both a
%       `Transfer-Encoding` header field and a `Content-Length` header field,
%       the latter MUST be ignored.
%    4. If the message uses the media type `multipart/byteranges`, and the
%       transfer-length is not otherwise specified, then this self-
%       elimiting media type defines the transfer-length. This media type
%       MUST NOT be used unless the sender knows that the recipient can parse
%       it; the presence in a request of a `Range` header with multiple byte-
%       range specifiers from a 1.1 client implies that the client can parse
%       `multipart/byteranges` responses.
%       A range header might be forwarded by a 1.0 proxy that does not
%       understand `multipart/byteranges`; in this case the server MUST
%       delimit the message using methods defined in items 1,3 or 5 of
%       this section.
%    5. By the server closing the connection. (Closing the connection
%       cannot be used to indicate the end of a request body, since that
%       would leave no possibility for the server to send back a response.)
%
% For compatibility with HTTP/1.0 applications, HTTP/1.1 requests
%  containing a message-body MUST include
%  a valid `Content-Length` header field
%  unless the server is known to be HTTP/1.1 compliant.
% If a request contains a message-body and a `Content-Length` is not given,
%  the server SHOULD respond with `400` (bad request)
%  if it cannot determine the length of the message,
%  or with `411` (length required) if it wishes to insist on
%  receiving a valid `Content-Length`.
%
% All HTTP/1.1 applications that receive entities MUST accept the
%  "chunked" transfer-coding (section 3.6), thus allowing this mechanism
%  to be used for messages when the message length cannot be determined
%  in advance.
%
% Messages MUST NOT include both a `Content-Length` header field and
%  a non-identity transfer-coding.
% If the message does include a non-identity transfer-coding,
%  the `Content-Length` MUST be ignored.
%
% When a `Content-Length` is given in a message where a message-body is
%  allowed, its field value MUST exactly match the number of `OCTET`s
%  in the message-body.
% HTTP/1.1 user agents MUST notify the user when an invalid length
% is received and detected.
%
% @tbd How to implement the encoded case?

'message-body'(T1, Body) -->
  'entity-body'(T1, Body).



%! 'message-header'(-ParseTree:compound, ?MessageHeader:pair(atom,atom))//
% HTTP header fields, which include `general-header`, `request-header`,
%  `response-header`, and `entity-header` fields,
%  follow the same generic format as that given in Section 3.1 of RFC 822.
%
% # Syntax
%
% Each header field consists of a name followed by a colon
%  and the field value.
%
% ~~~{.abnf}
% message-header = field-name ":" [ field-value ]
% ~~~
%
% ## Case-sensitivity
%
% Field names are case-insensitive.
%
% ## White-space
%
% The field value MAY be preceded by any amount of `LWS`,
%  though a single `SP` is preferred.
%
% ## Multi-line fields
%
% Header fields can be extended over multiple lines
%  by preceding each extra line with at least one `SP` or `HT`.
%
% # Pragmatics
%
% Applications ought to follow "common form", where one is known or indicated,
%  when generating HTTP constructs, since there might exist
%  some implementations that fail to accept anything beyond the common forms.
%
% ## Order
%
% The order in which header fields with differing field names are received
%  is not significant.
% However, it is "good practice" to send general-header fields first,
%  followed by `request-header` or `response-header` fields,
%  and ending with the `entity-header` fields.
%
% ## Multiple fields with the same name
%
% Multiple `message-header` fields with the same `field-name` MAY be present
%  in a message if and only if the entire `field-value` for that header field
%  is defined as a comma-separated list [i.e., `#(values)`].
% It MUST be possible to combine the multiple header fields into one
%  `"field-name: field-value"` pair, without changing the semantics
%  of the message, by appending each subsequent `field-value` to the first,
%  each separated by a comma.
% The order in which header fields with the same `field-name` are received
%  is therefore significant to the interpretation of the combined field value,
%  and thus a proxy MUST NOT change the order of these field values
%  when a message is forwarded.
%
% @see RFC 822

'message-header'(T0, Name-Value) -->
  'field-name'(T1, Name),
  ":",
  (
    'field-value'(T2, Value)
  ;
    ""
  ),
  {parse_tree('message-header', [T1,T2], T0)}.



%! 'start-line'(-ParseTree:compound)//
% In the interest of robustness, servers SHOULD ignore any empty line(s)
%  received where a `Request-Line` is expected.
% In other words, if the server is reading the protocol stream
%  at the beginning of a message and receives a `CRLF` first,
%  it should ignore the `CRLF`.
%
% Certain buggy HTTP/1.0 client implementations generate extra `CRLF`'s
%  after a `POST` request.
% To restate what is explicitly forbidden by the BNF,
%  an HTTP/1.1 client MUST NOT preface or follow a request
%  with an extra `CRLF`.

'start-line'('start-line'(T1)) -->
  'Request-Line'(T1).
'start-line'('start-line'(T1)) -->
  'Status-Line'(T1).

