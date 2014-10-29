:- module(
  rfc2616_version,
  [
    'HTTP-version'//1 % ?Version:compound
  ]
).

/** <module> HTTP version

DCG rule for the HTTP version field.

# RFC 7230

## Syntax

~~~{.abnf}
HTTP-version = HTTP-name "/" DIGIT "." DIGIT
HTTP-name    = %x48.54.54.50 ; "HTTP", case-sensitive
~~~

HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
 of the protocol.

The version of an HTTP message is indicated by an HTTP-Version field
 in the first line of the message.

## Semantics

### Minor

The `<minor>` number is incremented when the changes made to
 the protocol add features which do not change the general message parsing
 algorithm, but which may add to the message semantics and imply
 additional capabilities of the sender.

### Major

The `<major>` number is incremented when the format of a message
 within the protocol is changed.

## Pragmatics

The protocol versioning policy is intended to allow the sender to indicate
 the format of a message and its capacity for understanding
 further HTTP communication,
 rather than the features obtained via that communication.
No change is made to the version number for the addition of
 message components which do not affect communication behavior or
 which only add to extensible field values.

An application that sends a request or response message that includes
 HTTP-Version of `HTTP/1.1` MUST be at least conditionally compliant
 with this specification.
Applications that are at least conditionally compliant with
 this specification SHOULD use an HTTP-Version of `HTTP/1.1`
 in their messages,
 and MUST do so for any message that is not compatible with HTTP/1.0.

### Clients

The HTTP version of an application is the highest HTTP version for
 which the application is at least conditionally compliant.
A client MAY send a lower request version if it is known that the
 server incorrectly implements the HTTP specification, but only after
 the client has attempted at least one normal request and determined
 from the response status code or header fields (e.g., Server) that
 the server improperly handles higher request versions.

### Intermediaries

Proxy and gateway applications need to be careful when forwarding
 messages in protocol versions different from that of the application.
Since the protocol version indicates the protocol capability of the sender,
 a proxy/gateway MUST NOT send a message with a version indicator which is
 greater than its actual version.
If a higher version request is received,
 the proxy/gateway MUST either downgrade the request version,
 or respond with an error, or switch to tunnel behavior.

### Backwards compatibility

Due to interoperability problems with HTTP/1.0 proxies
 discovered since the publication of RFC 2068,
 caching proxies MUST, gateways MAY, and tunnels MUST NOT upgrade
 the request to the highest version they support.
The proxy/gateway's response to that request MUST be in
 the same major version as the request.

Note: Converting between versions of HTTP may involve modification of
 header fields required or forbidden by the versions involved.

# Legacy

RFC 2145 and RFC 2616 used to define HTTP versions differently:

## Syntax

~~~{.abnf}
HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
~~~

Leading zeros MUST be ignored by recipients and MUST NOT be sent.

## Semantics

Note that the major and minor numbers MUST be treated as separate
 integers and that each MAY be incremented higher than a single digit.
Thus, HTTP/2.4 is a lower version than HTTP/2.13, which in turn is
 lower than HTTP/12.3.

--

@author Wouter Beek
@version 2013/07, 2013/12, 2014/05, 2014/10
*/

:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg_rfc(rfc5234_basic)).



%! 'HTTP-name'// is det.

'HTTP-name' -->
  "HTTP".


%! 'HTTP-version'(?Version:compound)// is det.
% ~~~{.abnf}
% HTTP-version = HTTP-name "/" DIGIT "." DIGIT
% HTTP-name    = %x48.54.54.50 ; "HTTP", case-sensitive
% ~~~
%
% @compat RFC 7230

'HTTP-version'(version(Major,Minor)) -->
  'HTTP-name',
  "/",
  'DIGIT'(_, Major),
  ".",
  'DIGIT'(_, Minor).



%! 'obs-HTTP-Version'(?Version:compound)// is det.
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.abnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~
%
% @compat RFC 2616

'obs-HTTP-Version'(version(Major,Minor)) -->
  "HTTP/",
  '*'('DIGIT', _, MajorWeights, []),
  {weights_radix(MajorWeights, Major)},
  ".",
  '*'('DIGIT', _, MinorWeights, []),
  {weights_radix(MinorWeights, Minor)}.
