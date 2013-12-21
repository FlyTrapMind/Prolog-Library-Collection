:- module(
  rfc2616_connection,
  [
    'Connection'//2 % -ParseTree:compound
                    % ?Connection:compound
  ]
).

/** <module> RFC 2616 connection

DCG for the `Connection` header in responses and requests in RFC 2616.

# Datatypes

## Connection

~~~{.pl}
'Connection'(
  ConnectionTokens:list(atom)
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(flp(rfc2616_abnf)).
:- use_module(http(rfc2616_generic)).



%! 'Connection'(-ParseTree:compound, ?ConnectionTokens:list(atom))//
% # Syntax
%
% ~~~{.abnf}
% Connection = "Connection" ":" 1#(connection-token)
% ~~~
%
% # Semantics
%
% The `Connection` general-header field allows the sender to specify options
%  that are desired for that particular connection and
%  MUST NOT be communicated by proxies over further connections.
%
% # Pragmatics
%
% ## Proxy behavior
%
% HTTP/1.1 proxies MUST parse the `Connection` header field
%  before a message is forwarded and,
%  for each `connection-token` in this field,
%  remove any header field(s) from the message with the same name
%  as the `connection-token`.
% Connection options are signaled by the presence of a `connection-token`
%  in the `Connection` header field, not by any corresponding additional
%  header field(s), since the additional header field may not be sent
%  if there are no parameters associated with that connection option.
%
% ## Forbidden values
%
% Message headers listed in the `Connection` header MUST NOT include
%  end-to-end headers, such as `Cache-Control`.
%
% @tbd Specify all end-to-end headers.

'Connection'('Connection'(Ts), 'Connection'(ConnectionTokens)) -->
  "Connection:",
  abnf_list2('connection-token', 1-_, Ts, ConnectionTokens).



%! 'connection-token'(-ParseTree:compound, ?ConnectionToken:atom)//
% # Syntax
%
% ~~~{.abnf}
% connection-token = token
% ~~~
%
% # Semantics
%
% ## `close` value
%
% HTTP/1.1 defines the `"close"` connection option for the sender to signal
%  that the connection will be closed after completion of the response.
% Header [1] in either the request or the response header fields
%  indicates that the connection SHOULD NOT be considered persistent
%  after the current request/response is complete.
%
% ~~~{.http}
% [1]   Connection: close
% ~~~
%
% # Pragmatics
%
% HTTP/1.1 applications that do not support persistent connections MUST
%  include the `"close"` connection option in every message.
%
% ## Backwards compatibility
%
% A system receiving an HTTP/1.0 (or lower-version) message that
%  includes a Connection header MUST, for each connection-token in this
%  field, remove and ignore any header field(s) from the message with
%  the same name as the connection-token. This protects against mistaken
%  forwarding of such header fields by pre-HTTP/1.1 proxies.

'connection-token'('connection-token'(ConnectionToken), ConnectionToken) -->
  token(ConnectionToken).

