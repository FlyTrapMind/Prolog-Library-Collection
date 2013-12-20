:- module(
  rfc2616_request_header,
  [
    'Request-header'//1 % -ParseTree:compound
  ]
).

/** <module>

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/



%! 'Request-header'(-ParseTree:compound)//
% # Syntax
%
% ~~~{.abnf}
% Request-header = Accept
%                | Accept-Charset
%                | Accept-Encoding
%                | Accept-Language
%                | Authorization
%                | Expect
%                | From
%                | Host
%                | If-Match
%                | If-Modified-Since
%                | If-None-Match
%                | If-Range
%                | If-Unmodified-Since
%                | Max-Forwards
%                | Proxy-Authorization
%                | Range
%                | Referer
%                | TE
%                | User-Agent
% ~~~
%
% # Semantics
%
% The request-header fields allow the client to pass additional information
%  about the request, and about the client itself, to the server.
%
% These fields act as request modifiers, with semantics equivalent to
%  the parameters on a programming language method invocation.
%
% # Pragmatics
%
% ## Extensions
%
% Request-header field names can be extended reliably only in combination with
%  a change in the protocol version.
% However, new or experimental header fields MAY be given the semantics of
%  request-header fields if all parties in the communication recognize them
%  to be request-header fields.
% Unrecognized header fields are treated as entity-header fields.

'Request-header' --> 'Accept'.
'Request-header' --> 'Accept-Charset'.
'Request-header' --> 'Accept-Encoding'.
'Request-header' --> 'Accept-Language'.
'Request-header' --> 'Authorization'.
'Request-header' --> 'Expect'.
'Request-header' --> 'From'.
'Request-header' --> 'Host'.
'Request-header' --> 'If-Match'.
'Request-header' --> 'If-Modified-Since'.
'Request-header' --> 'If-None-Match'.
'Request-header' --> 'If_range'.
'Request-header' --> 'If-Unmodified-Since'.
'Request-header' --> 'Max-Forwards'.
'Request-header' --> 'Proxy-Authorization'.
'Request-header' --> 'Range'.
'Request-header' --> 'Referer'.
'Request-header' --> 'TE'.
'Request-header' --> 'User-Agent'.

