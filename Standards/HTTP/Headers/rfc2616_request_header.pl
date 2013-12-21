:- module(
  rfc2616_request_header,
  [
    'Request-header'//2 % -ParseTree:compound
                        % ?RequestHeader
  ]
).

/** <module>

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http_headers(rfc2616_accept)).
:- use_module(http_headers(rfc2616_accept_charset)).



%! 'Request-header'(-ParseTree:compound, ?RequestHeader)//
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

'Request-header'('Request-header'(T1), Statements) -->
  'Accept'(T1, Statements).
'Request-header'('Request-header'(T1), Statements) -->
  'Accept-Charset'(T1, Statements).
/*
'Request-header'('Request-header'(T1)) --> 'Accept-Encoding'.
'Request-header'('Request-header'(T1)) --> 'Accept-Language'.
'Request-header'('Request-header'(T1)) --> 'Authorization'.
'Request-header'('Request-header'(T1)) --> 'Expect'.
'Request-header'('Request-header'(T1)) --> 'From'.
'Request-header'('Request-header'(T1)) --> 'Host'.
'Request-header'('Request-header'(T1)) --> 'If-Match'.
'Request-header'('Request-header'(T1)) --> 'If-Modified-Since'.
'Request-header'('Request-header'(T1)) --> 'If-None-Match'.
'Request-header'('Request-header'(T1)) --> 'If_range'.
'Request-header'('Request-header'(T1)) --> 'If-Unmodified-Since'.
'Request-header'('Request-header'(T1)) --> 'Max-Forwards'.
'Request-header'('Request-header'(T1)) --> 'Proxy-Authorization'.
'Request-header'('Request-header'(T1)) --> 'Range'.
'Request-header'('Request-header'(T1)) --> 'Referer'.
'Request-header'('Request-header'(T1)) --> 'TE'.
'Request-header'('Request-header'(T1)) --> 'User-Agent'.
*/

