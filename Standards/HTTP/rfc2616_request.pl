:- module(
  rfc2616_request,
  [
    'Request'//6 % -ParseTree:compound
                 % ?Method:atom
                 % ?URI:compound
                 % ?Version:compound
                 % ?Headers:list(pair(atom,atom))
                 % ?Body:list(octet)
  ]
).

/** <module> RFC 2616 request

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_version)).



%! 'Request'(
%!   -ParseTree:compound,
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound,
%!   ?Headers:list(pair(atom,atom)),
%!   ?Body:list(octet)
%! )//
% An HTTP request.
%
% ~~~{.abnf}
% Request = Request-Line
%           *(( general-header | request-header | entity-header ) CRLF)
%           CRLF
%           [ message-body ]
% ~~~
%
% ## Requested resource determination
%
% In the order of preference:
%   1. 'Request-URI'//5 is an absolute_uri//2.
%   2. message_headers//2 includes a `Host` header field.
%   3. Respond with `400 (Bad Request)`.
%
% @tbd Implement specific support for 'general-header'//, 'request-header'//,
%      and 'entity-header'//.
% @tbd Implement requested resource determination.

'Request'(T0, Method, URI, Version, Headers, Body) -->
  'Request-Line'(T1, Method, URI, Version),
  dcg_multi2('_Request', _-_, Ts, Headers),
  'CRLF',
  (
    'message-body'(T3, Body)
  ;
    ""
  ),
  {parse_tree(request, [T1,T2,T3], T0)}.
'_Request'(T1, Header) -->
  'general-header'(T1, Header),
  'CRLF'.
'_Request'(T1, Header) -->
  'request-header'(T1, Header),
  'CRLF'.
'_Request'(T1, Header) -->
  'entity-header'(T1, Header),
  'CRLF'.



%! 'Request-Line'(
%!   -Tree:compound,
%!   ?Method:compound,
%!   ?URI:compound,
%!   ?Version:compound
%! )//
% The first line of an HTTP request.
%
% ~~~{.abnf}
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
% ~~~

%uri(Scheme, Authority, Path, Query)
'Request-Line'('Request-Line'(T1,T2,T3), Method, URI, Version) -->
  'Method'(T1, Method),
  'SP',
  'Request-URI'(T2, Scheme, Authority, Path, Query),
  'SP',
  'HTTP-Version'(T3, Major, Minor),
  'CRLF'.

