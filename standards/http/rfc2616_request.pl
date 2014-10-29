:- module(
  rfc2616_request,
  [
    'Request'//5 % ?Method:atom
                 % ?URI:compound
                 % ?Version:compound
                 % ?Headers:list(pair(atom,atom))
                 % ?Body:list(octet)
  ]
).

/** <module> RFC 2616: Request

@author Wouter Beek
@compat RFC 2616
@version 2013/12, 2014/10
*/

:- use_module(plDcg_rfc(rfc2616_basic)).
:- use_module(http(rfc2616_generic_message)).
:- use_module(http(rfc2616_method)).
:- use_module(http(rfc2616_version)).
:- use_module(http_headers(rfc2616_entity_header)).
:- use_module(http_headers(rfc2616_general_header)).
:- use_module(http_headers(rfc2616_request_header)).
:- use_module(http_parameters(rfc2616_request_uri)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(parse_tree)).



%! 'Request'(
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound,
%!   ?Headers:list(pair(atom,atom)),
%!   ?Body:list(octet)
%! )// .
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

'Request'(Method, URI, Version, Headers, Body) -->
  'Request-Line'(Method, URI, Version),
  '*'('Request0', Headers, []),
  'CRLF',
  (   'message-body'(Body)
  ;   {Body = []}
  ).
'Request0'(Header) -->
  (   'general-header'(Header)
  ;   'request-header'(Header)
  ;   'entity-header'(Header)
  ),
  'CRLF'.



%! 'Request-Line'(
%!   ?Method:compound,
%!   ?RequestURI:compound,
%!   ?Version:compound
%! )// .
% The first line of an HTTP request.
%
% ~~~{.abnf}
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
% ~~~

'Request-Line'(Method, RequestURI, Version) -->
  'Method'(Method),
  'SP',
  'Request-URI'(RequestURI),
  'SP',
  'HTTP-version'(Version),
  'CRLF'.

