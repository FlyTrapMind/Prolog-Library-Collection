:- module(
  rfc2616_response,
  [
    'Response'//5 % -ParseTree:compound
                  % ?Version:compound
                  % ?Status:compound
                  % ?Headers:list(pair(atom,atom))
                  % ?Body:list(code)
  ]
).

/** <module> RFC 2616 response

DCG for RFC 2616 response.

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_abnf)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_range_unit)).
:- use_module(http(rfc2616_status_line)).



%! 'Response'(
%!   -ParseTree:compound,
%!   ?Version:compound,
%!   ?Status:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% After receiving and interpreting a request message,
%  a server responds with an HTTP response message.
%
% ~~~{.abnf}
% Response = Status-Line
%            *(( general-header | response-header | entity-header ) CRLF)
%            CRLF
%            [ message-body ]
% ~~~

'Response'(T0, Version, Status, Headers, Body) -->
  'Status-Line'(T1, Version, Status),
  dcg_multi2('_Response', T2s, Headers),
  'CRLF',
  (
    'message-body'(T3, Body)
  ;
    "",
    {Body = []}
  ),
  {parse_tree(response, [T1,headers(T2s),T3], T0)}.
'_Response'(T1, H) -->
  'general-header'(T1, H),
  'CRLF'.
'_Response'(T1, H) -->
  'response-header'(T1, H),
  'CRLF'.
'_Response'(T1, H) -->
  'entity-header'(T1, H),
  'CRLF'.

