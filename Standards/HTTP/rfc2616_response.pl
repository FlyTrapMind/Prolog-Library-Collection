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

:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(flp(rfc2616_abnf)).
:- use_module(http(rfc2616)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic_message)).
:- use_module(http(rfc2616_status_line)).
:- use_module(http_headers(rfc2616_entity_header)).
:- use_module(http_headers(rfc2616_general_header)).
:- use_module(http_headers(rfc2616_response_header)).
:- use_module(http_parameters(rfc2616_range_unit)).



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
  headers(T2s, Headers),
  'CRLF',
  (
    'message-body'(T3, Body)
  ;
    "",
    {Body = []}
  ),
  {parse_tree(response, [T1,headers(T2s),T3], T0)}.

headers([], []) --> [].
headers([T1|Ts], [H|T]) -->
  'general-header'(T1, H),
  'CRLF',
  headers(Ts, T).
headers([T1|Ts], [H|T]) -->
  'response-header'(T1, H),
  'CRLF',
  headers(Ts, T).
headers([T1|Ts], [H|T]) -->
  'entity-header'(T1, H),
  'CRLF',
  headers(Ts, T).

