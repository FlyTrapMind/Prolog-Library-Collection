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



%! 'response-header'(-ParseTree:compound, ?Header)//
% The response-header fields allow the server to pass additional information
%  about the response which cannot be placed in the `Status-Line`.
%
% # Syntax
%
% ~~~{.abnf}
% response-header = Accept-Ranges        ; Section 14.5
%                 | Age                  ; Section 14.6
%                 | ETag                 ; Section 14.19
%                 | Location             ; Section 14.30
%                 | Proxy-Authenticate   ; Section 14.33
%                 | Retry-After          ; Section 14.37
%                 | Server               ; Section 14.38
%                 | Vary                 ; Section 14.44
%                 | WWW-Authenticate     ; Section 14.47
% ~~~
%
% # Semantics
%
% These header fields give information about the server
%  and about further access to the resource identified by the `Request-URI`.
%
% ## Unrecognized
%
% Unrecognized header fields are treated as entity-header fields.
%
% # Pragmatics
%
% Response-header field names can be extended reliably only
%  in combination with a change in the protocol version.
% However, new or experimental header fields MAY be given the semantics of
%  response-header fields if all parties in the communication
%  recognize them to be response-header fields.

'response-header'('response-header'(T1), RangeUnits) -->
  'Accept-Ranges'(T1, RangeUnits).
'response-header' -->
  'Age'.
'response-header' -->
  'ETag'.
'response-header' -->
  'Location'.
'response-header' -->
  'Proxy-Authenticate'.
'response-header' -->
  'Retry-After'.
'response-header' -->
  'Server'.
'response-header' -->
  'Vary'.
'response-header' -->
  'WWW-Authenticate'.



%! 'Accept-Ranges'(-ParseTree:compound, ?RangeUnits:list(atom))//
% The `Accept-Ranges` response-header field allows the server to indicate
%  its acceptance of range requests for a resource.
%
% ~~~{.abnf}
% Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
% acceptable-ranges = 1#range-unit | "none"
% ~~~
%
% Origin servers that accept byte-range requests MAY send
% ~~~{.abnf}
% Accept-Ranges: bytes
% ~~~
% but are not required to do so.
%
% Clients MAY generate byte-range requests without having received this header
%  for the resource involved.
%
% @see Range units are defined in [rfc2616_range_unit].
%
% Servers that do not accept any kind of range request for a resource MAY send
% ~~~{.abnf}
% Accept-Ranges: none
% ~~~
% to advise the client not to attempt a range request.

'Accept-Ranges'('Accept-Ranges'(T1), RangeUnits) -->
  "Accept-Ranges:",
  'acceptable-ranges'(T1, RangeUnits).

'acceptable-ranges'(T0, RangeUnits) -->
  abnf_list2('range-unit', 1-_, Ts, RangeUnits),
  {parse_tree('acceptable-ranges', Ts, T0)}.
'acceptable-ranges'('acceptable-ranges'(none), []) -->
  "none".

