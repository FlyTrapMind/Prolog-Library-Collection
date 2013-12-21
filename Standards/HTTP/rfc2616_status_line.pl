:- module(
  rfc2616_status_line,
  [
    'Status-Line'//3 % -ParseTree:compound
                     % ?Version:compound
                     % ?Status:compound
  ]
).

/** <module> RFC 2616 status line

DCG for RFC 2616 status lines.

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_version)).
:- use_module(math(radix)).




%! 'extension-code'(-ParseTree:compound, ?Status:between(100,999))// .
% HTTP status codes are extensible.
%
% # Syntax
%
% ~~~{.abnf}
% extension-code = 3DIGIT
% ~~~
%
% # Pramatics
%
% HTTP applications are not required to understand the meaning of
%  all registered status codes, though such understanding is
%  obviously desirable.
%
% ## Status code class
%
% However, applications MUST understand the class of any status code,
%  as indicated by the first digit, and treat any unrecognized response
%  as being equivalent to the `x00` status code of that class,
%  with the exception that an unrecognized response MUST NOT be cached.
%
% ### Example
%
% For example, if an unrecognized status code of `431` is received
%  by the client, it can safely assume that there was something wrong with
%  its request and treat the response as if it had received
%  a `400` status code.
% In such cases, user agents SHOULD present to the user
%  the entity returned with the response, since that entity is likely
%  to include human-readable information which will explain
%  the unusual status.

'extension-code'('extension-code'(Status), Status) -->
  dcg_multi1('DIGIT', 3, Ds),
  {digits_to_decimal(Ds, Status)}.



%! 'Reason-Phrase'(-ParseTree:compound, ?Reason:atom)//
% ~~~{.abnf}
% Reason-Phrase = *<TEXT, excluding CR, LF>
% ~~~

'Reason-Phrase'('Reason-Phrase'(Reason), Reason) -->
  dcg_multi1('_Reason-Phrase', _-_, Cs),
  {atom_codes(Reason, Cs)}.
'_Reason-Phrase'(C) -->
  'TEXT'(C),
  {\+ phrase('CR', [C]), \+ phrase('LF', [C])}.



%! 'Status-Code'(-Tree:compound, ?Status:between(100,505), ?Reason:atom)// .
%! 'Status-Code'(
%!   -ParseTree:compound,
%!   ?Status:between(100,999),
%!   ?Reason:atom
%! )// .
% # Syntax
%
% The `Status-Code` element is a 3-digit integer result code of the attempt
%  to understand and satisfy the request.
%
% ~~~{.abnf}
% Status-Code = "100"   ; Section 10.1.1: Continue
%             | "101"   ; Section 10.1.2: Switching Protocols
%             | "200"   ; Section 10.2.1: OK
%             | "201"   ; Section 10.2.2: Created
%             | "202"   ; Section 10.2.3: Accepted
%             | "203"   ; Section 10.2.4: Non-Authoritative Information
%             | "204"   ; Section 10.2.5: No Content
%             | "205"   ; Section 10.2.6: Reset Content
%             | "206"   ; Section 10.2.7: Partial Content
%             | "300"   ; Section 10.3.1: Multiple Choices
%             | "301"   ; Section 10.3.2: Moved Permanently
%             | "302"   ; Section 10.3.3: Found
%             | "303"   ; Section 10.3.4: See Other
%             | "304"   ; Section 10.3.5: Not Modified
%             | "305"   ; Section 10.3.6: Use Proxy
%             | "307"   ; Section 10.3.8: Temporary Redirect
%             | "400"   ; Section 10.4.1: Bad Request
%             | "401"   ; Section 10.4.2: Unauthorized
%             | "402"   ; Section 10.4.3: Payment Required
%             | "403"   ; Section 10.4.4: Forbidden
%             | "404"   ; Section 10.4.5: Not Found
%             | "405"   ; Section 10.4.6: Method Not Allowed
%             | "406"   ; Section 10.4.7: Not Acceptable
%             | "407"   ; Section 10.4.8: Proxy Authentication Required
%             | "408"   ; Section 10.4.9: Request Time-out
%             | "409"   ; Section 10.4.10: Conflict
%             | "410"   ; Section 10.4.11: Gone
%             | "411"   ; Section 10.4.12: Length Required
%             | "412"   ; Section 10.4.13: Precondition Failed
%             | "413"   ; Section 10.4.14: Request Entity Too Large
%             | "414"   ; Section 10.4.15: Request-URI Too Large
%             | "415"   ; Section 10.4.16: Unsupported Media Type
%             | "416"   ; Section 10.4.17: Requested range not satisfiable
%             | "417"   ; Section 10.4.18: Expectation Failed
%             | "500"   ; Section 10.5.1: Internal Server Error
%             | "501"   ; Section 10.5.2: Not Implemented
%             | "502"   ; Section 10.5.3: Bad Gateway
%             | "503"   ; Section 10.5.4: Service Unavailable
%             | "504"   ; Section 10.5.5: Gateway Time-out
%             | "505"   ; Section 10.5.6: HTTP Version not supported
%             | extension-code
% ~~~
%
% # Semantics
%
% The `Reason-Phrase` is intended to give a short textual description of
%  the `Status-Code`.
% The first digit of the `Status-Code` defines the class of response.
% The last two digits do not have any categorization role.
% There are 5 values for the first digit:
%   - `1xx`: **Informational**
%     Request received, continuing process.
%   - `2xx`: **Success**
%     The action was successfully received, understood, and accepted.
%   - `3xx`: **Redirection**
%     Further action must be taken in order to complete the request.
%   - `4xx`: **Client Error**
%     The request contains bad syntax or cannot be fulfilled.
%   - `5xx`: **Server Error**
%     The server failed to fulfill an apparently valid request.
%
% # Pragmatics
%
% ## Human or machine
%
% The `Status-Code` is intended for use by automata
%  and the `Reason-Phrase` is intended for the human user.
%
% ## Display of reason phrase
%
% The client is not required to examine or display the `Reason-Phrase`.
%
% ## Default values
%
% The individual values of the numeric status codes defined for HTTP/1.1,
%  and an example set of corresponding Reason-Phrase's, are presented below.
% The reason phrases listed here are only recommendations
%  -- they MAY be replaced by local equivalents without affecting
%  the protocol.

'Status-Code'(100, 'Continue') -->
  "100".
'Status-Code'(101, 'Switching Protocols') -->
  "101".
'Status-Code'(200, 'OK') -->
  "200".
'Status-Code'(201, 'Created') -->
  "201".
'Status-Code'(202, 'Accepted') -->
  "202".
'Status-Code'(203, 'Non-Authoritative Information') -->
  "203".
'Status-Code'(204, 'No Content') -->
  "204".
'Status-Code'(205, 'Reset Content') -->
  "205".
'Status-Code'(206, 'Partial Content') -->
  "206".
'Status-Code'(300, 'Multiple Choices') -->
  "300".
'Status-Code'(301, 'Moved Permanently') -->
  "301".
'Status-Code'(302, 'Found') -->
  "302".
'Status-Code'(303, 'See Other') -->
  "303".
'Status-Code'(304, 'Not Modified') -->
  "304".
'Status-Code'(305, 'Use Proxy') -->
  "305".
'Status-Code'(307, 'Temporary Redirect') -->
  "307".
'Status-Code'(400, 'Bad Request') -->
  "400".
'Status-Code'(401, 'Unauthorized') -->
  "401".
'Status-Code'(402, 'Payment Required') -->
  "402".
'Status-Code'(403, 'Forbidden') -->
  "403".
'Status-Code'(404, 'Not Found') -->
  "404".
'Status-Code'(405, 'Method Not Allowed') -->
  "405".
'Status-Code'(406, 'Not Acceptable') -->
  "406".
'Status-Code'(407, 'Proxy Authentication Required') -->
  "407".
'Status-Code'(408, 'Request Time-out') -->
  "408".
'Status-Code'(409, 'Conflict') -->
  "409".
'Status-Code'(410, 'Gone') -->
  "410".
'Status-Code'(411, 'Length Required') -->
  "411".
'Status-Code'(412, 'Precondition Failed') -->
  "412".
'Status-Code'(413, 'Request Entity Too Large') -->
  "413".
'Status-Code'(414, 'Request-URI Too Large') -->
  "414".
'Status-Code'(415, 'Unsupported Media Type') -->
  "415".
'Status-Code'(416, 'Requested range not satisfiable') -->
  "416".
'Status-Code'(417, 'Expectation Failed') -->
  "417".
'Status-Code'(500, 'Internal Server Error') -->
  "500".
'Status-Code'(501, 'Not Implemented') -->
  "501".
'Status-Code'(502, 'Bad Gateway') -->
  "502".
'Status-Code'(503, 'Service Unavailable') -->
  "503".
'Status-Code'(504, 'Gateway Time-out') -->
  "504".
'Status-Code'(505, 'HTTP Version not supported') -->
  "505".

% This clause is implicit in RFC 2616.
'Status-Code'('Status-Code'(Status,Reason), Status, Reason) -->
  'Status-Code'(Status, Reason).
'Status-Code'('Status-Code'(T1,T2), Status, Reason) -->
  'extension-code'(T1, Status),
  'Reason-Phrase'(T2, Reason).



%! 'Status-Line'(-ParseTree:compound, ?Version:compound, ?Status:compound)// .
% # Syntax
%
% The first line of a `Response` message is the `Status-Line`,
%  consisting of the protocol version followed by a numeric status code
%  and its associated textual phrase, with each element separated by
%  `SP` characters.
% No `CR` or `LF` is allowed except in the final `CRLF` sequence.
%
% ~~~{.abnf}
% Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
% ~~~
%
% @param ParseTree
% @param Version A compound term of the form
%        `version(Major:nonneg,Minor:nonneg)`
% @param Status A compound term of the form
%        `status(StatusCode:between(100,500),ReasonPhrase:atom)`

'Status-Line'('Status-Line'(T1,T2,T3), Version, status(Status, Reason)) -->
  'HTTP-Version'(T1, Version),
  'SP',
  'Status-Code'(T2, Status, Reason),
  'SP',
  'Reason-Phrase'(T3, Reason),
  'CRLF'.

