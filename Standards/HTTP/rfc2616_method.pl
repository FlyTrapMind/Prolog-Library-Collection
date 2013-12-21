:- module(
  rfc2616_method,
  [
    'Method'//2 % -ParseTree:compound
                % ?Method:compound
  ]
).

/** <module> RFC 2616 method

DCG for HTTP methods.

@author Wouter Beek
@version 2013/12
*/

:- use_module(http(rfc2616_generic)).



%~ 'extension-method'(-ParseTree:compound, ?Method:atom)// .
% ~~~{.abnf}
% extension-method = token
% ~~~

'extension-method'('extension-method'(Method), Method) -->
  token(Method).



%! 'Method'(
%!   ?Method:oneof([connect,delete,get,head,options,post,put,send,trace])
%! )// .
%! 'Method'(-ParseTree:compound, ?Method:compound)// .
% The HTTP method.
%
% # Syntax
%
% The method is case-sensitive.
%
% ~~~{.abnf}
% Method = "OPTIONS"   ; Section 9.2
%        | "GET"       ; Section 9.3
%        | "HEAD"      ; Section 9.4
%        | "POST"      ; Section 9.5
%        | "PUT"       ; Section 9.6
%        | "DELETE"    ; Section 9.7
%        | "TRACE"     ; Section 9.8
%        | "CONNECT"   ; Section 9.9
%        | extension-method
% ~~~
%
% # Semantics
%
% The `Method` token indicates the method to be performed on the resource
%  identified by the `Request-URI`.
%
% # Pragmatics
%
% The list of methods allowed by a resource can be specified in
%  an `Allow` header field.
%
% The return code of the response always notifies the client
%  whether a method is currently allowed on a resource,
%  since the set of allowed methods can change dynamically.
%
% ## Method not allowed (`405`, `501`)
%
% An origin server SHOULD return the status code `405` (Method Not Allowed)
%  if the method is known by the origin server but not allowed
%  for the requested resource, and `501` (Not Implemented)
%  if the method is unrecognized or not implemented by the origin server.
%
% ## Required methods
%
% The methods `GET` and `HEAD` MUST be supported
%  by all general-purpose servers.
%
% ## Optional methods
%
% All other methods are OPTIONAL;
%  however, if the above methods are implemented,
%  they MUST be implemented with the same semantics
%  as those specified in section 9.

'Method'(options) -->
  "OPTIONS".
'Method'(get) -->
  "GET".
'Method'(head) -->
  "HEAD".
'Method'(post) -->
  "POST".
'Method'(put) -->
  "PUT".
'Method'(delete) -->
  "DELETE".
'Method'(trace) -->
  "TRACE".
'Method'(connect) -->
  "CONNECT".

'Method'(method(M), method(M)) -->
  'Method'(M).
'Method'(method(T1), method(M)) -->
  'extension-method'(T1, M).

