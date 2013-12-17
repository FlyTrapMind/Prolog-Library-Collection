:- module(
  rfc2616_codings,
  [
    'content-coding'//2, % -ParseTree:compound
                         % ?ContentCoding:oneof([compress,deflate,gzip,identity])
    'transfer-coding'//3 % -ParseTree:compound
                         % ?TransferCoding:oneof([chunked,compress,deflate,gzip,identity])
                         % ?Parameters:list(kvpair(atom,atom))
  ]
).

/** <module> RFC 2616 codings

Support for RFC 2616 content and transfer codings.

Content coding is primarily used for compression.
Transfer coding is primarily used for safety.

Content coding is a property of the original entity.
Transfer coding is a property of the message.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http(rfc2616_generic)).



%! 'content-coding'(
%!   -ParseTree:compound,
%!   ?ContentCoding:oneof([compress,deflate,gzip,identity])
%! )//
%
% HTTP/1.1 uses content-coding values in the `Accept-Encoding`
%  and `Content-Encoding` header fields.
%
% # Syntax
%
% All content-coding values are case-insensitive.
%
% ~~~{.abnf}
% content-coding = token
% ~~~
%
% # Semantics
%
% Content coding values indicate an encoding transformation that has been
%  or can be applied to an entity.
%
% # Pragmatics
%
% Content codings are primarily used to allow a document to be compressed
%  or otherwise usefully transformed without losing the identity of
%  its underlying media type and without loss of information.
% Frequently, the entity is stored in coded form, transmitted directly,
%  and only decoded by the recipient.
%
% ## Decoding
%
% Although the value describes the content-coding,
%  what is more important is that it indicates what decoding mechanism
%  will be required to remove the encoding.
%
% ## Current values
%
% The Internet Assigned Numbers Authority (IANA) acts as a registry for
%  content-coding value tokens.
% Initially, the registry contains the following tokens:
%   * `gzip`
%     An encoding format produced by the file compression program "gzip"
%      (GNU zip) as described in RFC 1952.
%     This format is a Lempel-Ziv coding (LZ77) with a 32 bit CRC.
%   * `compress`
%     The encoding format produced by the common UNIX file compression
%      program "compress".
%     This format is an adaptive Lempel-Ziv-Welch coding (LZW).
%     Use of program names for the identification of encoding formats
%      is not desirable and is discouraged for future encodings.
%     Their use here is representative of historical practice,
%      not good design.
%     For compatibility with previous implementations of HTTP,
%      applications SHOULD consider `x-gzip` and `x-compress` to be equivalent
%      to `gzip` and `compress` respectively.
%   * `deflate`
%     The `zlib` format defined in RFC 1950 in combination with
%     the `deflate` compression mechanism described in RFC 1951.
%   * `identity`
%     The default encoding; the use of no transformation whatsoever.
%     This content-coding is used only in the `Accept-Encoding` header,
%      and SHOULD NOT be used in the `Content-Encoding` header.
%
% @see RFC 1950
% @see RFC 1951
% @see RFC 1952
%
% ## Future values
%
% New content-coding value tokens SHOULD be registered;
%  to allow interoperability between clients and servers,
%  specifications of the content coding algorithms needed to implement
%  a new value SHOULD be publicly available and adequate for
%  independent implementation, and conform to the purpose of content coding
%  defined in this section.
%
% --
%
% @see RFC 2616
% @tbd Content-coding values are case-insensitive.
% @tbd Value `identity` cannot occur in the `Content-Encoding` header.
% @tbd Check whether IANA has registered additional content encodings.

'content-coding'('content-coding'(T0), ContentCoding) -->
  % GENERATING
  % Only canonical values are allowed.
  (
    nonvar(ContentCoding)
  ->
    'content-coding_value'(ContentCoding, _)
  ;
    true
  ),
  
  token(T0, Token),
  
  % PARSING
  % There could be non-canonical content-coding values.
  % Translate these to their canonical value.
  (
    {var(ContentCoding)}
  ->
    'content-coding_value'(CanonicalValue, Tokens),
    memberchk(Token, Tokens).
  ;
    true
  ).

'content-coding_value'(compress, [compress,'x-compress']).
'content-coding_value'(deflate, [defalte]).
'content-coding_value'(gzip, [gzip,'x-gzip']).
'content-coding_value'(identity, [identity]).



%! 'transfer-coding'(
%!   -ParseTree:compound,
%!   ?TransferCoding:oneof([chunked,compress,deflate,gzip,identity]),
%!   ?TransferCoding:atom
%! )//
% Transfer-coding values are used to indicate an encoding transformation
%  that has been, can be, or may need to be applied to an entity-body
%  in order to ensure "safe transport" through the network.
% This differs from a content coding in that the transfer-coding is a
%  property of the message, not of the original entity.
%
% # Syntax
%
% ~~~{.abnf}
% transfer-coding = "chunked" | transfer-extension
% ~~~
%
% All transfer-coding values are case-insensitive.
%
% # Pragmatics
%
% HTTP/1.1 uses transfer-coding values in the `TE` header field
%  and in the `Transfer-Encoding` header field.
%
% ## Message length
%
% Whenever a transfer-coding is applied to a message-body,
%  the set of transfer-codings MUST include "chunked",
%  unless the message is terminated by closing the connection.
% When the "chunked" transfer-coding is used,
%  it MUST be the last transfer-coding applied to the message-body.
% The "chunked" transfer-coding MUST NOT be applied more than once
%  to a message-body.
% These rules allow the recipient to determine the transfer-length
%  of the message.
%
% ## Comparison to MIME `Content-Transfer-Encoding`
%
% Transfer-codings are analogous to the `Content-Transfer-Encoding` values
%  of MIME, which were designed to enable safe transport of binary data
%  over a 7-bit transport service.
% However, safe transport has a different focus for
%  an 8bit-clean transfer protocol.
% In HTTP, the only unsafe characteristic of message-bodies
%  is the difficulty in determining the exact body length,
%  or the desire to encrypt data over a shared transport.
%
% ## IANA
%
% The Internet Assigned Numbers Authority (IANA) acts as a registry for
%  transfer-coding value tokens.
% Initially, the registry contains the following tokens:
%   * `chunked`
%   * `compress`
%   * `deflate`
%   * `gzip`
%   * `identity`
%
% New transfer-coding value tokens SHOULD be registered in the same way
%  as new content-coding value tokens.
%
% ## Unimplemented transfer-codings
%
% A server which receives an entity-body with a transfer-coding
%  it does not understand SHOULD return 501 (Unimplemented),
%  and close the connection.
%
% ## Backward compatibility
%
% A server MUST NOT send transfer-codings to an HTTP/1.0 client.
%
% --
%
% @see RFC 2616

'transfer-coding'('transfer-coding'(chunked), chunked, []) -->
  "chunked".
'transfer-coding'('transfer-coding'(T0), Token, Params) -->
  'transfer-extension'(T0, Token, Params).

%! 'transfer-extension'(
%!   -ParseTree:compound,
%!   ?Token:atom,
%!   ?Parameters:list(kvpair)
%! )//
% ~~~{.abnf}
% transfer-extension = token *( ";" parameter )
% ~~~

'transfer-extension'('transfer-extension'([T0|Ts]), Token, Params) -->
  token(T0, Token),
  dcg_multi2(';_and_parameter', _-_, Ts, Params).
';_and_parameter'(Param) -->
  ";",
  parameter(Param).

%! parameter(-ParseTree:compound, ?AttributeValuePair:kvpair(atom,atom))//
% Parameters are in the form of attribute/value pairs.
% ~~~{.abnf}
% parameter = attribute "=" value
% ~~~

parameter(paramter(T1,T2), Attr=Value) -->
  attribute(T1, Attr),
  "=",
  value(T2, Val).

%! attribute(-ParseTree:compound, ?Attribute:atom)//
% ~~~{.abnf}
% attribute = token
% ~~~

attribute(attribute(T0), Attr) -->
  token(T0, Attr).

%! value(-ParseTree:compound, ?Value:atom)//
% ~~~{.abnf}
% value     = token | quoted-string
% ~~~

value(value(T0), Val) -->
  token(T0, Val).
value(value(T0), Val) -->
  'quoted-string'(T0, Val).

