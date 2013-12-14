:- module(
  http_dcg,
  [
    http_to_gv/1, % +Tree:compound
    'Request'//6, % -Tree:compound
                  % ?Method:atom
                  % ?URI:compound
                  % ?Version:compound
                  % ?MessageHeaders:list(pair)
                  % ?MessageBody:list(code)
    'Response'//5 % -Tree:compound
                  % ?Version:compound
                  % ?Status:compound
                  % ?MessageHeaders:list(pair)
                  % ?MessageBody:list(code)
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

# ABNF reuse

## CRLF

HTTP/1.1 defines the sequence carriage_return//1 line_feed//1
as the end-of-line marker for all protocol elements except
the entity-body (see appendix 19.3 for tolerant applications).
The end-of-line marker within an entity-body is defined by its
associated media type, as described in section 3.7.

# Concepts

  * **Age**
    The age of a response is the time since it was sent by, or
    successfully validated with, the origin server.
  * **Entity**
    The payload of the message.
  * **Expiration date**
    * **Explicit expiration time**
      The time at which the origin server intends that an entity should
      no longer be returned by a cache without further validation.
    * **Heuristic expiration time**
      An expiration time assigned by a cache when no explicit expiration
      time is available.
  * **First-hand**
    A response is first-hand if it comes directly and without
    unnecessary delay from the origin server, perhaps via one or more
    proxies, or if its validity has just been checked directly with
    the origin server.
  * **Fresh**
    The property of a response for which `age < freshness lifetime`.
  * **Freshness lifetime**
    The length of time between the generation of a response and its
    expiration time.
  * **Inbound/outbound**
    Traveling toward the origin server. / Traveling toward the user agent.
  * **Intermediary**
    * **Gateway**
      A receiving agent.
      Acting as if it were the origin server for the requested resource.
    * **Proxy**
      A forwarding agent, making requests on behalf of other clients.
      * **Transparent proxy**
        A proxy that does not modify the request or response beyond
        what is required for proxy authentication and identification.
    * **Tunnel**
      An intermediary program which is acting as a blind relay between
      two connections.
      A tunnel is not considered a party to the HTTP communication.
  * **Representation**
    An entity included with a response that is subject to content negotiation.
  * **Resource**
    A network data object or service that can be identified by a URI.
  * **Stale**
    The property of a response for which `age >= freshness lifetime`.
  * **Upstream/downstream**
    All messages flow from upstream to downstream.
  * **Validator**
    A protocol element (e.g., an entity tag or a Last-Modified time)
    that is used to find out whether a cache entry is an equivalent
    copy of an entity.
  * **Variant**
    A resource may have one, or more than one, representation(s)
    associated with it at any given instant.
    Use of the term 'variant' does not necessarily imply that the resource
    is subject to content negotiation.

# Client request

Components:
  * Method
  * URI
  * Protocol version
  * MIME-like message
    * Request modifiers
    * Client information
    * Body content

# Server response

Components:
  * Protocol version
  * Success or error code
  * MIME-like message
    * Server information
    * Entity meta-information
    * Entity-body content

# Caching

All non-tunnel parties (i.e., client, server, gateway, proxy)
may employ an internal cache.

# On implementing BNFs as DCGs

In BNFs there are (at least) two ways for writing an optional component.
  1. Using quare brackets.
  2. Using counters.

We give a simple example of the first variant in [1] and a simple example of
the second variant in [2].

~~~{.abnf}
[1] a = b [c]
~~~

~~~{.abnf}
[2] a = b *c
~~~

The question is: how should the parse tree be drawn in these two cases?
The first intuition I had was that in the case of [1] the parse tree for `a`
might exclude a parse tree for `c`, wehereas in the case of [2] the parse tree
for `a` always includes a parse tree for `c` (possibly with void content).

Sometimes a mix of the two variants occurs. Example [3] comes from
the HTTP 1.1 standard.

~~~{.abnf}
[3] message-header = field-name ":" [ field-value ]
    field-value = *( field-content | LWS )
~~~

Example [3] invalidates my former intuition, since a message header with no
field value would have two parse trees (ambiguity in the grammar).

From this I conclude that the BNF notation should not always be directly
reflected in the way the parse tree is constructed.

BNFs are very much directed towards their end result (producing all and only
conforming strings), and not so much to the way in which the end result is
constructed. (Decision procedure versus structural analysis.)

@author Wouter Beek
@see Based on RFC 2616, http://tools.ietf.org/html/rfc2616
@version 2013/07, 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_control)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(gv(gv_file)).
:- use_module(http(http_abnf)).
:- use_module(http(http_basic)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).
:- use_module(math(radix)).
:- use_module(uri(rfc2396_dcg)).

:- meta_predicate(chunk(-,?,//,?,?,?)).
:- meta_predicate('chunk-data'(-,?,//,?,?)).

:- setting(default_port, integer, 80, 'The default TCP port.').



allow(allow('Allow',':',T1), Methods) -->
  "Allow",
  colon,
  methods(T1, Methods).

%! attribute(-Tree:compound, ?Attribute:atom)//

attribute(attribute(T), Attribute) -->
  token(T, Attribute).

%! bytes_unit(-Tree:compound, ?RangeUnit:atom)//

bytes_unit(bytes_unit(bytes), bytes) --> "bytes".

%! charset(-Tree:compound, ?Charset:atom)//
% A **character set** is a method used with one or more tables
% to convert a sequence of octets into a sequence of characters.
%
% Unconditional conversion in the other direction is not required,
% in that not all characters may be available in a given character set
% and a character set may provide more than one sequence of octets
% to represent a particular character.
%
% The definition associated with a MIME character set name MUST
% fully specify the mapping to be performed from octets to characters.
% In particular, use of external profiling information to determine
% the exact mapping is not permitted.
%
% Note: This use of the term 'character set' is more commonly referred to
% as a 'character encoding'. However, since HTTP and MIME share
% the same registry, it is important that the terminology also be shared.
%
% HTTP character sets are identified by case-insensitive tokens. The
% complete set of tokens is defined by the IANA Character Set registry.
%
% @tbd Process tokens in a case-insensitive way.
% @tbd Compliance with MIME types.
% @tbd Compliance with IANA Character Set registry.
% @tbd Implementors should be aware of IETF character set requirements.

charset(charset(T), Charset) -->
  token(T, Charset).

%! chunk(
%!   -Tree:compound,
%!   ?ChunkSize:integer,
%!   ?ChunkData:list(code),
%!   ?ChunkExtension:list
%! )//

chunk(T0, ChunkSize, ChunkData, ChunkExtension) -->
  'chunk-size'(T1, ChunkSize),
  (
    "", {ChunkExtension = []}
  ;
    'chunk-extension'(T2, ChunkExtension)
  ),
  'CRLF'(T3, _),
  'chunk-data'(T4, ChunkSize, ChunkData),
  'CRLF'(T5, _),
  {parse_tree(chunk, [T1,T2,T3,T4,T5], T0)}.

%! 'chunked-body'(-
%!   Tree:compound,
%!   ?Chunks:list(compound),
%!   ?LastChunkExtension:list,
%!   ?EntityHeaders:list
%! )//
% The chunked encoding modifies the body of a message in order to
% transfer it as a series of chunks, each with its own size indicator,
% followed by an OPTIONAL trailer containing entity-header fields. This
% allows dynamically produced content to be transferred along with the
% information necessary for the recipient to verify that it has
% received the full message.
%
% ~~~{.abnf}
% Chunked-Body = *chunk last-chunk trailer CRLF
% chunk = chunk-size [ chunk-extension ] CRLF chunk-data CRLF
% ~~~
%
% The chunk-size field is a string of hex digits indicating the size of
% the chunk. The chunked encoding is ended by any chunk whose size is
% zero, followed by the trailer, which is terminated by an empty line.
%
% The trailer allows the sender to include additional HTTP header
% fields at the end of the message. The Trailer header field can be
% used to indicate which header fields are included in a trailer.
%
% @param Tree A parse tree.
% @param Chunks A list of compound terms of the form
%      `chunk(ChunkSize:integer,ChunkData:list(code),ChunkExtension:list)`.
% @param LastChunkExtension A list of name-value pairs.
% @param EntityHeaders A list of ???.

'chunked-body'(T0, Chunks, LastChunkExtension, EntityHeaders) -->
  % A non-negative number of chunks.
  chunks(T1, Chunks),
  % The last chunk.
  'last-chunk'(T2, LastChunkExtension),
  % The trailer of the chunk body.
  ("" ; trailer(T3, EntityHeaders)),
  'CRLF'(T4, _),
  {parse_tree('chunked-body', [T1,T2,T3,T4], T0)}.

%! 'chunk-data'(-Tree:compound, ?ChunkSize:integer, :ChunkData:list(code))//
% ~~~{.abnf}
% chunk-data = chunk-size(OCTET)
% ~~~

'chunk-data'('chunk-data'(ChunkData), ChunkSize, ChunkData) -->
  {length(ChunkData, ChunkSize)},
  ChunkData.

%! 'chunk-extension'(-Tree:compound, ?ChunkExtension:list)//
% ~~~{.abnf}
% chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% chunk-ext-name = token
% chunk-ext-val = token | quoted-string
% ~~~
%
% Notice that we choose to hide the parsing of the chunk extensions here
% (included as a list).

'chunk-extension'('chunk-extension'(L), L) -->
  'chunk-extension'(L).
'chunk-extension'([H|T]) -->
  ";",
  attribute(_T1, Attribute),
  (
    "",
    {H = Attribute}
    %{Tree = 'chunk-extension'(';',T1,Tree2)}
  ;
    "=",
    value(_T2, Value),
    {H = Attribute-Value}
    %{Tree1 = 'chunk-extension'(';',T1,'=',T2,Tree2)}
  ),
  'chunk-extension'(T).

%! 'chunk-size'(-Tree:compound, ?ChunkSize:integer)//
% ~~~{.abnf}
% chunk-size = 1*HEX
% ~~~
%
% @param Tree A parse tree.
% @param ChunkSize A decimal number.

'chunk-size'('chunk-size'(ChunkSize), ChunkSize) -->
  hexadecimal_number(ChunkSize).

%! chunks(-Tree:compound, ?Chunks:list(compound))//

chunks(chunks(L), L) -->
  chunks(L).
chunks([chunk(ChunkSize,ChunkData,ChunkExtension)|T]) -->
  chunk(_T1, ChunkSize, ChunkData, ChunkExtension),
  chunks(T).
chunks([]) --> [].

%! content_codings(-Tree:compound, ?ContentEncoding:atom)//
% Content coding values indicate an encoding transformation that has
% been or can be applied to an entity. Content codings are primarily
% used to allow a document to be compressed or otherwise usefully
% transformed without losing the identity of its underlying media type
% and without loss of information. Frequently, the entity is stored in
% coded form, transmitted directly, and only decoded by the recipient.
%
% ~~~{.abnf}
% content-coding = token
% ~~~
%
% All content-coding values are case-insensitive and registered by IANA.
%
% Although the value describes the content-coding, what is more important
% is that it indicates what decoding mechanism will be required to remove
% the encoding.
%
% @tbd Add support for the initial content encodings
%      (i.e., `compress`, `deflate`, `identity`, `gzip`).
% @tbd Check whether IANA has registered additional content encodings.

content_codings(content_codings(T), ContentEncoding) -->
  token(T, ContentEncoding).

%! delta_seconds(-Tree:compound, ?Seconds:integer)//
% Some HTTP header fields allow a time value to be specified as an
% integer number of seconds, represented in decimal, after the time
% that the message was received.
%
% ~~~{.abnf}
% delta-seconds = 1*DIGIT
% ~~~

delta_seconds(delta_seconds(Seconds), Seconds) -->
  decimal_number(Seconds).

%! encoded_entity_body(-Tree:compound, EncodedEntityBody:list(code))//

encoded_entity_body(
  encoded_entity_body(EncodedEntityBody),
  EncodedEntityBody
) -->
  dcg_all(EncodedEntityBody).

%! entity_body(-Tree:compound, ?EntityBody:list(code))//
% The entity-body (if any) sent with an HTTP request or response is in
% a format and encoding defined by the entity-header fields.
%
% ~~~{.abnf}
% entity-body = *OCTET
% ~~~
%
% The datatype of the entity_body//2 is determined by the header fields
% `Content-Type` and `Content-Encoding`. They defined the two-layer, ordered
% encoding model [1].
%
% ~~~
% [1] entity-body := Content-Encoding(Content-Type(data))
% ~~~

entity_body(entity_body(EntityBody), EntityBody) -->
  dcg_all(EntityBody).

%! 'entity-header'(-Tree:compound, ?EntityHeader:pair)//
% ~~~{.abnf}
% entity-header = Allow
%               | Content-Encoding
%               | Content-Language
%               | Content-Length
%               | Content-Location
%               | Content-MD5
%               | Content-Range
%               | Content-Type
%               | Expires
%               | Last-Modified
%               | extension-header
% ~~~

'entity-header'('entity-header'(T1), allow=Methods) --> allow(T1, Methods).
/*
'entity-header' --> content_encoding.
'entity-header' --> content_language.
'entity-header' --> content_length.
'entity-header' --> content_location.
'entity-header' --> content_md5.
'entity-header' --> content_range.
'entity-header' --> content_type.
'entity-header' --> expires.
'entity-header' --> last_modified.
'entity-header'('entity-header'(T1), ExtensionHeader) -->
  extension_header(T1, ExtensionHeader).
*/

%! entity_tag(-Tree:compound, ?Weak:boolean, ?EntityTag:atom)//
% Entity tags are used for comparing two or more entities from the same
% requested resource.

entity_tag(T, Weak, EntityTag) -->
  (
    "", {Weak = false, T = entity_tag(T2)}
  ;
    weak(T1, Weak), {T = entity_tag(T1,T2)}
  ),
  opaque_tag(T2, EntityTag).

%! extension_code(-Tree:compound, ?Status:integer)//

extension_code(extension_code(Status)) -->
  {nonvar(Status)}, !,
  {\+ 'Status-Code'(Status, _Name)},
  {atom_chars(Status, [D1,D2,D3])},
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3).
extension_code(extension_code(Status)) -->
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3),
  {digits_to_decimal([D1,D2,D3], Status)},
  {\+ 'Status-Code'(Status, _Name)}.

%! extension_header(-Tree:compound, ?ExtensionHeader:pair)//
% ~~~{.abnf}
% extension-header = message-header
% ~~~

extension_header(extension_header(T1), Header) -->
  message_header(T1, Header).

%! 'extension-method'(-Tree:compound, ?ExtensionMethod:atom)//
% ~~~{.abnf}
% extension-method = token
% ~~~

'extension-method'('extension-method'(T0), ExtensionMethod) -->
  token(T0, ExtensionMethod).

%! field_content(-Tree:compound, ?FieldContent:list(atom))//
% The field_content//2 does not include any leading or trailing
% linear_white_space//1: linear white space occurring before the first
% non-whitespace character of the field_value//2 or after the last
% non-whitespace character of the field_value//2.
% Such leading or trailing linear_white_space//1 MAY be
% removed without changing the semantics of the field value.
% Any linear_white_space//1 that occurs between field_content//2
% MAY be replaced with a single space//0, before interpreting
% the field value or forwarding the message downstream.
%
% ~~~{.abnf}
% field-content  = <the OCTETs making up the field-value
%                   and consisting of either *TEXT or combinations
%                   of token, separators, and quoted-string>
% ~~~
%
% We assume that in the BNF definition the set of combinations is intended to
% exclude the empty combination.
% We also assume that a separator cannot occur at the beginning or at the end
% of field content.
%
% Note that the specification allows field_content//2 to be `*TEXT*`.
% An infinite number of empty TEXTs occur at every spot
% in every field_value/2.
% We solve this by using `+TEXT` instead.

field_content(field_content(T), [H]) --> texts(T, H).
field_content(field_content(T1,T2), [H|T]) -->
  (token(T1, H) ; 'quoted-string'(T1, H)),
  field_content_(T2, T).
field_content_(field_content(T1,T2), [H|T]) -->
  (token(T1, H) ; separator(_) ; 'quoted-string'(T1, H)),
  field_content_(field_content(T2), T).
field_content_(field_content(T), [H]) -->
  (token(T, H) ; 'quoted-string'(T, H)).

%! field_name(-Tree:compound, FieldName:atom)//
% ~~~{.abnf}
% field-name = token
% ~~~

field_name(field_name(T), FieldName) --> token(T, FieldName).

%! field_value(-Tree:compound, ?FieldValue:atom)//
% ~~~{.abnf}
% field-value = *( field-content | LWS )
% ~~~
%
% Note that linear_white_space// may be part of field_content//2.
% This is a mistake in the specification.
% We solve this by prioritizing the rules with LWS over those with FC.

field_value(field_value(T), L) -->
  field_value_(T, L).
field_value_(TL, L) -->
  linear_white_space,
  field_value_(TL, L).
field_value_([TH|TT], [H|T]) -->
  field_content(TH, H),
  field_value_(TT, T).
field_value_([], []) --> [].

%! general_header//
% There are a few header fields which have general applicability for
% both request and response messages, but which do not apply to the
% entity being transferred. These header fields apply only to the
% message being transmitted.
%
% General-header field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields may be given the semantics of general
% header fields if all parties in the communication recognize them to
% be general-header fields. Unrecognized header fields are treated as
% entity-header fields.
/*
general_header --> cache_control.
general_header --> connection.
general_header --> date.
general_header --> pragma.
general_header --> trailer.
general_header --> transfer_encoding.
general_header --> upgrade.
general_header --> via.
general_header --> warning.
*/
%! generic_message(-Tree:compound, ?Headers:list(pair), ?Body:list(code))//
% ~~~{.abnf}
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ~~~

generic_message(T, MessageHeaders, MessageBody) -->
  start_line(T1),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  'CRLF'(T3, _),
  ('message-body'(T4, MessageBody) ; "", {MessageBody = []}),
  {parse_tree(generic_message, [T1,T2,T3,T4], T)}.

%! 'HTTP-message'(-Tree:compound)//
% ~~~{.abnf}
% HTTP-message = Request | Response ; HTTP/1.1 messages
% ~~~

'HTTP-message'('HTTP-message'(T)) -->
  'Request'(T, _Method, _URI, _Version, _MessageHeaders, _MessageBody).
'HTTP-message'('HTTP-message'(T)) -->
  'Response'(T, _Version, _Status, _MessageHeaders, _MessaageBody).

http_to_gv(Tree):-
  absolute_file_name(project(temp), File, [access(write),file_type(jpeg)]),
  tree_to_gv_file(
    [method(dot),name('HTTP message'),to_file_type(jpeg)],
    Tree,
    File
  ).

%! http_URL(
%!   -Tree:compound,
%!   ?Host:list(atomic),
%!   ?Port:integer,
%!   ?Path:list(list(atom)),
%!   ?Query:atom
%! )//
% The "http" scheme is used to locate network resources via the HTTP protocol.
%
% ~~~{.abnf}
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ~~~
%
% The use of IP addresses in URLs SHOULD be avoided whenever possible
% (see RFC 1900).
%
% @param Tree A parse tree.
% @param Host
% @param Port An integer representing a port.
%        If the port is empty or not given, port 80 is assumed.
% @param Path
% @param Query
%
% @tbd If the =abs_path= is not present in the URL,
%      it MUST be given as "/" when used as a Request-URI for a resource.

http_url(T0, Host, Port, Path, Query) -->
  % Schema prefix.
  "http://",

  % Host.
  rfc2396_host(T1, Host),

  % Optional port.
  (
    "", {var(Port), setting(default_port, Port)}
  ;
    ":'", rfc2396_port(T2, Port)
  ),

  % Optional absolute path + query.
  (
    "", {var(Path), var(Query)}
  ;
    % Absolute path.
    rfc2396_absolute_path(T3, Path),

    % Optional query.
    (
      "", {var(Query)}
    ;
      "?", rfc2396_query(T4, Query)
    )
  ),
  {parse_tree(http_url, [T1,T2,T3,T4], T0)}.

%! 'HTTP-Version'(-Tree:compound, ?Major:integer, ?Minor:integer) is det.
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.abnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~

'HTTP-Version'('HTTP-Version'(major(Major),minor(Minor)), Major, Minor) -->
  "HTTP/",
  decimal_number(Major),
  ".",
  decimal_number(Minor).

%! 'last-chunk'(-Tree:compound, ?LastChunkExtension:list)//
% The last chunk in a 'chunked-body'//3.
%
% ~~~{.abnf}
% last-chunk = 1*("0") [ chunk-extension ] CRLF
% ~~~
%
% Notice that 'chunk-extension'//2 could be left out completely,
% i.e., no empty list either.

'last-chunk'(T0, ChunkExtension) -->
  % A positive number of zeros.
  dcg_multi(zero, 1-_),
  % Optional chunk extension (attribute-value pairs).
  (
    "",
    {var(ChunkExtension)},
    {T0 = 'last-chunk'(T2)}
  ;
    'chunk-extension'(T1, ChunkExtension),
    {T0 = 'last-chunk'(T1,T2)}
  ),
  'CRLF'(T2, _).

%! linear_white_space//

linear_white_space -->
  linear_white_space(_Codes).

%! linear_white_space(?Codes:list(code))//
% HTTP/1.1 header field values can be folded onto multiple lines if the
% continuation line begins with space//1 or horizontal_tab//1.
% All linear white space, including folding, has the same semantics as
% space//1.
% A recipient MAY replace any linear_white_space//1 with a single space//1
% before interpreting the field value or forwarding the message downstream.

linear_white_space(L) -->
  ("", {L = T} ; 'CRLF'([H1,H2], _), {L = [H1,H2|T]}),
  linear_white_space_(T).

% Process less white space first, i.e., non-greedy, to accomodate
% a sparse use of white space in generation.
linear_white_space_([H]) -->
  (space(H) ; horizontal_tab(H)).
linear_white_space_([H|T]) -->
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).

%! media_type(-Tree:compound, ?Type:atom, ?Subtype:atom, ?Parameters:list)//
% The type//2, subtype//2, and parameter//2 attribute names are case-
% insensitive. Parameter values might or might not be case-sensitive,
% depending on the semantics of the parameter name. linear_white_space//1
% MUST NOT be used between the type and subtype, nor between an
% attribute and its value. The presence or absence of a parameter might
% be significant to the processing of a media-type, depending on its
% definition within the media type registry.
%
% ~~~{.abnf}
% media-type = type "/" subtype *( ";" parameter )
% ~~~
%
% @tbd Check the IANA for registered media types.

media_type(media_type(T1,'/',T2,T3), Type, SubType, Parameters) -->
  type(T1, Type),
  forward_slash,
  subtype(T2, SubType),
  parameters(T3, Parameters).

%! 'message-body'(-Tree:compound, ?MessageBody:list(code))//
% he message-body (if any) of an HTTP message is used to carry the
% entity-body associated with the request or response. The message-body
% differs from the entity-body only when a transfer-coding has been
% applied, as indicated by the `Transfer-Encoding` header field.
%
% `Transfer-Encoding` is a property of the message, not of the
% entity, and thus MAY be added or removed by any application along the
% request/response chain.
%
% ~~~{.abnf}
% message-body = entity-body
%              | <entity-body encoded as per Transfer-Encoding>
% ~~~
%
% ## Requests
%
% The presence of a message-body in a request is signaled by the
% inclusion of a `Content-Length` or `Transfer-Encoding` header field in
% the request's message-headers.
% A message-body MUST NOT be included in a request if the specification
% of the request method does not allow so.
%
% ## Responses
%
% For response messages, whether or not a message-body is included with
% a message is dependent on both the request method and the response
% status code
%
% All responses to the HEAD request method MUST NOT include a message-body,
% even though the presence of entity-header fields might lead one to believe
% they do.
%
% All 1xx (informational), 204 (no content), and 304 (not modified) responses
% MUST NOT include a 'message-body'//1. All other responses do include a
% 'message-body'//1, although it MAY be of zero length.
%
% ## Message length
%
% The transfer-length of a message is the length of the message-body as
% it appears in the message; that is, after any transfer-codings have
% been applied. When a message-body is included with a message, the
% transfer-length of that body is determined by one of the following
% (in order of precedence):
%    1. Any response message which "MUST NOT" include a message-body (such
%       as the `1xx`, `204`, and `304` responses and any response to a `HEAD`
%       request) is always terminated by the first empty line after the
%       header fields, regardless of the entity-header fields present in
%       the message.
%    2. If a `Transfer-Encoding` header field (section 14.41) is present and
%       has any value other than "identity", then the transfer-length is
%       defined by use of the "chunked" transfer-coding (section 3.6),
%       unless the message is terminated by closing the connection.
%    3. If a `Content-Length` header field (section 14.13) is present, its
%       decimal value in OCTETs represents both the entity-length and the
%       transfer-length. The `Content-Length` header field MUST NOT be sent
%       if these two lengths are different (i.e., if a `Transfer-Encoding`
%       header field is present). If a message is received with both a
%       `Transfer-Encoding` header field and a `Content-Length` header field,
%       the latter MUST be ignored.
%    4. If the message uses the media type `multipart/byteranges`, and the
%       transfer-length is not otherwise specified, then this self-
%       elimiting media type defines the transfer-length. This media type
%       MUST NOT be used unless the sender knows that the recipient can parse
%       it; the presence in a request of a `Range` header with multiple byte-
%       range specifiers from a 1.1 client implies that the client can parse
%       `multipart/byteranges` responses.
%       A range header might be forwarded by a 1.0 proxy that does not
%       understand `multipart/byteranges`; in this case the server MUST
%       delimit the message using methods defined in items 1,3 or 5 of
%       this section.
%    5. By the server closing the connection. (Closing the connection
%       cannot be used to indicate the end of a request body, since that
%       would leave no possibility for the server to send back a response.)
%
% For compatibility with HTTP/1.0 applications, HTTP/1.1 requests
% containing a message-body MUST include a valid Content-Length header
% field unless the server is known to be HTTP/1.1 compliant. If a
% request contains a message-body and a Content-Length is not given,
% the server SHOULD respond with 400 (bad request) if it cannot
% determine the length of the message, or with 411 (length required) if
% it wishes to insist on receiving a valid Content-Length.
%
% All HTTP/1.1 applications that receive entities MUST accept the
% "chunked" transfer-coding (section 3.6), thus allowing this mechanism
% to be used for messages when the message length cannot be determined
% in advance.
%
% Messages MUST NOT include both a Content-Length header field and a
% non-identity transfer-coding. If the message does include a non-
% identity transfer-coding, the Content-Length MUST be ignored.
%
% When a Content-Length is given in a message where a message-body is
% allowed, its field value MUST exactly match the number of OCTETs in
% the message-body. HTTP/1.1 user agents MUST notify the user when an
% invalid length is received and detected.

'message-body'('message-body'(T0), MessageBody) -->
  entity_body(T0, MessageBody).
'message-body'('message-body'(T0), MessageBody) -->
  encoded_entity_body(T0, MessageBody).

%! message_header(-Tree:compound, ?NVPair)//
% ~~~{.abnf}
% message-header = field-name ":" [ field-value ]
% ~~~

message_header(message_header(T1), Name-Value) -->
  dcg_call(Name, T1, Value).

/* THE  GENERIC MESSAGE HEADER GRAMMAR
  field_name(T1, FieldName),
  colon,
  (field_value(T2, FieldValue) ; "", {FieldValue = []}),
  {parse_tree(message_header, [T1,':',T2], T)}.
*/

%! message_heades(-Tree:compound, ?MessageHeaders:list)//
% The order in which header fields with differing field names are
% received is not significant. However, it is "good practice" to send
% general-header fields first, followed by request-header or response-
% header fields, and ending with the entity-header fields.
%
% Multiple message-header fields with the same field-name MAY be
% present in a message if and only if the entire field-value for that
% header field is defined as a comma-separated list [i.e., #(values)].
% It MUST be possible to combine the multiple header fields into one
% "field-name: field-value" pair, without changing the semantics of the
% message, by appending each subsequent field-value to the first, each
% separated by a comma. The order in which header fields with the same
% field-name are received is therefore significant to the
% interpretation of the combined field value, and thus a proxy MUST NOT
% change the order of these field values when a message is forwarded.

message_headers(message_headers(T1,T2), [H]) -->
  message_header(T1, H),
  'CRLF'(T2, _).
message_headers(message_headers(T1,T2,T3), [H|T]) -->
  message_header(T1, H),
  'CRLF'(T2, _),
  message_headers(T3, T).

%! 'Method'(-Tree:compound, ?Method:atom)//
% The method is case-sensitive.
% The list of methods allowed by a resource can be specified in an
% `Allow` header field.
%
% The methods `GET` and `HEAD` MUST be supported by all general-purpose
% servers. All other methods are OPTIONAL.
%
% ## Status codes
%
% Status codes to be used in the response:
%   * `405 (Method Not Allowed)`
%     The method is known by the origin server but not allowed for the
%     requested resource.
%    * `501 (Not Implemented)`
%      The method is unrecognized or not implemented by the origin server.
%
% ~~~{.abnf}
% Method = "OPTIONS" | "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE"
%        | "CONNECT" | extension-method
% ~~~

'Method'(method('CONNECT'), connect) --> "CONNECT".
'Method'(method('DELETE'), delete) --> "DELETE".
'Method'(method('GET'), get) --> "GET".
'Method'(method('HEAD'), head) --> "HEAD".
'Method'(method('OPTIONS'), options) --> "OPTIONS".
'Method'(method('POST'), post) --> "POST".
'Method'(method('PUT'), put) --> "PUT".
'Method'(method('TRACE'), trace) --> "TRACE".
'Method'(method(T), ExtensionMethod) -->
  'extension-method'(T, ExtensionMethod).

methods(methods(T1,',',T2), [H|T]) -->
  'Method'(T1, H),
  ",",
  methods(T2, T).
methods(methods(T1), [H]) -->
  'Method'(T1, H).

%! opaque_tag(-Tree:compound, ?OpaqueTag:atom)//

opaque_tag(opaque_tag(T), OpaqueTag) --> 'quoted-string'(T, OpaqueTag).

%! other_range_unit(-Tree:compound, ?RangeUnit:atom)//

other_range_unit(other_range_unit(T), RangeUnit) --> token(T, RangeUnit).

%! parameter(-Tree:compound, -AttributeValuePair)//

parameter(parameter(T1,'=',T2), Attribute-Value) -->
  attribute(T1, Attribute),
  equals_sign,
  value(T2, Value).

%! parameters(?Parameters:list)//
% Note that we do not make the list structure explicit in the parse tree.

parameters(parameters(L), L) -->
  parameters(L).
parameters([H|T]) -->
  semi_colon,
  parameter(_T, H),
  parameters(T).
parameters([H]) -->
  semi_colon,
  parameter(_T, H).

%! product(-Tree:compound, ?Name:atom, ?Version:atom)//
% Product tokens are used to allow communicating applications to
% identify themselves by software name and version. Most fields using
% product tokens also allow sub-products which form a significant part
% of the application to be listed, separated by white space. By
% convention, the products are listed in order of their significance
% for identifying the application.
%
% ~~~{.abnf}
% product = token ["/" product-version]
% ~~~
%
% Examples:
% ~~~
% User-Agent: CERN-LineMode/2.15 libwww/2.17b3
% Server: Apache/0.8.4
% ~~~
%
% product//3 tokens SHOULD be short and to the point. They MUST NOT be
% used for advertising or other non-essential information. Although any
% token character MAY appear in a 'product-version'//2, this token//2 SHOULD
% only be used for a version identifier (i.e., successive versions of
% the same product SHOULD only differ in the 'product-version'//2 portion of
% product//3).

product(product(name(T1),T2), Name, Version) -->
  token(T1, Name),
  forward_slash,
  'product-version'(T2, Version).
product(product(name(T1)), Name, Version) -->
  {var(Version)},
  token(T1, Name).

%! 'product-version'(-Tree:compound, ?Version:atom)//
% ~~~{.abnf}
% product-version = token
% ~~~

'product-version'('product-version'(T), Version) --> token(T, Version).

%! qvalue(-Tree:compound, QValue:float)//
% HTTP content negotiation uses short floating point numbers to indicate
% the relative importance ('weight') of various  negotiable parameters.
% A weight is normalized to a real number in the range 0 through 1,
% where 0 is the minimum and 1 the maximum value.
% If a parameter has a quality value of 0, then content with
% this parameter is 'not acceptable' for the client.
% HTTP/1.1 applications MUST NOT generate more than three digits after the
% decimal point. User configuration of these values SHOULD also be
% limited in this fashion.
%
% ~~~{.abnf}
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ~~~
%
% 'Quality values' is a misnomer, since these values merely represent
% relative degradation in desired quality.

qvalue(qvalue(QValue), QValue) -->
  "0",
  ".",
  decimal_number(After),
  {
    between(0, 999, After),
    number_parts(QValue, 1, After)
  }.
qvalue(qvalue(1.0), 1.0) -->
  "1",
  ".",
  dcg_multi(zero, 0-3).

%! range_unit(-Tree:compound, ?RangeUnit:atom)//
% HTTP/1.1 allows a client to request that only part (a range of) the
% response entity be included within the response.
% An entity can be broken down into subranges according to various
% structural units.

range_unit(range_unit(T), RangeUnit) --> bytes_unit(T, RangeUnit).
range_unit(range_unit(T), RangeUnit) --> other_range_unit(T, RangeUnit).

%! 'Reason-Phrase'(-Tree:compound, ?ReasonPhrase:atom)//
% The 'Reason-Phrase'//2 is intended to give a short textual description
% of the 'Status-Code'//3.
%
% The 'Status-Code'//3 is intended for use by automata and the 'Reason-Phrase'//2
% is intended for the human user.
%
% The client is not required to examine or display the 'Reason-Phrase'//2.

'Reason-Phrase'('Reason-Phrase'(ReasonPhrase), ReasonPhrase) -->
  {nonvar(ReasonPhrase)}, !,
  {atom_codes(ReasonPhrase, Codes)},
  reason_phrase_(Codes).
'Reason-Phrase'('Reason-Phrase'(ReasonPhrase), ReasonPhrase) -->
  reason_phrase_(Codes),
  {atom_codes(ReasonPhrase, Codes)}.
reason_phrase_([H|T]) -->
  [H],
  {\+ code_type(H, cntrl)},
  {\+ memberchk(H, [13,10])},
  reason_phrase_(T).
reason_phrase_([]) --> [].

%! 'Request'(
%!   -Tree:compound,
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
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

'Request'(T0, Method, URI, Version, MessageHeaders, MessageBody) -->
  'Request-Line'(T1, Method, URI, Version),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  'CRLF'(T3, _),
  ('message-body'(T4, MessageBody) ; ""),
  {parse_tree(request, [T1,T2,T3,T4], T0)}.

%! 'request-header'//
% Request-header field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields MAY be given the semantics of request-
% header fields if all parties in the communication recognize them to
% be request-header fields. Unrecognized header fields are treated as
% entity-header fields.
%
% ~~~{.abnf}
% request-header = Accept
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
/*
'request-header' --> accept.
'request-header' --> accept_charset.
'request-header' --> accept_encoding.
'request-header' --> accept_language.
'request-header' --> authorization.
'request-header' --> expect.
'request-header' --> from.
'request-header' --> host.
'request-header' --> if_match.
'request-header' --> if_modified_since.
'request-header' --> if_none_match.
'request-header' --> if_range.
'request-header' --> if_unmodified_since.
'request-header' --> max_forwards.
'request-header' --> proxy_authorization.
'request-header' --> range.
'request-header' --> referer.
'request-header' --> te.
'request-header' --> user_agent.
*/
%! 'Request-Line'(
%!   -Tree:compound,
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound
%! )//
% The first line of an HTTP request.
%
% ~~~{.abnf}
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
% ~~~

'Request-Line'(
  'Request-Line'(T1,T2,T3,T4,T5,T6),
  Method,
  uri(Scheme, Authority, Path, Query),
  version(Major, Minor)
) -->
  'Method'(T1, Method),
  space, {T2 = sp},
  'Request-URI'(T3, Scheme, Authority, Path, Query),
  space, {T4 = sp},
  'HTTP-Version'(T5, Major, Minor),
  'CRLF'(T6, _).

%! 'Request-URI'(
%!   -Tree:compound,
%!   ?Scheme:atom,
%!   ?Authority:compound,
%!   ?Path:list(list(atom)),
%!   ?Query:atom
%! )//
% Identifies the resource upon which to apply the request.
%
% ~~~{.abnf}
% Request-URI = "*" | absoluteURI | abs_path | authority
% ~~~
%
% The request URI variant that is used depends on the nature of the request.
%
% ## Asterisk
%
% The request does not apply to a particular resource, but to the server
% itself.
%
% This is only allowed when the method used does not necessarily apply
% to a resource.
%
% Example:
% ~~~{.http}
% OPTIONS * HTTP/1.1
% ~~~
%
% ## Absolute URI
%
% This form is REQUIRED when the request is being made to a proxy.
% The proxy is requested to forward the request or service it
% from a valid cache, and return the response. Note that the proxy MAY
% forward the request on to another proxy or directly to the server
% specified by the absolute_uri//5.
% In order to avoid request loops, a proxy MUST be able to recognize
% all of its server names, including any aliases, local variations,
% and the numeric IP address.
%
% Example:
% ~~~{.http}
% GET http://www.w3.org/pub/WWW/TheProject.html HTTP/1.1
% ~~~
%
% To allow for transition to the use of absolute_uri//5 in all requests
% in future versions of HTTP, all HTTP/1.1 servers MUST it in requests,
% even though HTTP/1.1 clients will only generate them in requests to proxies.
%
% ## Authority
%
% The authority form is only used by the `CONNECT` method.
%
% ## Absolute path
%
% The most common variant of 'Request-URI'//5 is that used to identify a
% resource on an origin server or gateway. In this case the absolute_path//2
% of the URI MUST be transmitted, and the network location of the URI
% (authority) MUST be transmitted in a `Host` header field.
%
% Example:
%
% ~~~{.http}
% GET /pub/WWW/TheProject.html HTTP/1.1
% Host: www.w3.org
% ~~~
%
% The rfc2396_absolute_path//2 cannot be empty. If none is present it MUST be
% given as `/` (i.e., the server root).

'Request-URI'('Request-URI'('*'), _Scheme, _Authority, _Path, _Query) -->
  "*".
'Request-URI'('Request-URI'(T1), Scheme, Authority, Path, Query) -->
  rfc2396_absolute_uri(T1, Scheme, Authority, Path, Query).
'Request-URI'('Request-URI'(T1), _Scheme, _Authority, Path, _Query) -->
  rfc2396_absolute_path(T1, Path).
'Request-URI'('Request-URI'(T1), _Scheme, Authority, _Path, _Query) -->
  rfc2396_authority(T1, Authority).

%! 'Response'(
%!   -Tree:compound,
%!   ?Version:compound,
%!   ?Status:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% After receiving and interpreting a request message, a server responds
% with an HTTP response message.
%
% ~~~{.abnf}
% Response = Status-Line
%            *(( general-header | response-header | entity-header ) CRLF)
%            CRLF
%            [ message-body ]
% ~~~

'Response'(T0, Version, Status, MessageHeaders, MessageBody) -->
  'Status-Line'(T1, Version, Status),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  'CRLF'(T3, _),
  ('message-body'(T4, MessageBody) ; "", {MessageBody = []}),
  {parse_tree(response, [T1,T2,T3,T4], T0)}.

%! 'response-header'//
% ~~~{.abnf}
% response-header = Accept-Ranges
%                 | Age
%                 | ETag
%                 | Location
%                 | Proxy-Authenticate
%                 | Retry-After
%                 | Server
%                 | Vary
%                 | WWW-Authenticate
% ~~~
%
% 'response-header'// field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields MAY be given the semantics of response-
% header fields if all parties in the communication recognize them to
% be 'response-header'// fields. Unrecognized header fields are treated as
% 'entity-header'// fields.
/*
'response-header' --> 'Accept-Ranges'.
'response-header' --> 'Age'.
'response-header' --> etag.
'response-header' --> location.
'response-header' --> proxy_authenticate.
'response-header' --> retry_after.
'response-header' --> server.
'response-header' --> vary.
'response-header' --> www_authenticate.
*/

%! start_line(-Tree:compound)//
% ~~~{.abnf}
% start-line = Request-Line | Status-Line
% ~~~

start_line(start_line(T0)) --> 'Request-Line'(T0, _Method, _URI, _Version).
start_line(start_line(T0)) --> 'Status-Line'(T0, _Version, _Status).

%! 'Status-Code'(?Status:integer, ?Reason:atom) is nondet.
% The first digit of the 'Status-Code'/2 defines the class of response.
% The last two digits do not have any categorization role.
%
% There are 5 values for the first digit:
%   * `1xx`
%     Informational - Request received, continuing process.
%   * `2xx`
%     Success - The action was successfully received, understood,
%     and accepted.
%   * `3xx`
%     Redirection - Further action must be taken in order to complete
%     the request.
%   * `4xx`
%     Client Error - The request contains bad syntax or cannot be fulfilled.
%   * `5xx`
%     Server Error - The server failed to fulfill an apparently valid request.
%
% The individual values of the numeric status codes defined for
% HTTP/1.1, and an example set of corresponding Reason-Phrase's, are
% presented below. The reason phrases listed here are only
% recommendations -- they MAY be replaced by local equivalents without
% affecting the protocol.

'Status-Code'(100, 'Continue').
'Status-Code'(101, 'Switching Protocols').
'Status-Code'(200, 'OK').
'Status-Code'(201, 'Created').
'Status-Code'(202, 'Accepted').
'Status-Code'(203, 'Non-Authoritative Information').
'Status-Code'(204, 'No Content').
'Status-Code'(205, 'Reset Content').
'Status-Code'(206, 'Partial Content').
'Status-Code'(300, 'Multiple Choices').
'Status-Code'(301, 'Moved Permanently').
'Status-Code'(302, 'Found').
'Status-Code'(303, 'See Other').
'Status-Code'(304, 'Not Modified').
'Status-Code'(305, 'Use Proxy').
'Status-Code'(307, 'Temporary Redirect').
'Status-Code'(400, 'Bad Request').
'Status-Code'(401, 'Unauthorized').
'Status-Code'(402, 'Payment Required').
'Status-Code'(403, 'Forbidden').
'Status-Code'(404, 'Not Found').
'Status-Code'(405, 'Method Not Allowed').
'Status-Code'(406, 'Not Acceptable').
'Status-Code'(407, 'Proxy Authentication Required').
'Status-Code'(408, 'Request Timeout').
'Status-Code'(409, 'Conflict').
'Status-Code'(410, 'Gone').
'Status-Code'(411, 'Length Required').
'Status-Code'(412, 'Precondition Failed').
'Status-Code'(413, 'Request Entity Too Large').
'Status-Code'(414, 'Request URI Too Large').
'Status-Code'(415, 'Unsupported Media Type').
'Status-Code'(416, 'Requested Range not Satisfiable').
'Status-Code'(417, 'Expectation Failed').
'Status-Code'(500, 'Internal Server Error').
'Status-Code'(501, 'Not Implemented').
'Status-Code'(502, 'Bad Gateway').
'Status-Code'(503, 'Service Unavailable').
'Status-Code'(504, 'Gateway Timeout').
'Status-Code'(505, 'HTTP Version not supported').

%! 'Status-Code'(-Tree:compound, ?Status:integer, ?Reason:atom)//
% A 3-digit integer result code of the attempt to understand and satisfy
% the request.
%
% HTTP status codes are extensible. HTTP applications are not required
% to understand the meaning of all registered status codes, though such
% understanding is obviously desirable. However, applications MUST
% understand the class of any status code, as indicated by the first
% digit, and treat any unrecognized response as being equivalent to the
% `x00` status code of that class, with the exception that an
% unrecognized response MUST NOT be cached.

'Status-Code'('Status-Code'(Status), Status, Reason1) -->
  {nonvar(Status)}, !,
  {
    'Status-Code'(Status, Reason2),
    decimal_to_digits(Status, [D1,D2,D3])
  },
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3),
  % Use the default reason for the given status code.
  {(var(Reason1) -> Reason1 = Reason2 ; true)}.
'Status-Code'('Status-Code'(Status), Status, _Reason1) -->
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  decimal_digit(_, D3),
  {
    digits_to_decimal([D1,D2,D3], Status),
    'Status-Code'(Status, _Reason2)
  }.
'Status-Code'('Status-Code'(Status), Status, _Reason) -->
  extension_code(Status).

%! 'Status-Line'(-Tree:compound, ?Version:compound, ?Status:compound)//
% The first line of a response// message.

'Status-Line'(
  'Status-Line'(T1,T2,T3,T4,T5,T6),
  version(Major, Minor),
  status(Status, Reason)
) -->
  'HTTP-Version'(T1, Major, Minor),
  space, {T2 = sp},
  'Status-Code'(T3, Status, Reason),
  space, {T4 = sp},
  'Reason-Phrase'(T5, Reason),
  'CRLF'(T6, _).

%! subtype(-Tree:compound, ?SubType:atom)//
% Media subtype.
%
% ~~~{.abnf}
% subtype = token
% ~~~

subtype(subtype(T0), SubType) -->
  token(T0, SubType).

%! texts(-Tree:compound, ?Text:atom)//
% The 'TEXT'//1 rule is only used for descriptive field contents and values
% that are not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
% ISO-8859-1 only when encoded according to the rules of RFC 2047.
%
% ~~~{.abnf}
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~
%
% A 'CRLF'//2 is allowed in the definition of 'TEXT'// only as part of
%   a header field continuation.
% It is expected that the folding 'LWS'// will be replaced
%   with a single 'SP'// before interpretation of the 'TEXT'// value.

texts(texts(Text), Text) -->
  {nonvar(Text)}, !,
  {atom_codes(Text, Codes)},
  texts(Codes).
texts(texts(Text), Text) -->
  texts(Codes),
  {atom_codes(Text, Codes)}, !.
texts([H|T]) -->
  'TEXT'(H),
  texts(T).
texts([H]) -->
  'TEXT'(H).

%! trailer(-Tree:compound, ?EntityHeaders:list)//
% The trailer of a 'chunked-body'//3.
%
% ~~~{.abnf}
% trailer = *(entity-header CRLF)
% ~~~

trailer(trailer(T1,T2), [H]) -->
  'entity-header'(T1, H),
  'CRLF'(T2, _).
trailer(trailer(T1,T2,T3), [H|T]) -->
  'entity-header'(T1, H),
  'CRLF'(T2, _),
  trailer(T3, T).

%! 'transfer-coding'(-Tree:compound, ?Token:atom, ?Parameters:list)//
% Transfer-coding values are used to indicate an encoding
% transformation that has been, can be, or may need to be applied to an
% entity-body in order to ensure safe transport through the network.
% This differs from a content coding in that the transfer-coding is a
% property of the message, not of the original entity.
%
% ~~~{.abnf}
% transfer-coding = "chunked" | transfer-extension
% ~~~
%
% Parameters are in the form of attribute/value pairs.
%
% ~~~{.abnf}
% parameter = attribute "=" value
% attribute = token
% value = token | quoted-string
% ~~~
%
% All transfer-coding values are case-insensitive.

'transfer-coding'('transfer-coding'(chunked), chunked, []) -->
  "chunked".
'transfer-coding'('transfer-coding'(T), Token, Parameters) -->
  'transfer-extension'(T, Token, Parameters).

%! 'transfer-extension'(-Tree:compound, ?Token:atom, ?Parameters:list)//
%
% ~~~{.abnf}
% transfer-extension = token *( ";" parameter )
% ~~~

'transfer-extension'('transfer-extension'(T1,T2), Token, Parameters) -->
  token(T1, Token),
  parameters(T2, Parameters).

%! type(-Tree:compound, ?Type:atom)//
% Media type.
%
% ~~~{.abnf}
% type = token
% ~~~

type(type(T0), Type) -->
  token(T0, Type).

%! value(-Tree:compound, ?Value:atom)//

value(value(T), Value) -->
  token(T, Value).
value(value(T), Value) -->
  'quoted-string'(T, Value).

%! weak(-Tree:compound)//

weak(weak('W/'), true) --> "W/".

