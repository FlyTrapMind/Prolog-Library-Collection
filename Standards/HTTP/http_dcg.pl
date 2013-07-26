:- module(
  http_dcg,
  [
    http_to_gv/1, % +Tree:compound
    request//6, % -Tree:compound
                % ?Method:atom
                % ?URI:compound
                % ?Version:compound
                % ?MessageHeaders:list(pair)
                % ?MessageBody:list(code)
    response//5 % -Tree:compound
                % ?Version:compound
                % ?Status:compound
                % ?MessageHeaders:list(pair)
                % ?MessageBody:list(code)
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

Tag suggested by Anne Ogborn on the SWI-Prolog mailing list: `doc-needs-help`.

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

~~~{.bnf}
[1] a = b [c]
~~~

~~~{.bnf}
[2] a = b *c
~~~

The question is: how should the parse tree be drawn in these two cases?
The first intuition I had was that in the case of [1] the parse tree for `a`
might exclude a parse tree for `c`, wehereas in the case of [2] the parse tree
for `a` always includes a parse tree for `c` (possibly with void content).

Sometimes a mix of the two variants occurs. Example [3] comes from
the HTTP 1.1 standard.

~~~{.bnf}
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
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(gv(gv_file)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).
:- use_module(math(radix)).
:- use_module(datetime(rfc1123)).
:- use_module(lang(rfc1766)).
:- use_module(uri(rfc2396)).

:- meta_predicate(chunk(-,?,//,?,?,?)).
:- meta_predicate(chunk_data(-,?,//,?,?)).

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
  chunk_size(T1, ChunkSize),
  (
    "", {ChunkExtension = []}
  ;
    chunk_extension(T2, ChunkExtension)
  ),
  crlf(T3),
  chunk_data(T4, ChunkSize, ChunkData),
  crlf(T5),
  {parse_tree(chunk, [T1,T2,T3,T4,T5], T0)}.

%! chunked_body(-
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
% Chunked-Body = *chunk last-chunk trailer CRLF
% chunk = chunk-size [ chunk-extension ] CRLF chunk-data CRLF
%
% The chunk-size field is a string of hex digits indicating the size of
% the chunk. The chunked encoding is ended by any chunk whose size is
% zero, followed by the trailer, which is terminated by an empty line.
%
% The trailer allows the sender to include additional HTTP header
% fields at the end of the message. The Trailer header field can be
% used to indicate which header fields are included in a trailer.
%
% @arg Tree A parse tree.
% @arg Chunks A list of compound terms of the form
%      `chunk(ChunkSize:integer,ChunkData:list(code),ChunkExtension:list)`.
% @arg LastChunkExtension A list of name-value pairs.
% @arg EntityHeaders A list of ???.

chunked_body(T0, Chunks, LastChunkExtension, EntityHeaders) -->
  % A non-negative number of chunks.
  chunks(T1, Chunks),
  % The last chunk.
  last_chunk(T2, LastChunkExtension),
  % The trailer of the chunk body.
  ("" ; trailer(T3, EntityHeaders)),
  crlf(T4),
  {parse_tree(chunked_body, [T1,T2,T3,T4], T0)}.

%! chunk_data(-Tree:compound, ?ChunkSize:integer, :ChunkData:list(code))//
% ~~~{.bnf}
% chunk-data = chunk-size(OCTET)
% ~~~

chunk_data(chunk_data(ChunkData), ChunkSize, ChunkData) -->
  {length(ChunkData, ChunkSize)},
  ChunkData.

%! chunk_extension(-Tree:compound, ?ChunkExtension:list)//
% ~~~{.bnf}
% chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% chunk-ext-name = token
% chunk-ext-val = token | quoted-string
% ~~~
%
% Notice that we choose to hide the parsing of the chunk extensions here
% (included as a list).

chunk_extension(chunk_extension(L), L) -->
  chunk_extension(L).
chunk_extension([H|T]) -->
  semi_colon,
  attribute(_T1, Attribute),
  (
    "",
    {H = Attribute}
    %{Tree = chunk_extension(';',T1,Tree2)}
  ;
    equals_sign,
    value(_T2, Value),
    {H = Attribute-Value}
    %{Tree1 = chunk_extension(';',T1,'=',T2,Tree2)}
  ),
  chunk_extension(T).

%! chunk_size(-Tree:compound, ?ChunkSize:integer)//
% ~~~{.bnf}
% chunk-size = 1*HEX
% ~~~
%
% @arg Tree A parse tree.
% @arg ChunkSize A decimal number.

chunk_size(chunk_size(ChunkSize), ChunkSize) -->
  hexadecimal_number(ChunkSize).

%! chunks(-Tree:compound, ?Chunks:list(compound))//

chunks(chunks(L), L) -->
  chunks(L).
chunks([chunk(ChunkSize,ChunkData,ChunkExtension)|T]) -->
  chunk(_T1, ChunkSize, ChunkData, ChunkExtension),
  chunks(T).
chunks([]) --> [].

%! comment(-Tree:compound, ?Comment:atom)//
% Comments can be included in some HTTP header fields by surrounding
% the comment text with parentheses (singular parenthesis//1).
% Comments are only allowed in fields containing `“comment”` as part of
% their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.bnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~

comment(comment(Comment), Comment) -->
  {nonvar(Comment)}, !,
  {atom_codes(Comment, Codes)},
  comment(Codes).
comment(comment(Comment), Comment) -->
  comment(Codes),
  {atom_codes(Comment, Codes)}.
comment(Codes) -->
  opening_round_bracket,
  comment_(Codes),
  closing_round_bracket.
comment_([]) --> [].
comment_([H|T]) -->
  ctext(H),
  comment_(T).
comment_([H1,H2|T]) -->
  quoted_pair([H1,H2]),
  comment_(T).
% Comments can contain other comments.
% Since ctext//1 and quoted_pair//1 are on codes level, we need to process
% these nested comments on codes level as well. This means that we cannot
% include them in the parse tree!
comment_(L) -->
  opening_round_bracket(H),
  comment_(T),
  closing_round_bracket(X),
  {append([H|T], [X], L)}.

%! content_codings(-Tree:compound, ?ContentEncoding:atom)//
% Content coding values indicate an encoding transformation that has
% been or can be applied to an entity. Content codings are primarily
% used to allow a document to be compressed or otherwise usefully
% transformed without losing the identity of its underlying media type
% and without loss of information. Frequently, the entity is stored in
% coded form, transmitted directly, and only decoded by the recipient.
%
% ~~~{.bnf}
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

%! ctext(?CText:code)//
% ~~~{.bnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~

ctext(C) -->
  text(C),
  {\+ memberchk(C, [40,41])}.

%! crlf//

crlf(T1) -->
  crlf(T1, _Codes).

%! crlf(-Tree:atom, ?Codes:list(code))//
% HTTP/1.1 defines the sequence carriage_return//1 line_feed//1
% as the end-of-line marker for all protocol elements except
% the entity-body (see appendix 19.3 for tolerant applications).
% The end-of-line marker within an entity-body is defined by its
% associated media type, as described in section 3.7.

crlf(crlf, [CR,LF]) -->
  carriage_return(CR),
  line_feed(LF).

%! delta_seconds(-Tree:compound, ?Seconds:integer)//
% Some HTTP header fields allow a time value to be specified as an
% integer number of seconds, represented in decimal, after the time
% that the message was received.
%
% ~~~{.bnf}
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
% ~~~{.bnf}
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

%! entity_header(-Tree:compound, ?EntityHeader:pair)//
% ~~~{.bnf}
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

entity_header(entity_header(T1), allow=Methods) --> allow(T1, Methods).
/*
entity_header --> content_encoding.
entity_header --> content_language.
entity_header --> content_length.
entity_header --> content_location.
entity_header --> content_md5.
entity_header --> content_range.
entity_header --> content_type.
entity_header --> expires.
entity_header --> last_modified.
entity_header(entity_header(T1), ExtensionHeader) -->
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
  {\+ status_code(Status, _Name)},
  {atom_chars(Status, [D1,D2,D3])},
  decimal_digit(D1),
  decimal_digit(D2),
  decimal_digit(D3).
extension_code(extension_code(Status)) -->
  decimal_digit(D1),
  decimal_digit(D2),
  decimal_digit(D3),
  {digits_to_decimal([D1,D2,D3], Status)},
  {\+ status_code(Status, _Name)}.

%! extension_header(-Tree:compound, ?ExtensionHeader:pair)//
% ~~~{.bnf}
% extension-header = message-header
% ~~~

extension_header(extension_header(T1), Header) -->
  message_header(T1, Header).

%! extension_method(-Tree:compound, ?ExtensionMethod:atom)//
% ~~~{.bnf}
% extension-method = token
% ~~~

extension_method(extension_method(ExtensionMethod), ExtensionMethod) -->
  token(ExtensionMethod).

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
% ~~~{.bnf}
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
% Note that the specification allows field_content//2 to be `*TEXT*.
% An infinite number of empty TEXTs occur at every spot
% in every field_value/2.
% We solve this by using `+TEXT` instead.

field_content(field_content(T), [H]) --> texts(T, H).
field_content(field_content(T1,T2), [H|T]) -->
  (token(T1, H) ; quoted_string(T1, H)),
  field_content_(T2, T).
field_content_(field_content(T1,T2), [H|T]) -->
  (token(T1, H) ; separator ; quoted_string(T1, H)),
  field_content_(field_content(T2), T).
field_content_(field_content(T), [H]) -->
  (token(T, H) ; quoted_string(T, H)).

%! field_name(-Tree:compound, FieldName:atom)//
% ~~~{.bnf}
% field-name = token
% ~~~

field_name(field_name(T), FieldName) --> token(T, FieldName).

%! field_value(-Tree:compound, ?FieldValue:atom)//
% ~~~{.bnf}
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
% ~~~{.bnf}
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ~~~

generic_message(T, MessageHeaders, MessageBody) -->
  start_line(T1),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  crlf(T3),
  (message_body(T4, MessageBody) ; "", {MessageBody = []}),
  {parse_tree(generic_message, [T1,T2,T3,T4], T)}.

%! http_message(-Tree:compound)//
% ~~~{.bnf}
% HTTP-message = Request | Response ; HTTP/1.1 messages
% ~~~

http_message(http_message(T)) -->
  request(T, _Method, _URI, _Version, _MessageHeaders, _MessageBody).
http_message(http_message(T)) -->
  response(T, _Version, _Status, _MessageHeaders, _MessaageBody).

http_to_gv(Tree):-
  absolute_file_name(project(temp), File, [access(write), file_type(jpeg)]),
  convert_tree_to_gv([name('HTTP message')], Tree, dot, jpeg, File).

%! http_url(
%!   -Tree:compound,
%!   ?Host:list(atomic),
%!   ?Port:integer,
%!   ?Path:list(list(atom)),
%!   ?Query:atom
%! )//
% ~~~{.bnf}
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ~~~
%
% @arg Tree A parse tree.
% @arg Host
% @arg Port An integer representing a port.
%      Defaults to the value for setting `default_port`.
% @arg Path
% @arg Query

http_url(T, Host, Port, Path, Query) -->
  "http://",
  host(T1, Host),
  (
    "", {var(Port), setting(default_port, Port)}
  ;
    ":'", port(T2, Port)
  ),
  (
    "", {var(Path), var(Query)}
  ;
    absolute_path(T3, Path),
    (
      "", {var(Query)}
    ;
      "?", query(T4, Query)
    )
  ),
  {parse_tree(http_url, [T1,T2,T3,T4], T)}.

%! http_version(-Tree:compound, ?Major:integer, ?Minor:integer) is det.
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.bnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~

http_version(
  http_version('HTTP','/',major(Major),'.',minor(Minor)),
  Major,
  Minor
) -->
  "HTTP",
  forward_slash,
  decimal_number(Major),
  dot,
  decimal_number(Minor).

%! last_chunk(-Tree:compound, ?LastChunkExtension:list)//
% The last chunk in a chunked_body//3.
%
% ~~~{.bnf}
% last-chunk = 1*("0") [ chunk-extension ] CRLF
% ~~~
%
% Notice that chunk_extension//2 could be left out completely,
% i.e., no empty list either.

last_chunk(T0, ChunkExtension) -->
  % A positive number of zeros.
  dcg_multi(zero, between(1,_)),
  % Optional chunk extension (attribute-value pairs).
  (
    "",
    {var(ChunkExtension)},
    {T0 = last_chunk(T2)}
  ;
    chunk_extension(T1, ChunkExtension),
    {T0 = last_chunk(T1,T2)}
  ),
  crlf(T2).

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
  ("", {L = T} ; crlf([H1,H2]), {L = [H1,H2|T]}),
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
% ~~~{.bnf}
% media-type = type "/" subtype *( ";" parameter )
% ~~~
%
% @tbd Check the IANA for registered media types.

media_type(media_type(T1,'/',T2,T3), Type, SubType, Parameters) -->
  type(T1, Type),
  forward_slash,
  subtype(T2, SubType),
  parameters(T3, Parameters).

%! message_body(-Tree:compound, ?MessageBody:list(code))//
% he message-body (if any) of an HTTP message is used to carry the
% entity-body associated with the request or response. The message-body
% differs from the entity-body only when a transfer-coding has been
% applied, as indicated by the `Transfer-Encoding` header field.
%
% `Transfer-Encoding` is a property of the message, not of the
% entity, and thus MAY be added or removed by any application along the
% request/response chain.
%
% ~~~{.bnf}
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
% MUST NOT include a message_body//1. All other responses do include a
% message_body//1, although it MAY be of zero length.
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

message_body(message_body(T0), MessageBody) -->
  entity_body(T0, MessageBody).
message_body(message_body(T0), MessageBody) -->
  encoded_entity_body(T0, MessageBody).

%! message_header(-Tree:compound, ?NVPair)//
% ~~~{.bnf}
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
  crlf(T2).
message_headers(message_headers(T1,T2,T3), [H|T]) -->
  message_header(T1, H),
  crlf(T2),
  message_headers(T3, T).

%! method(-Tree:compound, ?Method:atom)//
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
% ~~~{.bnf}
% Method = "OPTIONS" | "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE"
%        | "CONNECT" | extension-method
% ~~~

method(method('CONNECT'), connect) --> "CONNECT".
method(method('DELETE'), delete) --> "DELETE".
method(method('GET'), get) --> "GET".
method(method('HEAD'), head) --> "HEAD".
method(method('OPTIONS'), options) --> "OPTIONS".
method(method('POST'), post) --> "POST".
method(method('PUT'), put) --> "PUT".
method(method('TRACE'), trace) --> "TRACE".
method(method(T), ExtensionMethod) --> extension_method(T, ExtensionMethod).

methods(methods(T1,',',T2), [H|T]) -->
  method(T1, H),
  comma,
  methods(T2, T).
methods(methods(T1), [H]) -->
  method(T1, H).

%! opaque_tag(-Tree:compound, ?OpaqueTag:atom)//

opaque_tag(opaque_tag(T), OpaqueTag) --> quoted_string(T, OpaqueTag).

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
% ~~~{.bnf}
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
% token character MAY appear in a product_version//2, this token//2 SHOULD
% only be used for a version identifier (i.e., successive versions of
% the same product SHOULD only differ in the product_version//2 portion of
% product//3).

product(product(name(T1),T2), Name, Version) -->
  token(T1, Name),
  forward_slash,
  product_version(T2, Version).
product(product(name(T1)), Name, Version) -->
  {var(Version)},
  token(T1, Name).

%! product_version(-Tree:compound, ?Version:atom)//
% ~~~{.bnf}
% product-version = token
% ~~~

product_version(product_version(T), Version) --> token(T, Version).

%! qdtext(?Code:code)//
% ~~~{.bnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(C) -->
  text(C),
  % Exclude double quote.
  {\+ C == 34}.

%! quoted_pair(?Codes:list(code))//
% The backslash//1 character (`“\”`) MAY be used as a single-character
% quoting mechanism only within quoted_string//1 and comment//1 constructs.
% ~~~{.bnf}
% quoted-pair = "\" CHAR
% ~~~

quoted_pair([Backslash,Char]) -->
  backslash(Backslash),
  [Char].

%! quoted_string(-Tree:compound, ?QuotedString:atom)//
% A string of text is parsed as a single word if it is quoted using
% double_quote//1 marks.
%
% ~~~{.bnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~

quoted_string(quoted_string(QuotedString), QuotedString) -->
  {nonvar(QuotedString)}, !,
  {atom_codes(QuotedString, Codes)},
  quoted_string(Codes).
quoted_string(quoted_string(QuotedString), QuotedString) -->
  quoted_string(Codes),
  {atom_codes(QuotedString, Codes)}.
quoted_string(L) -->
  double_quote(H),
  quoted_string_(T),
  double_quote(X),
  {append([H|T], [X], L)}.
quoted_string_([]) --> [].
quoted_string_([H|T]) -->
  qdtext(H),
  quoted_string_(T).
quoted_string_([H1,H2|T]) -->
  quoted_pair([H1,H2]),
  quoted_string_(T).

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
% ~~~{.bnf}
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ~~~
%
% 'Quality values' is a misnomer, since these values merely represent
% relative degradation in desired quality.

qvalue(qvalue(QValue), QValue) -->
  zero,
  dot,
  decimal_number(After),
  {
    between(0, 999, After),
    integers_to_float(1, After, QValue)
  }.
qvalue(qvalue(1.0), 1.0) -->
  one,
  dot,
  dcg_multi(zero, between(0,3)).

%! range_unit(-Tree:compound, ?RangeUnit:atom)//
% HTTP/1.1 allows a client to request that only part (a range of) the
% response entity be included within the response.
% An entity can be broken down into subranges according to various
% structural units.

range_unit(range_unit(T), RangeUnit) --> bytes_unit(T, RangeUnit).
range_unit(range_unit(T), RangeUnit) --> other_range_unit(T, RangeUnit).

%! reason_phrase(-Tree:compound, ?ReasonPhrase:atom)//
% The reason_phrase//2 is intended to give a short textual description
% of the status_code//3.
%
% The status_code//3 is intended for use by automata and the reason_phrase//2
% is intended for the human user.
%
% The client is not required to examine or display the reason_phrase//2.

reason_phrase(reason_phrase(ReasonPhrase), ReasonPhrase) -->
  {nonvar(ReasonPhrase)}, !,
  {atom_codes(ReasonPhrase, Codes)},
  reason_phrase_(Codes).
reason_phrase(reason_phrase(ReasonPhrase), ReasonPhrase) -->
  reason_phrase_(Codes),
  {atom_codes(ReasonPhrase, Codes)}.
reason_phrase_([H|T]) -->
  [H],
  {\+ code_type(H, cntrl)},
  {\+ memberchk(H, [13,10])},
  reason_phrase_(T).
reason_phrase_([]) --> [].

%! request(
%!   -Tree:compound,
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% An HTTP request.
%
% ~~~{.bnf}
% Request = Request-Line
%           *(( general-header | request-header | entity-header ) CRLF)
%           CRLF
%           [ message-body ]
% ~~~
%
% ## Requested resource determination
%
% In the order of preference:
%   1. request_uri//5 is an absolute_uri//2.
%   2. message_headers//2 includes a `Host` header field.
%   3. Respond with `400 (Bad Request)`.
%
% @tbd Implement specific support for `general_header`, `request_header`,
%      and `entity_header`,
% @tbd Implement requested resource determination.

request(T0, Method, URI, Version, MessageHeaders, MessageBody) -->
  request_line(T1, Method, URI, Version),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  crlf(T3),
  (message_body(T4, MessageBody) ; ""),
  {parse_tree(request, [T1,T2,T3,T4], T0)}.

%! request_header//
% Request-header field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields MAY be given the semantics of request-
% header fields if all parties in the communication recognize them to
% be request-header fields. Unrecognized header fields are treated as
% entity-header fields.
%
% ~~~{.bnf}
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
request_header --> accept.
request_header --> accept_charset.
request_header --> accept_encoding.
request_header --> accept_language.
request_header --> authorization.
request_header --> expect.
request_header --> from.
request_header --> host.
request_header --> if_match.
request_header --> if_modified_since.
request_header --> if_none_match.
request_header --> if_range.
request_header --> if_unmodified_since.
request_header --> max_forwards.
request_header --> proxy_authorization.
request_header --> range.
request_header --> referer.
request_header --> te.
request_header --> user_agent.
*/
%! request_line(
%!   -Tree:compound,
%!   ?Method:atom,
%!   ?URI:compound,
%!   ?Version:compound
%! )//
% The first line of an HTTP request.
%
% ~~~{.bnf}
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
% ~~~

request_line(
  request_line(T1,T2,T3,T4,T5,T6),
  Method,
  uri(Scheme, Authority, Path, Query),
  version(Major, Minor)
) -->
  method(T1, Method),
  space, {T2 = sp},
  request_uri(T3, Scheme, Authority, Path, Query),
  space, {T4 = sp},
  http_version(T5, Major, Minor),
  crlf(T6).

%! request_uri(
%!   -Tree:compound,
%!   ?Scheme:atom,
%!   ?Authority:compound,
%!   ?Path:list(list(atom)),
%!   ?Query:atom
%! )//
% Identifies the resource upon which to apply the request.
%
% ~~~{.bnf}
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
% The most common variant of request_uri//5 is that used to identify a
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
% The absolute_path//2 cannot be empty. If none is present it MUST be
% given as `/` (i.e., the server root).

request_uri(request_uri(T1), Scheme, Authority, Path, Query) -->
  absolute_uri(T1, Scheme, Authority, Path, Query).
request_uri(request_uri(T1), _Scheme, _Authority, Path, _Query) -->
  absolute_path(T1, Path).
request_uri(request_uri(T1), _Scheme, Authority, _Path, _Query) -->
  authority(T1, Authority).
request_uri(request_uri('*'), _Scheme, _Authority, _Path, _Query) -->
  asterisk.

%! response(
%!   -Tree:compound,
%!   ?Version:compound,
%!   ?Status:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% After receiving and interpreting a request message, a server responds
% with an HTTP response message.
%
% ~~~{.bnf}
% Response = Status-Line
%            *(( general-header | response-header | entity-header ) CRLF)
%            CRLF
%            [ message-body ]
% ~~~

response(T0, Version, Status, MessageHeaders, MessageBody) -->
  status_line(T1, Version, Status),
  ("", {MessageHeaders = []} ; message_headers(T2, MessageHeaders)),
  crlf(T3),
  (message_body(T4, MessageBody) ; "", {MessageBody = []}),
  {parse_tree(response, [T1,T2,T3,T4], T0)}.

%! response_header//
% ~~~{.bnf}
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
% response_header// field names can be extended reliably only in
% combination with a change in the protocol version. However, new or
% experimental header fields MAY be given the semantics of response-
% header fields if all parties in the communication recognize them to
% be response_header// fields. Unrecognized header fields are treated as
% entity_header// fields.
/*
response_header --> accept_ranges.
response_header --> age.
response_header --> etag.
response_header --> location.
response_header --> proxy_authenticate.
response_header --> retry_after.
response_header --> server.
response_header --> vary.
response_header --> www_authenticate.
*/
%! separator//

separator -->
  separator(_C).

%! separator(?Code:code)//
% ~~~{.bnf}
% separators = "(" | ")" | "<" | ">" | "@"
%              | "," | ";" | ":" | "\" | <">
%              | "/" | "[" | "]" | "?" | "="
%              | "{" | "}" | SP | HT
% ~~~

separator(C) --> at_sign(C). % 64
separator(C) --> bracket(C). % 40,41,91,93,123,125
separator(C) --> colon(C). % 58
separator(C) --> comma(C). %44
separator(C) --> double_quote(C). %34
separator(C) --> equals_sign(C). % 61
separator(C) --> greater_than_sign(C). % 62
separator(C) --> horizontal_tab(C). % 9
separator(C) --> question_mark(C). % 63
separator(C) --> semi_colon(C). % 59
separator(C) --> slash(C). % 47, 92
separator(C) --> less_than_sign(C). % 60
separator(C) --> space(C). % 32

%! start_line(-Tree:compound)//
% ~~~{.bnf}
% start-line = Request-Line | Status-Line
% ~~~

start_line(start_line(T0)) --> request_line(T0, _Method, _URI, _Version).
start_line(start_line(T0)) --> status_line(T0, _Version, _Status).

%! status_code(?Status:integer, ?Reason:atom) is nondet.
% The first digit of the status_code/2 defines the class of response.
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

status_code(100, 'Continue').
status_code(101, 'Switching Protocols').
status_code(200, 'OK').
status_code(201, 'Created').
status_code(202, 'Accepted').
status_code(203, 'Non-Authoritative Information').
status_code(204, 'No Content').
status_code(205, 'Reset Content').
status_code(206, 'Partial Content').
status_code(300, 'Multiple Choices').
status_code(301, 'Moved Permanently').
status_code(302, 'Found').
status_code(303, 'See Other').
status_code(304, 'Not Modified').
status_code(305, 'Use Proxy').
status_code(307, 'Temporary Redirect').
status_code(400, 'Bad Request').
status_code(401, 'Unauthorized').
status_code(402, 'Payment Required').
status_code(403, 'Forbidden').
status_code(404, 'Not Found').
status_code(405, 'Method Not Allowed').
status_code(406, 'Not Acceptable').
status_code(407, 'Proxy Authentication Required').
status_code(408, 'Request Timeout').
status_code(409, 'Conflict').
status_code(410, 'Gone').
status_code(411, 'Length Required').
status_code(412, 'Precondition Failed').
status_code(413, 'Request Entity Too Large').
status_code(414, 'Request URI Too Large').
status_code(415, 'Unsupported Media Type').
status_code(416, 'Requested Range not Satisfiable').
status_code(417, 'Expectation Failed').
status_code(500, 'Internal Server Error').
status_code(501, 'Not Implemented').
status_code(502, 'Bad Gateway').
status_code(503, 'Service Unavailable').
status_code(504, 'Gateway Timeout').
status_code(505, 'HTTP Version not supported').

%! status_code(-Tree:compound, ?Status:integer, ?Reason:atom)//
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

status_code(status_code(Status), Status, Reason1) -->
  {nonvar(Status)}, !,
  {
    status_code(Status, Reason2),
    number_to_digits(Status, [D1,D2,D3])
  },
  decimal_digit(D1),
  decimal_digit(D2),
  decimal_digit(D3),
  % Use the default reason for the given status.
  {(var(Reason1) -> Reason1 = Reason2 ; true)}.
status_code(status_code(Status), Status, _Reason1) -->
  decimal_digit(D1),
  decimal_digit(D2),
  decimal_digit(D3),
  {
    digits_to_decimal([D1,D2,D3], Status),
    status_code(Status, _Reason2)
  }.
status_code(status_code(Status), Status, _Reason) -->
  extension_code(Status).

%! status_line(-Tree:compound, ?Version:compound, ?Status:compound)//
% The first line of a response// message.

status_line(
  status_line(T1,T2,T3,T4,T5,T6),
  version(Major, Minor),
  status(Status, Reason)
) -->
  http_version(T1, Major, Minor),
  space, {T2 = sp},
  status_code(T3, Status, Reason),
  space, {T4 = sp},
  reason_phrase(T5, Reason),
  crlf(T6).

%! subtype(-Tree:compound, ?SubType:atom)//
% Media subtype.
%
% ~~~{.bnf}
% subtype = token
% ~~~

subtype(subtype(SubType), SubType) -->
  token(SubType).

%! text(?Code:code)//
% The text//1 rule is only used for descriptive field contents and values
% that are not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
% ISO-8859-1 [22] only when encoded according to the rules of RFC 2047 [14].
%
% ~~~{.bnf}
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~
%
% A crlf//1 is allowed in the definition of text//1 only as part of
% a header field continuation.
% It is expected that the folding linear_white_space//1 will be replaced
% with a single space//1 before interpretation of the text//1 value.

text(C) -->
  [C],
  {\+ code_type(C, cntrl)}.

%! texts(-Tree:compound, ?Text:atom)//

texts(texts(Text), Text) -->
  {nonvar(Text)}, !,
  {atom_codes(Text, Codes)},
  texts(Codes).
texts(texts(Text), Text) -->
  texts(Codes),
  {atom_codes(Text, Codes)}, !.
texts([H|T]) -->
  text(H),
  texts(T).
texts([H]) -->
  text(H).

%! token(-Tree:compound, ?Token:atom)//
% Many HTTP/1.1 header field values consist of words separated by
% linear_white_space//1 or special//1 characters.
% These special//1 characters MUST be in a quoted_string//1
% to be used within a parameter value (as defined in section 3.6).
%
% ~~~{.bnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~

token(token(Token), Token) -->
  {nonvar(Token)}, !,
  {atom_codes(Token, Codes)},
  token(Codes).
token(token(Token), Token) -->
  token(Codes),
  {atom_codes(Token, Codes)}.
token([H|T]) -->
  token_(H),
  token(T).
token([C]) -->
  token_(C).
token_(C) -->
  [C],
  {
    \+ code_type(C, cntrl),
    \+ memberchk(
      C,
      [9,32,34,40,41,44,47,58,59,60,61,62,63,64,91,92,93,123,125]
    )
  }.

%! trailer(-Tree:compound, ?EntityHeaders:list)//
% The trailer of a chunked_body//3.
%
% ~~~{.bnf}
% trailer = *(entity-header CRLF)
% ~~~

trailer(trailer(T1,T2), [H]) -->
  entity_header(T1, H),
  crlf(T2).
trailer(trailer(T1,T2,T3), [H|T]) -->
  entity_header(T1, H),
  crlf(T2),
  trailer(T3, T).

%! transfer_coding(-Tree:compound, ?Token:atom, ?Parameters:list)//
% Transfer-coding values are used to indicate an encoding
% transformation that has been, can be, or may need to be applied to an
% entity-body in order to ensure safe transport through the network.
% This differs from a content coding in that the transfer-coding is a
% property of the message, not of the original entity.
%
% ~~~{.bnf}
% transfer-coding = "chunked" | transfer-extension
% transfer-extension = token *( ";" parameter )
% ~~~
%
% Parameters are in  the form of attribute/value pairs.
%
% ~~~{.bnf}
% parameter = attribute "=" value
% attribute = token
% value = token | quoted-string
% ~~~
%
% All transfer-coding values are case-insensitive.

transfer_coding(transfer_coding(chunked), chunked, []) -->
  "chunked".
transfer_coding(transfer_coding(T), Token, Parameters) -->
  transfer_extension(T, Token, Parameters).

%! transfer_extension(-Tree:compound, ?Token:atom, ?Parameters:list)//

transfer_extension(transfer_extension(T1,T2), Token, Parameters) -->
  token(T1, Token),
  parameters(T2, Parameters).

%! type(-Tree:compound, ?Type:atom)//
% Media type.
%
% ~~~{.bnf}
% type = token
% ~~~

type(type(Type), Type) -->
  token(Type).

%! value(-Tree:compound, ?Value:atom)//

value(value(T), Value) -->
  token(T, Value).
value(value(T), Value) -->
  quoted_string(T, Value).

%! weak(-Tree:compound)//

weak(weak('W/'), true) --> "W/".

