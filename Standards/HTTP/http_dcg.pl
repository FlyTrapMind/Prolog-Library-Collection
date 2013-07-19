:- module(
  http_dcg,
  [
  ]
).

/** <module> HTTP_DCG

DCG rules for the HTTP 1.1 specification.

Tag suggested by Anne Ogborn on the SWI-Prolog mailing list: `doc-needs-help`.

# Concepts

  * **Age**
    The age of a response is the time since it was sent by, or
    successfully validated with, the origin server.
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

@author Wouter Beek
@see Based on RFC 2616, http://tools.ietf.org/html/rfc2616
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).
:- use_module(rfc(rfc_1123)). % Date and time
:- use_module(rfc(rfc_1766)). % Language tag
:- use_module(rfc(rfc_2396)). % URIs

:- setting(default_port, integer, 80, 'The default TCP port.').



%! attribute(-Tree:compound, ?Attribute:atom)//

attribute(attribute(T), Attribute) -->
  token(T, Attribute).

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
%!   ?ChunkData:integer,
%!   ?ChunkData:list(code),
%!   ?ChunkExtension:list
%! )//

chunk(T, ChunkSize, ChunkData, ChunkExtension) -->
  chunk_size(T1, ChunkSize),
  (
    "",
    {ChunkExtension = []},
    {T = chunk(T1,T3)}
  ;
    chunk_extension(T2, ChunkExtension),
    {T = chunk(T1,T2,T3)}
  ),
  crlf,
  chunk_data(T3, ChunkSize, ChunkData),
  crlf.

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

chunked_body(
  chunked_body(T1,T2,T3),
  Chunks,
  LastChunkExtension,
  EntityHeaders
) -->
  % A non-negative number of chunks.
  chunks(T1, Chunks),
  % The last chunk.
  last_chunk(T2, LastChunkExtension),
  % The trailer of the chunk body.
  trailer(T3, EntityHeaders),
  crlf.

%! chunk_data(-Tree:compound, ?ChunkSize:integer, ?ChunkData:list(code))//
% ~~~{.bnf}
% chunk-data = chunk-size(OCTET)
% ~~~

chunk_data(chunk_data(ChunkData), ChunkSize, ChunkData) -->
  {length(ChunkData, ChunkSize)},
  ChunkSize.

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

crlf -->
  crlf(_Codes).

%! crlf(?Codes:list(code))//
% HTTP/1.1 defines the sequence carriage_return//1 line_feed//1
% as the end-of-line marker for all protocol elements except
% the entity-body (see appendix 19.3 for tolerant applications).
% The end-of-line marker within an entity-body is defined by its
% associated media type, as described in section 3.7.

crlf([CR,LF]) -->
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

entity_header(stub) --> [].

%! entity_tag(-Tree:compound, ?Weak:boolean, ?EntityTag:atom)//
% Entity tags are used for comparing two or more entities from the same
% requested resource.

entity_tag(T, Weak, EntityTag) -->
  (
    "", {Weak = false, T = entity_tag(T2)}
  ;
    weak(T1, Weak), T = entity_tag(T1,T2)}
  ),
  opaque_tag(T2, EntityTag).

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

http_version(http_version('HTTP','/',Major,'.',Minor), Major, Minor) -->
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

last_chunk(T, ChunkExtension) -->
  % A positive number of zeros.
  dcg_plus(zero),
  % Optional chunk extension (attribute-value pairs).
  (
    "",
    {var(ChunkExtension)},
    {T = last_chunk}
  ;
    chunk_extension(T1, ChunkExtension),
    {T = last_chunk(T1)}
  ),
  crlf.

%! linear_white_space(?Codes:list(code))//
% HTTP/1.1 header field values can be folded onto multiple lines if the
% continuation line begins with space//1 or horizontal_tab//1.
% All linear white space, including folding, has the same semantics as
% space//1.
% A recipient MAY replace any linear_white_space//1 with a single space//1
% before interpreting the field value or forwarding the message downstream.

linear_white_space([CR,LF,H|T]) -->
  crlf([CR,LF]),
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).
linear_white_space([H|T]) -->
  (space(H) ; horizontal_tab(H)),
  linear_white_space_(T).
linear_white_space_([]) --> [].
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

opaque_tag(opaque_tag(T), OpaqueTag) --> quoted_string(T, OpaqueTag).

other_range_unit(other_range_unit(T), Token) --> token(T, Token).

%! parameter(-Tree:compound, -AttributeValuePair)//

parameter(parameter(T1,'=',T2), Attribute-Value) -->
  attribute(T1, Attribute),
  equals_sign,
  value(T2, Value).

%! parameters(Parameters:list)//

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
  dcg_multi(zero, 3, 0).

%! range_unit(-Tree:compound, ?RangeUnit:atom)//
% HTTP/1.1 allows a client to request that only part (a range of) the
% response entity be included within the response.
% An entity can be broken down into subranges according to various
% structural units.

range_unit(range_unit(T), RangeUnit) --> bytes_unit(T, RangeUnit).
range_unit(range_unit(T), RangeUnit) --> other_range_unit(T, RangeUnit).

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

trailer(trailer(L), L) -->
  trailer(L).
trailer([H|T]) -->
  entity_header(H),
  crlf,
  trailer(T).
trailer([]) --> [].

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

weak(weak('W/'), true) --> "W/".
