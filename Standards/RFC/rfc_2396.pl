:- module(rfc_2396, []).

/** <module> URI

A grammar and a description of basic functionality for URI.

"This document defines a grammar that is a superset of all valid URI,
such that an implementation can parse the common components of a URI
reference without knowing the scheme-specific requirements of every
possible identifier type. This document does not define a generative
grammar for URI; that task will be performed by the individual
specifications of each URI scheme."

# Characters

  * **Character**
    A distinguishable semantic entity.
  * **Charset**
    A sequence of octets defined by a component of the URI
    is used to represent a sequence of characters.
    A charset defines this mapping.
  * **Octet**
    An 8-bit byte

## Mappings:

  * From URI characters to octets.
  * From octets to original characters.

~~~{.txt}
URI character sequence -> octet sequence -> original character sequence
~~~

Simplest situation: US-ASCII original character `C` is represented by
the octet for the US-ASCII code for `C`.

# Concepts

  * **Identifier**
    An object that can act as a reference to something that has identity.
  * **Resource**
    Anything that has identity.
  * **Uniform Resource Identifier (URI)**
    A means for identifying a resource.

# The big divide

## Category 1

**Propert names**, rigid designators, e.g., electronic documents, images.
Extensional semantics.

## Category 2

**Services**, descriptions, e.g., "today's weather report for Los Angeles".
Intensional semantics.

> The resource is the conceptual mapping to an entity or set of
> entities, not necessarily the entity which corresponds to that
> mapping at any particular instance in time. Thus, a resource
> can remain constant even when its content -- the entities to
> which it currently corresponds -- changes over time, provided
> that the conceptual mapping is not changed in the process.

# Variants

  * URL
    A location or name or both.
    Identification by primary access mechanism (e.g., network location)
    rather than by name.
  * URN
    Globally unique and persistent.
    Even when the resource caeses to exist or becomes unavailable.

# Excluded characters

## Control characters

The control characters in the US-ASCII coded character set are not
used within a URI, both because they are non-printable and because
they are likely to be misinterpreted by some control mechanisms.

~~~{.bnf}
control = <US-ASCII coded characters 00-1F and 7F hexadecimal>
~~~

## Space character

The space character is excluded because significant spaces may
disappear and insignificant spaces may be introduced when URI are
transcribed or typeset or subjected to the treatment of word-
processing programs.  Whitespace is also used to delimit URI in many
contexts.

~~~{.bnf}
space = <US-ASCII coded character 20 hexadecimal>
~~~

## URI delimiters

The angle-bracket `<` and `>` and double-quote (`"`) characters are
excluded because they are often used as the delimiters around URI in
text documents and protocol fields.  The character `#` is excluded
because it is used to delimit a URI from a fragment identifier in URI
references (Section 4). The percent character `%` is excluded because
it is used for the encoding of escaped characters.

~~~{.bnf}
delims = "<" | ">" | "#" | "%" | <">
~~~

# Others

Other characters are excluded because gateways and other transport
agents are known to sometimes modify such characters, or they are
used as delimiters.

~~~{.bnf}
unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
~~~

@author Wouter Beek
@compat RFC 2396
@see http://www.ietf.org/rfc/rfc2396.txt
@version 2013/05, 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(library(lists)).



%! absolute_path(-PathSegments:list(list(atom)))//
% The path may consist of a sequence of path segments separated by a
% single forward_slash//1.
%
% ~~~{.bnf}
% abs_path = "/"  path_segments
% path_segments = segment *( "/" segment )
% ~~~

absolute_path([Parameters]) -->
  forward_slash,
  path_segment(Parameters).
absolute_path([ParametersH|ParametersT]) -->
  forward_slash,
  path_segment(ParametersH),
  absolute_path(ParametersT).

%! absolute_uri(-Scheme:atom, -SchemeSpecificPart:compound)//
% An absolute URI contains the name of the scheme being used (`<scheme>`)
% followed by a colon//1 and then a string (the `<scheme-specific-part>`)
% whose interpretation depends on the scheme.
%
% ~~~{.txt}
% <scheme>:<scheme-specific-part>
% ~~~
%
% ## Hierarchical relationships
%
% A subset of URI share a common syntax for representing
% hierarchical relationships within the namespace.
% This "generic URI" syntax consists of a sequence of four main components:
% ~~~{.txt}
% <scheme>://<authority><path>?<query>
% ~~~
% each of which, except `<scheme>`, may be absent from a particular URI.
% For example, some URI schemes do not allow an `<authority>` component,
% and others do not use a `<query>` component.
%
% URI that are hierarchical in nature use the forward_slash//0 for
% separating hierarchical components. For some file systems,
% a foward_slash//0 is the delimiter used to construct a
% file name hierarchy, and thus the URI path will look similar to
% a file pathname.
% This does NOT imply that the resource is a file or that the URI
% maps to an actual filesystem pathname.
%
% ~~~{.bnf}
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ~~~

absolute_uri(Scheme, hierarchical_part(Authority, PathSegments, Query)) -->
  scheme(Scheme),
  colon,
  hierarchical_part(Authority, PathSegments, Query).
absolute_uri(Scheme, opaque_part(OpaquePart)) -->
  scheme(Scheme),
  colon,
  opaque_part(OpaquePart).

%! authority(-Authority:compound)//
% Many URI schemes include a top hierarchical element for a naming
% authority, such that the namespace defined by the remainder of the
% URI is governed by that authority. This authority component is
% typically defined by an Internet-based server or a scheme-specific
% registry of naming authorities.
%
% ~~~{.bnf}
% authority = server | registry_based_naming_authority
% ~~~

authority(server(User, Host, Port)) -->
  server(User, Host, Port).
authority(registry_based_naming_authority(Authority)) -->
  registry_based_naming_authority(Authority).

dashed_alpha_numerics([]) --> [].
dashed_alpha_numerics([H|T]) -->
  (alpha_numeric(H) ; hyphen_minus(H)),
  dashed_alpha_numerics(T).

%! domain_label(-DomainLabel:atom)//
% ~~~{.bnf}
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ~~~

domain_label(Char) -->
  alpha_numeric(Code),
  {char_code(Char, Code)}.
domain_label(DomainLabel) -->
  alpha_numeric(H),
  dashed_alpha_numerics(T),
  alpha_numeric(X),
  {append([H|T], [X], Codes),
   atom_codes(DomainLabel, Codes)}.

%! domain_labels(-DomainLabels:list(atom))//
% Hostnames take the form described in Section 3 of [RFC1034] and
% Section 2.1 of [RFC1123]: a sequence of domain labels separated by
% dot//1, each domain label starting and ending with an alphanumeric
% character and possibly also containing hyphen_minus//1 characters.

domain_labels([]) --> [].
domain_labels([DomainLabel|DomainLabels]) -->
  domain_label(L),
  dot(X),
  {append(L, [X], Codes)},
  {atom_codes(DomainLabel, Codes)},
  domain_labels(DomainLabels).

%! escaped_character(-Code:code)//
% An **escaped octet** is encoded as a character triplet, consisting of the
% percent character `%` followed by the two hexadecimal digits
% representing the octet code.
%
% ~~~{.bnf}
% escaped = "%" hex hex
% ~~~
%
% Because the percent `%` character always has the reserved purpose of
% being the escape indicator, it must be escaped as `%25` in order to
% be used as data within a URI.

escaped_character(N) -->
  percent_sign, hexadecimal_digit(D1), hexadecimal_digit(D2),
  % Reconstruct the code from the hexadecimal digits.
  {N is D1 * 16 + D2}.

%! fragment(-Fragment:atom)//
% When a URI reference is used to perform a retrieval action on the
% identified resource, the optional fragment identifier, separated from
% the URI by a number_sign//0, consists of additional reference information
% to be interpreted by the user agent after the retrieval action has been
% successfully completed.
% As such, it is not part of a URI, but is often used in conjunction with
% a URI.
%
% The semantics of a fragment identifier is a property of the data
% resulting from a retrieval action, regardless of the type of URI used
% in the reference.  Therefore, the format and interpretation of
% fragment identifiers is dependent on the media type [RFC2046] of the
% retrieval result.
%
%  A fragment identifier is only meaningful when a URI reference is
% intended for retrieval and the result of that retrieval is a document
% for which the identified fragment is consistently defined.

fragment(Fragment) -->
  fragment_(Codes),
  {atom_codes(Fragment, Codes)}.
fragment_([]) --> [].
fragment_([H|T]) -->
  uri_character(H),
  fragment(T).

%! hierarchical_part(
%!   -Authority:compound,
%!   -PathSegments:list(list(atom)),
%!   -Query:atom
%! )//

hierarchical_part(Authority, PathSegments, Query) -->
  (network_path(Authority, PathSegments) ; absolute_path(PathSegments)),
  ("" ; question_mark, query(Query)).

%! host(-Host:compound)//
% The host is a domain name of a network host, or its IPv4 address as a
% set of four decimal_digits//1 groups separated by dot//0.
%
% ~~~{.bnf}
% host = hostname | IPv4address
% ~~~
%
% The rightmost domain label of a fully qualified host or domain name
% will never start with a decimal_digit//1, thus syntactically distinguishing
% domain names from IPv4 addresses, and may be followed by a single dot//0
% if it is necessary to distinguish between the complete host or domain name
% and any local domain.
%
% @tbd Literal IPv6 addresses are not supported.

host(name(HostNameLabels)) --> host_name(HostNameLabels).
host(ipv4(Address)) --> ipv4_address(Address).

%! host_and_port(-Host:compound, -Port:integer)//
% A non-default port number may optionally be supplied, in decimal,
% separated from the host//1 by a colon//0.
%
% ~~~{.bnf}
% hostport = host [ ":" port ]
% ~~~

host_and_port(Host, _NoPort) --> host(Host).
host_and_port(Host, Port) -->
  host(Host),
  colon,
  port(Port).

%! host_name(-HostNameLabels:list(atom))//
% Hostnames take the form described in Section 3 of [RFC1034] and
% Section 2.1 of [RFC1123]: a sequence of domain labels separated by
% dot//0, each domain label starting and ending with an alphanumeric
% character and possibly also containing hyphen_minus//0 characters.
%
% ~~{.bnf}
% hostname = *( domainlabel "." ) toplabel [ "." ]
% ~~~

host_name(HostNameLabels) -->
  domain_labels(DomainLabels),
  top_label(TopLabel),
  ("" ; dot),
  {append(DomainLabels, [TopLabel], HostNameLabels)}.

%! ipv4_address(-Address:list(integer))//
% ~~~{.bnf}
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ~~~
%
% @tbd A suitable representation for including a literal IPv6
%      address as the host part of a URL is desired, but has not yet been
%      determined or implemented in practice.

ipv4_address([N1,N2,N3,N4]) -->
  decimal_number(N1),
  dot,
  decimal_number(N2),
  dot,
  decimal_number(N3),
  dot,
  decimal_number(N4).

mark(C) --> hyphen_minus(C).
mark(C) --> underscore(C).
mark(C) --> dot(C).
mark(C) --> exclamation_mark(C).
mark(C) --> tilde(C).
mark(C) --> asterisk(C).
mark(C) --> single_quote(C).
mark(C) --> round_bracket(C).

%! network_path(-Authority:compound, -PathSegments:list(list(atom)))//

network_path(Authority, PathSegments) -->
  forward_slash, forward_slash,
  authority(Authority),
  ({PathSegments = []} ; absolute_path(PathSegments)).

%! opaque_part(-Atom:atom)//
% URIs that do not make use of the forward_slash//1 for separating
% hierarchical components are considered opaque by the generic URI
% parser.
%
% ~~~{.bnf}
% opaque_part = uric_no_slash *uric
% ~~~

opaque_part(Atom) -->
  opaque_part_(Codes),
  {atom_codes(Atom, Codes)}.
opaque_part_([H|T]) -->
  uri_character_no_slash(H),
  uri_characters(T).

%! parameter(-Parameter:atom)//
% ~~~{.bnf}
% param = *parameter_character
% ~~~

parameter(Parameter) -->
  parameter_(Codes),
  {atom_codes(Parameter, Codes)}.
parameter_([]) --> [].
parameter_([H|T]) -->
  parameter_character(H),
  parameter_(T).

%! parameter_character(-Code:code)//
% ~~~{.bnf}
% pchar = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ~~~

parameter_character(C) --> ampersand(C).
parameter_character(C) --> at_sign(C).
parameter_character(C) --> colon(C).
parameter_character(C) --> comma(C).
parameter_character(C) --> dollar_sign(C).
parameter_character(C) --> equals_sign(C).
parameter_character(C) --> escaped_character(C).
parameter_character(C) --> plus_sign(C).
parameter_character(C) --> unreserved_character(C).

%! path(-Path:compound)//
% The path component contains data, specific to the authority (or the
% scheme if there is no authority component), identifying the resource
% within the scope of that scheme and authority.
%
% ~~~{.bnf}
% path = [ abs_path | opaque_part ]
% ~~~
%
% Note that path//1 is not used in any production.

path(absolute([])) --> [].
path(absolute(PathSegments)) --> absolute_path(PathSegments).
path(opaque(OpaquePart)) --> opaque_part(OpaquePart).

%! path_segment(-Parameters:list(atom))//
% Each path segment may include a sequence of parameters,
% indicated by the semi_colon//0 character.
%
% Within a path segment, the characters forward_slash//0,
% semi_colon//0, equals_sign//0, and question_mark//0 are reserved.
%
% ~~~{.bnf}
% segment = *pchar *( ";" param )
% ~~~

path_segment([H]) -->
  parameter(H).
path_segment([H|T]) -->
  parameter(H),
  semi_colon,
  path_segment(T).

%! port(-Port:integer)//
% The port is the network port number for the server. Most schemes
% designate protocols that have a default port number.
% If the port is omitted, the default port number is assumed.
%
% ~~~{.bnf}
% port = *digit
% ~~~

port(Port) --> decimal_number(Port).

%! query(-Query:atom)//
% The query component is a string of information to be interpreted by
% the resource.
%
% ~~~{.bnf}
% query = *uric
% ~~~
%
% Within a query component, the characters `;`, `/`, `?`, `:`, `@`,
% `&`, `=`, `+`, `,`, and `$` are reserved.

query(Query) -->
  uri_characters(Codes),
  {atom_codes(Query, Codes)}.

%! registry_based_naming_authority(-Authority:atom)//
% The structure of a registry-based naming authority is specific to the
% URI scheme, but constrained to the allowed characters for an
% authority component.
%
% ~~~{.bnf}
% registry_based_naming_authority = 1*( unreserved | escaped | "$" | "," |
%                                       ";" | ":" | "@" | "&" | "=" | "+" )
% ~~~

registry_based_naming_authority(Authority) -->
  registry_based_naming_authority_(Codes),
  {atom_codes(Authority, Codes)}.
registry_based_naming_authority_([H]) -->
  registry_based_naming_authority_character(H).
registry_based_naming_authority_([H|T]) -->
  registry_based_naming_authority_character(H),
  registry_based_naming_authority_(T).

registry_based_naming_authority_character(C) --> unreserved_character(C).
registry_based_naming_authority_character(C) --> escaped_character(C).
registry_based_naming_authority_character(C) -->
  ( dollar_sign(C)
  ; comma(C)
  ; semi_colon(C)
  ; colon(C)
  ; at_sign(C)
  ; ampersand(C)
  ; equals_sign(C)
  ; plus_sign(C)
  ).

/*
%! relative_uri//1
% The syntax for relative URI is a shortened form of that for absolute
% URI, where some prefix of the URI is missing and certain path
% components ("." and "..") have a special meaning when, and only when,
% interpreting a relative path.  The relative URI syntax is defined in
% Section 5.

relative_uri(Query) -->
  network_path(Authority, PathSegments),
  ("" ; question_mark, query(Query)).
relative_uri(PathSegments, Query) -->
  absolute_path(PathSegments),
  ("" ; question_mark, query(Query)).
relative_uri(Query) -->
  relative_path,
  ("" ; question_mark, query(Query)).
*/

%! reserved_character(-Code:code)//
% A character is **reserved** if the semantics of the URI changes
% if the character is replaced with its escaped US-ASCII encoding.
%
% ~~~{.bnf}
% reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ~~~

reserved_character(C) --> semi_colon(C).
reserved_character(C) --> forward_slash(C).
reserved_character(C) --> question_mark(C).
reserved_character(C) --> colon(C).
reserved_character(C) --> at_sign(C).
reserved_character(C) --> ampersand(C).
reserved_character(C) --> equals_sign(C).
reserved_character(C) --> plus_sign(C).
reserved_character(C) --> dollar_sign(C).
reserved_character(C) --> comma(C).

%! scheme(+Scheme:atom)//
% Just as there are many different methods of access to resources,
% there are a variety of schemes for identifying such resources.  The
% URI syntax consists of a sequence of components separated by reserved
% characters, with the first component defining the semantics for the
% remainder of the URI string.
%
% Scheme names consist of a sequence of characters beginning with a
% letter_lowercase//1 and followed by any combination of letter_lowercase//1,
% decimal_digit//1, plus_sign//1, dot//1, or hyphen_minus//1.
% For resiliency, programs interpreting URI should treat upper case letters
% as equivalent to lower case in scheme names (e.g., allow `HTTP` as
% well as `http`).
%
% ~~~{.bnf}
% scheme = alpha *( alpha | digit | "+" | "-" | "." )
% ~~~

scheme(Atom) -->
  scheme_(Codes),
  {atom_codes(Atom, Codes)}.
scheme_([H|T]) -->
  letter(H),
  scheme_characters(T).

scheme_character(C) --> alpha_numeric(C).
scheme_character(C) --> plus_sign(C).
scheme_character(C) --> hyphen_minus(C).
scheme_character(C) --> dot(C).

scheme_characters([]) --> [].
scheme_characters([H|T]) -->
  scheme_character(H),
  scheme_characters(T).

%! server(-User:atom, -Host:compound, -Port:integer)//
% URL schemes that involve the direct use of an IP-based protocol to a
% specified server on the Internet use a common syntax for the server
% component of the URI's scheme-specific data:
% ~~~{.txt}
% <userinfo>@<host>:<port>
% ~~~
% where `<userinfo>` may consist of a user name and, optionally,
% scheme-specific information about how to gain authorization to access
% the server. The parts `<userinfo>@` and `:<port>` may be omitted.
%
% The user information, if present, is followed by a commercial at-sign (`@`).
%
% ~~~{.bnf}
% server = [ [ userinfo "@" ] hostport ]
% ~~~

server(_NoUser, _NoHost, _NoPort) --> [].
server(_NoUser, Host, Port) --> host_and_port(Host, Port).
server(User, Host, Port) -->
  user_info(User),
  at_sign,
  host_and_port(Host, Port).

%! top_label(+Codes:list(code))//
% The rightmost domain label of a fully qualified host or domain name will
% never start with a digit, thus syntactically distinguishing domain
% names from IPv4 addresses, and may be followed by a single dot//0
% if it is necessary to distinguish between the complete domain name
% and any local domain.
%
% ~~~{.bnf}
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ~~~

top_label([C]) --> letter(C).
top_label(L) -->
  letter(H),
  dashed_alpha_numerics(T),
  letter(X),
  {append([H|T], [X], L)}.

%! unreserved_character(+Code:code)//
% Unreserved characters can be escaped without changing the semantics
% of the URI, but this should not be done unless the URI is being used
% in a context that does not allow the unescaped character to appear.

unreserved_character(C) --> alpha_numeric(C).
unreserved_character(C) --> mark(C).

%! uri_character(+Code:code)//
% Everything but the first character of the opaque_part//1
% can be a forward_slash//1 as well.
%
% ~~~{.bnf}
% uric = reserved | unreserved | escaped
% ~~~

uri_character(C) --> reserved_character(C).
uri_character(C) --> unreserved_character(C).
uri_character(C) --> escaped_character(C).

%! uri_character_no_slash(-Code:code)//
% The first character of the opaque_part//1 is not allowed to be a slash//1.
%
% ~~~{.bnf}
% uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
%                 "&" | "=" | "+" | "$" | ","
% ~~~

uri_character_no_slash(C) --> unreserved_character(C).
uri_character_no_slash(C) --> escaped_character(C).
uri_character_no_slash(C) -->
  ( semi_colon(C)
  ; question_mark(C)
  ; colon(C)
  ; at_sign(C)
  ; ampersand(C)
  ; equals_sign(C)
  ; plus_sign(C)
  ; dollar_sign(C)
  ; comma(C)
  ).

%! uri_characters(-Codes:list(code))//

uri_characters --> [].
uri_characters([H|T]) -->
  uri_character(H),
  uri_characters(T).

%! uri_reference//
% The term "URI-reference" is used here to denote the common usage of a
% resource identifier.  A URI reference may be absolute or relative,
% and may have additional information attached in the form of a
% fragment identifier.  However, "the URI" that results from such a
% reference includes only the absolute URI after the fragment
% identifier (if any) is removed and after any relative URI is resolved
% to its absolute form.  Although it is possible to limit the
% discussion of URI syntax and semantics to that of the absolute
% result, most usage of URI is within general URI references, and it is
% impossible to obtain the URI from such a reference without also
% parsing the fragment and resolving the relative form.
%
% ~~~{.bnf}
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ~~~

uri_reference(Scheme, SchemeSpecificPart, Fragment) -->
  absolute_uri(Scheme, SchemeSpecificPart),
  ("", number_sign, fragment(Fragment)).
/*
uri_reference(_Scheme, _SchemeSpecificPart, Fragment) -->
  relative_uri,
  ("", number_sign, fragment(Fragment)).
*/

%! user_info(-User:atom)//
% Some URL schemes use the format `user:password` in the userinfo
% field. This practice is NOT RECOMMENDED, because the passing of
% authentication information in clear text (such as URI) has proven to
% be a security risk in almost every case where it has been used.
%
% ~~~{.bnf}
% userinfo      = *( unreserved | escaped |
%                    ";" | ":" | "&" | "=" | "+" | "$" | "," )
% ~~~

user_info(User) -->
  user_info_(Codes),
  {atom_codes(User, Codes)}.
user_info_([]) --> [].
user_info_([H|T]) -->
  user_info__(H),
  user_info_(T).

user_info__(C) --> unreserved_character(C).
user_info__(C) --> escaped_character(C).
user_info__(C) -->
  ( semi_colon(C)
  ; colon(C)
  ; ampersand(C)
  ; equals_sign(C)
  ; plus_sign(C)
  ; dollar_sign(C)
  ; comma(C)
  ).
