:- module(
  rfc3987_dcg,
  [
    'IRI-reference'//0
  ]
).

/** <module> RFC 3987

## Definitions

  * *Character*
    A member of a set of elements used for the organization,
    control, or representation of data.
  * *|character encoding|*
    A method of representing a sequence of characters as a sequence of octets.
    Also, a method of (unambiguously) converting a sequence of octets into
    a sequence of characters.
  * *|Character repertoire|*
    A set of characters.
  * *Charset*
    The name of a parameter or attribute used to identify
    a character encoding.
  * *|IRI reference|*
    An IRI reference may be absolute or relative.
    However, the "IRI" that results from such a reference only includes
    absolute IRIs; any relative IRI references are resolved to their
    absolute form.
  * *Octet*
    An ordered sequence of eight bits considered as a unit.
  * *|Presentation element|*
    A presentation form corresponding to a protocol element; for example,
    using a wider range of characters.
  * *|Protocol element|*
    Any portion of a message that affects processing of that message
    by the protocol in question.
  * *|Running text|*
    Human text (paragraphs, sentences, phrases) with syntax according to
    orthographic conventions of a natural language, as opposed to syntax
    defined for ease of processing by machines (e.g., markup,
    programming languages).
  * *|UCS: Universal Character Set|*
    The coded character set defined by ISO/IEC 10646 and the Unicode Standard.

@author Wouter Beek
@see http://tools.ietf.org/html/rfc3987
@version 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(standards(abnf)).



% DCG RULES %

% There are two types of IRI reference: (1) IRI, (2) IRI relative reference.
'IRI-reference' --> 'IRI'.
'IRI-reference' --> 'irelative-ref'.

% IRI
'IRI' --> scheme, ":", 'ihier-part', ("?", iquery ; ""), ("#", ifragment ; "").

% IRI-1: IRI scheme.
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot.
scheme --> 'ALPHA', scheme_.
scheme_ --> [].
scheme_ --> ('ALPHA' ; 'DIGIT' ; "+" ; "-" ; "."), scheme_.

% IRI-2: IRI hierarchical part.
'ihier-part' --> "//", iauthority, 'ipath-abempty'.
'ihier-part' --> 'ipath-absolute'.
'ihier-part' --> 'ipath-rootless'.
'ihier-part' --> 'ipath-empty'.

% IRI-2.1: IRI authority.
% If the user info occurs, it is separated from the host with an ampesat.
% If the port occurs, it is separated from the host with a colon.
iauthority --> (iuserinfo, "@" ; ""), ihost, (":", port ; "").

% IRI-2.1.1: User info.
iuserinfo --> [].
iuserinfo --> (iunreserved ; 'pct-encoded' ; 'sub-delims' ; ":"), iuserinfo.

% IRI-2.1.2: Host.
% A host denotes a physical machine that is connected to the Internet.
ihost --> 'IP-literal'.
ihost --> 'IPv4address'.
ihost --> 'ireg-name'.

'IP-literal' --> "[", ('IPv6address' ; 'IPvFuture'), "]".

'IPv6address' --> dcg_multi(h16, 6, [separator(colon)]), ":", ls32.
'IPv6address' --> "::", dcg_multi(h16, 5, [separator(colon)]), ":", ls32.
'IPv6address' -->
  dcg_multi(h16, _-1, [separator(colon)]), "::",
  dcg_multi(h16, 4, [separator(colon)]), ":", ls32.
'IPv6address' -->
  dcg_multi(h16, _-2, [separator(colon)]), "::",
  dcg_multi(h16, 3, [separator(colon)]), ":", ls32.
'IPv6address' -->
  dcg_multi(h16, _-3, [separator(colon)]), "::",
  dcg_multi(h16, 2, [separator(colon)]), ":", ls32.
'IPv6address' -->
  dcg_multi(h16, _-4, [separator(colon)]), "::",
  dcg_multi(h16, 1, [separator(colon)]), ":", ls32.
'IPv6address' --> dcg_multi(h16, _-5, [separator(colon)]), "::", ls32.
'IPv6address' --> dcg_multi(h16, _-6, [separator(colon)]), "::", h16.
'IPv6address' --> dcg_multi(h16, _-7, [separator(colon)]), "::".

h16 --> dcg_multi('HEXDIG', 1-4).

ls32 --> h16, ":", h16.
ls32 --> 'IPv4address'.

'IPv4address' --> dcg_multi('dec-octet', 4, [separator(dot)]).

'dec-octet' --> 'DIGIT'. % 0-9
'dec-octet' --> between_digit(1, 9, _, _), 'DIGIT'. % 10-99
'dec-octet' --> "1", dcg_multi('DIGIT', 2). % 100-199
'dec-octet' --> "2", between_digit(0, 4, _, _), 'DIGIT'. % 200-249
'dec-octet' --> "25", between_digit(0, 5, _, _). % 250-255

'IPvFuture' --> "v", dcg_multi('HEXDIG', 1-_), ".", dcg_multi('IPvFuture_', 1-_).
'IPvFuture_' --> unreserved ; 'sub-delims' ; ":".

'ireg-name' --> [].
'ireg-name' --> (iunreserved ; 'pct-encoded' ; 'sub-delims'), 'ireg-name'.

'ipath-abempty' --> [].
'ipath-abempty' --> "/", isegment, 'ipath-abempty'.

isegment --> [].
isegment --> ipchar, isegment.

'ipath-absolute' --> "/", ('isegment-nz', isegments ; "").

'isegment-nz' --> ipchar.
'isegment-nz' --> ipchar, 'isegment-nz'.

isegments --> [].
isegments --> "/", isegment, isegments.

'ipath-rootless' --> 'isegment-nz', isegments.

'ipath-empty' --> [].

% IRI-2.1.3: Port.
port --> [].
port --> 'DIGIT', port.

% IRI-2.2: Query.
iquery --> [].
iquery --> ipchar ; iprivate ; "/" ; "?".

% IRI-2.3: Fragment identifier.
ifragment --> [].
ifragment --> ipchar ; "/" ; "?".

% Relative IRI.
'irelative-ref' --> 'irelative-part', ("?", iquery ; ""), ("#", ifragment ; "").

'irelative-part' --> "//", iauthority, 'ipath-abempty'.
'irelative-part' --> 'ipath-absolute'.
'irelative-part' --> 'ipath-noscheme'.
'irelative-part' --> 'ipath-empty'.

'ipath-noscheme' --> 'isegment-nz-nc', isegments.

% Non-zero-length segment without any colon ":".
'isegment-nz-nc' --> iunreserved ; 'pct-encoded' ; 'sub-delims' ; "@".
'isegment-nz-nc' -->
  (iunreserved ; 'pct-encoded' ; 'sub-delims' ; "@"),
  'isegment-nz-nc'.



% CHARACTERS %

ipchar --> iunreserved ; 'pct-encoded' ; 'sub-delims' ; ":" ; "@".

iprivate -->
  ( between_hex('E000',   'F8FF',   _)
  ; between_hex('F0000',  'FFFFD',  _)
  ; between_hex('100000', '10FFFD', _)
  ).

iunreserved --> 'ALPHA' ; 'DIGIT' ; "-" ; "." ; "_" ; "~" ; ucschar.

'pct-encoded' --> "%", 'HEXDIG', 'HEXDIG'.

'sub-delims' --> "!" ; "$" ; "&" ; "'" ; "(" ; ")" ; "*" ; "+" ; "," ; ";" ; "=".

ucschar -->
  ( between_hex('A0',    'D7FF',  _)
  ; between_hex('F900',  'FDCF',  _)
  ; between_hex('FDF0',  'FFEF',  _)
  ; between_hex('10000', '1FFFD', _)
  ; between_hex('20000', '2FFFD', _)
  ; between_hex('30000', '3FFFD', _)
  ; between_hex('40000', '4FFFD', _)
  ; between_hex('50000', '5FFFD', _)
  ; between_hex('60000', '6FFFD', _)
  ; between_hex('70000', '7FFFD', _)
  ; between_hex('80000', '8FFFD', _)
  ; between_hex('90000', '9FFFD', _)
  ; between_hex('A0000', 'AFFFD', _)
  ; between_hex('B0000', 'BFFFD', _)
  ; between_hex('C0000', 'CFFFD', _)
  ; between_hex('D0000', 'DFFFD', _)
  ; between_hex('E1000', 'EFFFD', _)
  ).

unreserved --> 'ALPHA' ; 'DIGIT' ; "-" ; "." ; "_" ; "~".



% EXTRA RULES % 

'absolute-IRI' --> scheme, ":", 'ihier-part', ("?", iquery ; "").

'gen-delims' --> ":" ; "/" ; "?" ; "#" ; "[" ; "]" ; "@".

% Begins with "/" or is empty.
ipath --> 'ipath-abempty'.
% Begins with "/" but not "//".
ipath --> 'ipath-absolute'.
% Begins with a non-colon segment
ipath --> 'ipath-noscheme'.
% Begins with a segment
ipath --> 'ipath-rootless'.
% Zero characters.
ipath --> 'ipath-empty'.

reserved --> 'gen-delims'.
reserved --> 'sub-delims'.
