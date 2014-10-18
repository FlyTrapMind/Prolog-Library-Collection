:- module(
  rfc3987_dcg,
  [
    'IRI'//1, % -ParseTree:compound
    'IRI-reference'//1 % -ParseTree:compound
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
@version 2013/09, 2014/01
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)). % Used in meta-options.
:- use_module(plDcg(dcg_cardinal)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(parse_tree)).
:- use_module(flp(rfc4234_basic)).
:- use_module(math(radix)).



% 'IRI'(-ParseTree:compound)//
% ~~~{.abnf}
% IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
% ~~~

'IRI'(T0) -->
  scheme(T1),
  ":",
  'ihier-part'(T2),
  ("?", iquery(T3) ; ""),
  ("#", ifragment(T4) ; ""),
  {parse_tree('IRI', [T1,T2,T3,T4], T0)}.


% ! 'IRI-reference'(-ParseTree:compound)// .
% There are two types of IRI reference: (1) IRI, (2) IRI relative reference.
%
% ~~~{.abnf}
% IRI-reference = IRI / irelative-ref
% ~~~

'IRI-reference'('IRI-reference'(T1)) --> 'IRI'(T1).
'IRI-reference'('IRI-reference'(T1)) --> 'irelative-ref'(T1).



% SCHEME %

%! scheme(-ParseTree:compound)// .
% IRI-1: IRI scheme.
%
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot.
%
% ~~~{.abnf}
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ~~~

scheme(scheme(Scheme)) -->
  'ALPHA'(H),
  '*'(scheme_, T),
  {atom_codes(Scheme, [H|T])}.
scheme_(C) --> 'ALPHA'(C).
scheme_(C) --> 'DIGIT'(C, _).
scheme_(C) --> plus_sign(C).
scheme_(C) --> minus_sign(C).
scheme_(C) --> dot(C).



% HIERARCHICAL PART %

%! 'ihier-part'(-ParseTree:compound)// .
% IRI-2: IRI hierarchical part.
%
% ~~~{.abnf}
% ihier-part = "//" iauthority ipath-abempty
%            / ipath-absolute
%            / ipath-rootless
%            / ipath-empty
% ~~~

'ihier-part'('ihier-part'(T1,T2)) -->
  "//",
  iauthority(T1),
  'ipath-abempty'(T2).
'ihier-part'('ihier-part'(T1)) -->
  'ipath-absolute'(T1).
'ihier-part'('ihier-part'(T1)) --> 'ipath-rootless'(T1).
'ihier-part'('ihier-part'(T1)) --> 'ipath-empty'(T1).


%! iauthority(-ParseTree:compound)// .
% IRI-2.1: IRI authority.
%
% ~~~{.abnf}
% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
% ~~~
%
% If the user info occurs, it is separated from the host with an ampesat.
% If the port occurs, it is separated from the host with a colon.

iauthority(T0) -->
  (iuserinfo(T1), "@" ; ""),
  ihost(T2),
  (":", port(T3) ; ""),
  {parse_tree(iauthority, [T1,T2,T3], T0)}.


%! iuserinfo(-ParseTree:compound)// .
% IRI-2.1.1: User info.
%
% ~~~{.abnf}
% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
% ~~~
%
% This is a difficult DCG rule, since it combines
%  codes (like iunreserved//1) with code lists (like 'pct-encoded'//1).

iuserinfo(iuserinfo(IUserInfo)) -->
  '*'(iuserinfo_, Codes),
  {atom_codes(IUserInfo, Codes)}.
iuserinfo_(C) --> iunreserved(C).
iuserinfo_(C) --> 'pct-encoded'(C).
iuserinfo_(C) --> 'sub-delims'(C).
iuserinfo_(C) --> colon(C).


%! ihost(-ParseTree:compound)// .
% IRI-2.1.2: Host.
%
% A host denotes a physical machine that is connected to the Internet.
%
% ~~~{.abnf}
% ihost = IP-literal / IPv4address / ireg-name
% ~~~

ihost(ihost(T1)) --> 'IP-literal'(T1).
ihost(ihost(T1)) --> 'IPv4address'(T1).
ihost(ihost(T1)) --> 'ireg-name'(T1).


%! 'IP-literal'(-ParseTree:compound)// .
% ~~~{.abnf}
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ~~~

'IP-literal'('IP-literal'(T1)) -->
  bracketed(square,
    'IP-literal_'(T1)
  ).
'IP-literal_'(T1) --> 'IPv6address'(T1).
'IP-literal_'(T1) --> 'IPvFuture'(T1).


%! 'IPv6address'(-ParseTree:compound)// .
% ~~~{.abnf}
% IPv6address =                              6( h16 ":" ) ls32
%               /                       "::" 5( h16 ":" ) ls32
%               / [               h16 ] "::" 4( h16 ":" ) ls32
%               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%               / [ *4( h16 ":" ) h16 ] "::"              ls32
%               / [ *5( h16 ":" ) h16 ] "::"              h16
%               / [ *6( h16 ":" ) h16 ] "::"
% ~~~

'IPv6address'('IPv6address'(Ts)) -->
  '#'(6, h16, Ts1, [separator(colon)]),
  ":",
  ls32(T2),
  {append(Ts1,[T2],Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  "::",
  '#'(5, h16, Ts1, [separator(colon)]),
  ":",
  ls32(T2),
  {append(Ts1,[T2],Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(1, h16, Ts1, [separator(colon)]),
  "::",
  '#'(4, h16, Ts2, [separator(colon)]),
  ":",
  ls32(T3),
  {append([Ts1,Ts2,[T3]], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(2, h16, Ts1, [separator(colon)]),
  "::",
  '#'(3, h16, Ts2, [separator(colon)]),
  ":",
  ls32(T3),
  {append([Ts1,Ts2,[T3]], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(3, h16, Ts1, [separator(colon)]),
  "::",
  '#'(2, h16, Ts2, [separator(colon)]),
  ":",
  ls32(T3),
  {append([Ts1,Ts2,[T3]], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(4, h16, Ts1, [separator(colon)]),
  "::",
  '#'(1, h16, Ts2, [separator(colon)]),
  ":",
  ls32(T3),
  {append([Ts1,Ts2,[T3]], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(5, h16, Ts1, [separator(colon)]),
  "::",
  ls32(T2),
  {append(Ts1, [T2], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(6, h16, Ts1, [separator(colon)]),
  "::",
  h16(T2),
  {append(Ts1, [T2], Ts)}.
'IPv6address'('IPv6address'(Ts)) -->
  '*n'(7, h16, Ts, [separator(colon)]),
  "::".


%! h16(-ParseTree:compound)// .
% 16-bit hexadecimal.
%
% ~~~{.abnf}
% h16 = 1*4HEXDIG
% ~~~

h16(h16(Hex)) -->
  'm*n'(1, 4, 'HEXDIG', Codes, []),
  {atom_codes(Hex, Codes)}.


%! ls32(-ParseTree:compound)// .
% ~~~{.abnf}
% ls32 = ( h16 ":" h16 ) / IPv4address
% ~~~

ls32(ls32(T1,T2)) -->
  h16(T1),
  ":",
  h16(T2).
ls32(ls32(T1)) -->
  'IPv4address'(T1).


%! 'IPv4address'(-ParseTree:compound)// .
% ~~~{.abnf}
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ~~~

'IPv4address'('IPv4address'(T1,T2,T3,T4)) -->
  '#'(4, 'dec-octet', [T1,T2,T3,T4], [separator(dot)]).


%! 'dec-octet'(-ParseTree:compound)// .
% ~~~{.abnf}
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ~~~

% 0-9
'dec-octet'('dec-octet'(N)) -->
  'DIGIT'(_, N).
% 10-99
'dec-octet'('dec-octet'(N)) -->
  between_decimal_digit(1, 9, _, D1),
  'DIGIT'(_, D2),
  {digits_decimal([D1,D2], N)}.
% 100-199
'dec-octet'('dec-octet'(N)) -->
  "1",
  '+'('DIGIT', [D2,D3], []),
  {digits_decimal([1,D2,D3], N)}.
% 200-249
'dec-octet'('dec-octet'(N)) -->
  "2",
  between_decimal_digit(0, 4, _, D2),
  'DIGIT'(_, D3),
  {digits_decimal([2,D2,D3], N)}.
% 250-255
'dec-octet'('dec-octet'(N)) -->
  "25",
  between_decimal_digit(0, 5, _, D3),
  {digits_decimal([2,5,D3], N)}.


%! 'IPvFuture'(-ParseTree:compound)// .
% ~~~{.abnf}
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ~~~

'IPvFuture'('IPvFuture'(major(T1),minor(T2))) -->
  "v",
  '+'('HEXDIG', Codes1, []),
  ".",
  '+'('IPvFuture_', Codes2, []),
  {
    atom_codes(T1, Codes1),
    atom_codes(T2, Codes2)
  }.
'IPvFuture_'(C) --> unreserved(C).
'IPvFuture_'(C) --> 'sub-delims'(C).
'IPvFuture_'(C) --> colon(C).


%! 'ireg-name'(?IRegName:atom)// .
% ~~~{.abnf}
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ~~~

'ireg-name'('ireg-name'(Atom)) -->
  '*'('ireg-name_', Codes, []),
  {atom_codes(Atom, Codes)}.
'ireg-name_'(C) --> iunreserved(C).
'ireg-name_'(C) --> 'pct-encoded'(C).
'ireg-name_'(C) --> 'sub-delims'(C).


%! 'ipath-abempty'// .
% ~~~{.abnf}
% ipath-abempty = *( "/" isegment )
% ~~~

'ipath-abempty'('ipath-abempty'(Segments)) -->
  '*'(forwardslash_segment, Segments, []).
forwardslash_segment(Segment) -->
  "/",
  isegment(Segment).


%! isegment(-ParseTree:compound)// .
% ~~~{.abnf}
% isegment = *ipchar
% ~~~

isegment(isegment(Segment)) -->
  '*'(ipchar, Codes, []),
  {atom_codes(Segment, Codes)}.


%! 'ipath-absolute'// .
% ~~~{.abnf}
% ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
% ~~~

'ipath-absolute'(T0) -->
  "/",
  (
    'isegment-nz'(T1),
    '*'(forwardslash_segment, Ts, [])
  ;
    ""
  ),
  {parse_tree('ipath-absolute', [T1|Ts], T0)}.


%! 'isegment-nz'(?Segment:atom)// .
% ~~~{.abnf}
% isegment-nz = 1*ipchar
% ~~~

'isegment-nz'('isegment-nz'(Segment)) -->
  '+'(ipchar, Codes, []),
  {atom_codes(Segment, Codes)}.


%! 'ipath-rootless'(?Segments:list(atom))// .
% ~~~{.abnf}
% ipath-rootless = isegment-nz *( "/" isegment )
% ~~~

'ipath-rootless'('ipath-rootless'([H|T])) -->
  'isegment-nz'(H),
   '*'(forwardslash_segment, T, []).


%! 'ipath-empty'
% ~~~{.abnf}
% ipath-empty = 0<ipchar>
% ~~~

'ipath-empty'('ipath-empty') --> [].


%! post(?Port:nonneg)// .
% IRI-2.1.3: Port.
%
% ~~~{.abnf}
% port = *DIGIT
% ~~~

port(port(Port)) -->
  '*'('DIGIT', Codes, _, []),
  {number_codes(Port, Codes)}.


%! iquery(?Query:atom)// .
% IRI-2.2: Query.
%
% ~~~{.abnf}
% iquery = *( ipchar / iprivate / "/" / "?" )
% ~~~

iquery(iquery(Query)) -->
  '*'(iquery_, Codes, []),
  {atom_codes(Query, Codes)}.
iquery_(C) --> ipchar(C).
iquery_(C) --> iprivate(C).
iquery_(C) --> forward_slash(C).
iquery_(C) --> question_mark(C).


%! ifragment(?IFragment:atom)// .
% IRI-2.3: Fragment identifier.
%
% ~~~{.abnf}
% ifragment = *( ipchar / "/" / "?" )
% ~~~

ifragment(ifragment(IFragment)) -->
  '*'(ifragment_, Codes, []),
  {atom_codes(IFragment, Codes)}.
ifragment_(C) --> ipchar(C).
ifragment_(C) --> forward_slash(C).
ifragment_(C) --> question_mark(C).


%! 'irelative-part'(-ParseTree:compound)// .
% Relative IRI.
%
% ~~~{.abnf}
% irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
% ~~~

'irelative-ref'(T0) -->
  'irelative-part'(T1),
  ("?", iquery(T2) ; ""),
  ("#", ifragment(T3) ; ""),
  {parse_tree('irelative-ref', [T1,T2,T3], T0)}.


%! 'irelative-part'(-ParseTree:compound)// .
% ~~~{.abnf}
% irelative-part = "//" iauthority ipath-abempty
%                / ipath-absolute
%                / ipath-noscheme
%                / ipath-empty
% ~~~

'irelative-part'('irelative-part'(T1,T2)) -->
  forward_slash, forward_slash,
  iauthority(T1),
  'ipath-abempty'(T2).
'irelative-part'('irelative-part'(T1)) --> 'ipath-absolute'(T1).
'irelative-part'('irelative-part'(T1)) --> 'ipath-noscheme'(T1).
'irelative-part'('irelative-part'(T1)) --> 'ipath-empty'(T1).


%! 'ipath-noscheme'(-ParseTree:compound)// .
% ~~~{.abnf}
% ipath-noscheme = isegment-nz-nc *( "/" isegment )
% ~~~

'ipath-noscheme'('ipath-noscheme'([H|T])) -->
  'isegment-nz-nc'(H),
  '*'(forwardslash_segment, T, []).

%! 'isegment-nz-nc'(?Segment:atom)// .
% Non-zero-length segment without any colon ":".
%
% ~~~{.abnf}
% isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
%                ; non-zero-length segment without any colon ":"
% ~~~

'isegment-nz-nc'('isegment-nz-nc'(Segment)) -->
  '+'('isegment-nz-nc_', Codes, []),
  {atom_codes(Segment, Codes)}.
'isegment-nz-nc_'(C) --> iunreserved(C).
'isegment-nz-nc_'(C) --> 'pct-encoded'(C).
'isegment-nz-nc_'(C) --> 'sub-delims'(C).
'isegment-nz-nc_'(C) --> at_sign(C).



% CHARACTERS %

%! ipchar(?Code:code)// .
% ~~~{.abnf}
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ~~~

ipchar(C) --> iunreserved(C).
ipchar(C) --> 'pct-encoded'(C).
ipchar(C) --> 'sub-delims'(C).
ipchar(C) --> colon(C).
ipchar(C) --> at_symbol(C).


%! iprivate(?Code:code)// .
% ~~~{.abnf}
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ~~~

iprivate(C) -->
  [C],
  {( between_hex('E000',   'F8FF',   C), !
  ;  between_hex('F0000',  'FFFFD',  C), !
  ;  between_hex('100000', '10FFFD', C)
  )}.


%! iunreserved(?Code:code)// .
% ~~~{.abnf}
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ~~~

iunreserved(C) --> 'ALPHA'(C).
iunreserved(C) --> 'DIGIT'(C, _).
iunreserved(C) --> hyphen(C).
iunreserved(C) --> dot(C).
iunreserved(C) --> underscore(C).
iunreserved(C) --> tilde(C).
iunreserved(C) --> ucschar(C).


%! 'pct-encoded'(?Code:code)// .
% ~~~{.abnf}
% pct-encoded = "%" HEXDIG HEXDIG
% ~~~

'pct-encoded'(C) -->
  "%",
  'HEXDIG'(H1),
  'HEXDIG'(H2),
  {digits_decimal([H1,H2], 16, C)}.


%! 'sub-delims'(?Code:code)// .
% ~~~{.abnf}
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
% ~~~

'sub-delims'(C) --> exclamation_mark(C).
'sub-delims'(C) --> dollar_sign(C).
'sub-delims'(C) --> ampersand(C).
'sub-delims'(C) --> single_quote(C).
'sub-delims'(C) --> round_bracket(C).
'sub-delims'(C) --> asterisk(C).
'sub-delims'(C) --> plus_sign(C).
'sub-delims'(C) --> comma(C).
'sub-delims'(C) --> semi_colon(C).
'sub-delims'(C) --> equals_sign(C).


%! ucschar(?Code:code)// .
% ~~~{.abnf}
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ~~~

ucschar(C) -->
  [C],
  {( between_hex('A0',    'D7FF',  C), !
  ;  between_hex('F900',  'FDCF',  C), !
  ;  between_hex('FDF0',  'FFEF',  C), !
  ;  between_hex('10000', '1FFFD', C), !
  ;  between_hex('20000', '2FFFD', C), !
  ;  between_hex('30000', '3FFFD', C), !
  ;  between_hex('40000', '4FFFD', C), !
  ;  between_hex('50000', '5FFFD', C), !
  ;  between_hex('60000', '6FFFD', C), !
  ;  between_hex('70000', '7FFFD', C), !
  ;  between_hex('80000', '8FFFD', C), !
  ;  between_hex('90000', '9FFFD', C), !
  ;  between_hex('A0000', 'AFFFD', C), !
  ;  between_hex('B0000', 'BFFFD', C), !
  ;  between_hex('C0000', 'CFFFD', C), !
  ;  between_hex('D0000', 'DFFFD', C), !
  ;  between_hex('E1000', 'EFFFD', C)
  )}.


%! unreserved(?Code:code)// .

unreserved(C) --> 'ALPHA'(C).
unreserved(C) --> 'DIGIT'(C, _).
unreserved(C) --> hyphen(C).
unreserved(C) --> dot(C).
unreserved(C) --> underscore(C).
unreserved(C) --> tilde(C).



% EXTRA RULES %

%! 'absolute-IRI'(-ParseTree:compound)// .
% ~~~{.abnf}
% absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]
% ~~~

'absolute-IRI'(T0) -->
  scheme(T1),
  ":",
  'ihier-part'(T2),
  (
    "?",
    iquery(T3)
  ;
    ""
  ),
  {parse_tree('absolute-IRI', [T1,T2,T3], T0)}.


%! 'gen-delims'(?Code:code)// .
% ~~~{.abnf}
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ~~~

'gen-delims'(C) --> colon(C).
'gen-delims'(C) --> forward_slash(C).
'gen-delims'(C) --> question_mark(C).
'gen-delims'(C) --> number_sign(C).
'gen-delims'(C) --> square_bracket(C).
'gen-delims'(C) --> at_sign(C).


%! ipath(-ParseTree:compound)// .
% ~~~{.abnf}
% ipath = ipath-abempty    ; begins with "/" or is empty
%       / ipath-absolute   ; begins with "/" but not "//"
%       / ipath-noscheme   ; begins with a non-colon segment
%       / ipath-rootless   ; begins with a segment
%       / ipath-empty      ; zero characters
% ~~~

% Begins with "/" or is empty.
ipath(ipath(T1)) --> 'ipath-abempty'(T1).
% Begins with "/" but not "//".
ipath(ipath(T1)) --> 'ipath-absolute'(T1).
% Begins with a non-colon segment
ipath(ipath(T1)) --> 'ipath-noscheme'(T1).
% Begins with a segment
ipath(ipath(T1)) --> 'ipath-rootless'(T1).
% Zero characters.
ipath(ipath(T1)) --> 'ipath-empty'(T1).


%! reserved(?Code:code)// .

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).

