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
@version 2013/09, 2014/01, 2014/10
*/

:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)). % Used in meta-options.
:- use_module(plDcg(dcg_cardinal)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(parse_tree)).
:- use_module(plDcg_flp(rfc4234_basic)).



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
scheme_(Code) --> 'ALPHA'(Code).
scheme_(Code) --> 'DIGIT'(_, Code).
scheme_(Code) --> plus_sign(Code).
scheme_(Code) --> minus_sign(Code).
scheme_(Code) --> dot(Code).





% HIERARCHICAL PART %

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
iuserinfo_(Code) --> iunreserved(Code).
iuserinfo_(Code) --> 'pct-encoded'(Code).
iuserinfo_(Code) --> 'sub-delims'(Code).
iuserinfo_(Code) --> colon(Code).



%! 'IP-literal'(-ParseTree:compound)// .
% ~~~{.abnf}
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ~~~

'IP-literal'('IP-literal'(T1)) -->
  bracketed(square, 'IP-literal_'(T1)).
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

h16(h16(Number)) -->
  'm*n'(1, 4, 'HEXDIG', Weights, []),
  {weights_radix(Weights, dec(Number))}.



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
'dec-octet'('dec-octet'(Number)) -->
  'DIGIT'(Number, _).
% 10-99
'dec-octet'('dec-octet'(Number)) -->
  between_digit(1, 9, D1),
  'DIGIT'(D2),
  {weights_radix([D1,D2], Number)}.
% 100-199
'dec-octet'('dec-octet'(Number)) -->
  "1",
  '#'(2, 'DIGIT', [D2,D3], []),
  {weights_radix([1,D2,D3], Number)}.
% 200-249
'dec-octet'('dec-octet'(Number)) -->
  "2",
  between_digit(0, 4, D2),
  'DIGIT'(D3),
  {weights_radix([2,D2,D3], Number)}.
% 250-255
'dec-octet'('dec-octet'(Number)) -->
  "25",
  between_digit(0, 5, D3),
  {weights_radix([2,5,D3], Number)}.



%! 'IPvFuture'(-ParseTree:compound)// .
% ~~~{.abnf}
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ~~~

'IPvFuture'('IPvFuture'(major(T1),minor(T2))) -->
  "v",
  '+'('HEXDIG', _, Codes1, []),
  ".",
  '+'('IPvFuture_', Codes2, []),
  {
    atom_codes(T1, Codes1),
    atom_codes(T2, Codes2)
  }.
'IPvFuture_'(Code) --> unreserved(Code).
'IPvFuture_'(Code) --> 'sub-delims'(Code).
'IPvFuture_'(Code) --> colon(Code).



%! 'ireg-name'(?IRegName:atom)// .
% ~~~{.abnf}
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ~~~

'ireg-name'('ireg-name'(Atom)) -->
  '*'('ireg-name_', Codes, []),
  {atom_codes(Atom, Codes)}.
'ireg-name_'(Code) --> iunreserved(Code).
'ireg-name_'(Code) --> 'pct-encoded'(Code).
'ireg-name_'(Code) --> 'sub-delims'(Code).



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
  (   'isegment-nz'(T1),
      '*'(forwardslash_segment, Ts, []), !
  ;   ""
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
  '*'('DIGIT', _, Codes, []),
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
iquery_(Code) --> ipchar(Code).
iquery_(Code) --> iprivate(Code).
iquery_(Code) --> forward_slash(Code).
iquery_(Code) --> question_mark(Code).



%! ifragment(?IFragment:atom)// .
% IRI-2.3: Fragment identifier.
%
% ~~~{.abnf}
% ifragment = *( ipchar / "/" / "?" )
% ~~~

ifragment(ifragment(IFragment)) -->
  '*'(ifragment_, Codes, []),
  {atom_codes(IFragment, Codes)}.
ifragment_(Code) --> ipchar(Code).
ifragment_(Code) --> forward_slash(Code).
ifragment_(Code) --> question_mark(Code).



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
  "//",
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
'isegment-nz-nc_'(Code) --> iunreserved(Code).
'isegment-nz-nc_'(Code) --> 'pct-encoded'(Code).
'isegment-nz-nc_'(Code) --> 'sub-delims'(Code).
'isegment-nz-nc_'(Code) --> at_sign(Code).





% CHARACTERS %

%! ipchar(?Code:code)// .
% ~~~{.abnf}
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ~~~

ipchar(Code) --> iunreserved(Code).
ipchar(Code) --> 'pct-encoded'(Code).
ipchar(Code) --> 'sub-delims'(Code).
ipchar(Code) --> colon(Code).
ipchar(Code) --> at_symbol(Code).



%! iprivate(?Code:code)// .
% ~~~{.abnf}
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ~~~

iprivate(Code) -->
  [Code],
  {( between_code(hex('E000'),   hex('F8FF'),   Code), !
  ;  between_code(hex('F0000'),  hex('FFFFD'),  Code), !
  ;  between_code(hex('100000'), hex('10FFFD'), Code)
  )}.



%! iunreserved(?Code:code)// .
% ~~~{.abnf}
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ~~~

iunreserved(Code) --> 'ALPHA'(Code).
iunreserved(Code) --> 'DIGIT'(_, Code).
iunreserved(Code) --> hyphen(Code).
iunreserved(Code) --> dot(Code).
iunreserved(Code) --> underscore(Code).
iunreserved(Code) --> tilde(Code).
iunreserved(Code) --> ucschar(Code).



%! 'pct-encoded'(?Code:code)// .
% ~~~{.abnf}
% pct-encoded = "%" HEXDIG HEXDIG
% ~~~

'pct-encoded'(Code) -->
  "%",
  '#'(2, 'HEXDIG', _, Codes, []),
  {number_codes(Code, Codes)}.



%! 'sub-delims'(?Code:code)// .
% ~~~{.abnf}
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
% ~~~

'sub-delims'(Code) --> exclamation_mark(Code).
'sub-delims'(Code) --> dollar_sign(Code).
'sub-delims'(Code) --> ampersand(Code).
'sub-delims'(Code) --> single_quote(Code).
'sub-delims'(Code) --> round_bracket(Code).
'sub-delims'(Code) --> asterisk(Code).
'sub-delims'(Code) --> plus_sign(Code).
'sub-delims'(Code) --> comma(Code).
'sub-delims'(Code) --> semi_colon(Code).
'sub-delims'(Code) --> equals_sign(Code).



%! ucschar(?Code:code)// .
% ~~~{.abnf}
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ~~~

ucschar(Code) -->
  [Code],
  {( between_code(hex('A0'),    hex('D7FF'),  Code), !
  ;  between_code(hex('F900'),  hex('FDCF'),  Code), !
  ;  between_code(hex('FDF0'),  hex('FFEF'),  Code), !
  ;  between_code(hex('10000'), hex('1FFFD'), Code), !
  ;  between_code(hex('20000'), hex('2FFFD'), Code), !
  ;  between_code(hex('30000'), hex('3FFFD'), Code), !
  ;  between_code(hex('40000'), hex('4FFFD'), Code), !
  ;  between_code(hex('50000'), hex('5FFFD'), Code), !
  ;  between_code(hex('60000'), hex('6FFFD'), Code), !
  ;  between_code(hex('70000'), hex('7FFFD'), Code), !
  ;  between_code(hex('80000'), hex('8FFFD'), Code), !
  ;  between_code(hex('90000'), hex('9FFFD'), Code), !
  ;  between_code(hex('A0000'), hex('AFFFD'), Code), !
  ;  between_code(hex('B0000'), hex('BFFFD'), Code), !
  ;  between_code(hex('C0000'), hex('CFFFD'), Code), !
  ;  between_code(hex('D0000'), hex('DFFFD'), Code), !
  ;  between_code(hex('E1000'), hex('EFFFD'), Code)
  )}.



%! unreserved(?Code:code)// .

unreserved(Code) --> 'ALPHA'(Code).
unreserved(Code) --> 'DIGIT'(_, Code).
unreserved(Code) --> hyphen(Code).
unreserved(Code) --> dot(Code).
unreserved(Code) --> underscore(Code).
unreserved(Code) --> tilde(Code).





% EXTRA RULES %

%! 'absolute-IRI'(-ParseTree:compound)// .
% ~~~{.abnf}
% absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]
% ~~~

'absolute-IRI'(T0) -->
  scheme(T1),
  ":",
  'ihier-part'(T2),
  (   "?",
      iquery(T3)
  ;   ""
  ),
  {parse_tree('absolute-IRI', [T1,T2,T3], T0)}.



%! 'gen-delims'(?Code:code)// .
% ~~~{.abnf}
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ~~~

'gen-delims'(Code) --> colon(Code).
'gen-delims'(Code) --> forward_slash(Code).
'gen-delims'(Code) --> question_mark(Code).
'gen-delims'(Code) --> number_sign(Code).
'gen-delims'(Code) --> square_bracket(Code).
'gen-delims'(Code) --> at_sign(Code).



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

reserved(Code) --> 'gen-delims'(Code).
reserved(Code) --> 'sub-delims'(Code).
