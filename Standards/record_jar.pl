:- module(
  record_jar,
  [
    'record-jar'//2 % ?Encoding:atom
                    % ?Records:list(list(nvpair))
  ]
).

/** <module> RECORD_JAR

Support for the =|record-jar|= format for storing multiple records with a
variable repertoire of fields in a text format.

## Syntax

~~~{.abnf}
record-jar   = [encodingSig] [separator] *record
record       = 1*field separator
field        = ( field-name field-sep field-body CRLF )
field-name   = 1*character
field-sep    = *SP ":" *SP
field-body   = *(continuation 1*character)
continuation = ["\"] [[*SP CRLF] 1*SP]
separator    = [blank-line] *("%%" [comment] CRLF)
comment      = SP *69(character)
character    = SP / ASCCHAR / UNICHAR / ESCAPE
encodingSig  = "%%encoding" field-sep
                *(ALPHA / DIGIT / "-" / "_") CRLF
blank-line   = WSP CRLF

; ASCII characters except %x26 (&) and %x5C (\)
ASCCHAR      = %x21-25 / %x27-5B / %x5D-7E
; Unicode characters
UNICHAR      = %x80-10FFFF
ESCAPE       = "\" ("\" / "&" / "r" / "n" / "t" )
            / "&#x" 2*6HEXDIG ";"
~~~

@author Wouter Beek
@see Originally described in *The Art of Unix Programming*.
@see Latest description was found at
     http://tools.ietf.org/html/draft-phillips-record-jar-02
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(math(radix)).
:- use_module(standards(abnf)).



%! 'ASCCHAR'(?Code:code)//
% ASCII characters except %x26 (&) and %x5C (\).
%
% ~~~{.abnf}
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ~~~

'ASCCHAR'(C) -->
  [C],
  {(between(33, 37, C)
  ; between(39, 91, C)
  ; between(93, 126, C)
  )}.

%! 'blank-line'//
%
% ~~~{.abnf}
% blank-line = WSP CRLF
% ~~~

'blank-line' -->
  'WSP',
  'CRLF'.

%! character(?Code:code)//
%
% ~~~{.abnf}
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ~~~
%
% Note that ampersand// and backslash// are explicitly excluded.

character(C) -->
  'ASCCHAR'(C).
character(C) -->
  'SP'(C).
character(C) -->
  'ESCAPE'(C).
character(C) -->
  'UNICHAR'(C).

%! comment(?Comment:atom)//
%
% ~~~{.abnf}
% comment = SP *69(character)
% ~~~

comment(Comment) -->
  'SP',
  dcg_multi(character, between(0,69), Codes),
  {atom_codes(Comment, Codes)}.

%! continuation//
%
% ~~~{.abnf}
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ~~~

continuation -->
  (backslash ; ""),
  (
    (dcg_multi('SP'), 'CRLF' ; ""),
    dcg_multi('SP', between(1,_))
  ;
    ""
  ).

%! encodingSig(?Encoding:atom)//
%
% ~~~{.abnf}
% encodingSig  = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ~~~

encodingSig(Encoding) -->
  "%%encoding",
  'field-sep',
  dcg_multi(('ALPHA' ; 'DIGIT' ; hyphen_minus ; underscore), _N, Codes),
  {atom_codes(Encoding, Codes)},
  'CRLF'.

%! 'ESCAPE'(?Code:code)//
%
% ~~~{.abnf}
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ~~~

'ESCAPE'(C) -->
  backslash,
  ( backslash(C)
  ; ampersat(C)
  ; r_lowercase(C)
  ; n_lowercase(C)
  ; t_lowercase(C)
  ).
'ESCAPE'(C) -->
  "&#x",
  dcg_multi('HEXDIG', between(2,6), DecimalDigits, _Codes),
  {digits_to_decimal(DecimalDigits, C)}.

%! field(?Field:nvpair)//
% ~~~{.abnf}
% field = ( field-name field-sep field-body CRLF )
% ~~~

field(Name=Body) -->
  'field-name'(Name),
  'field-sep',
  'field-body'(Body),
  'CRLF'.

%! 'field-body'(?Body:list(atom))//
%
% ~~~{.abnf}
% field-body   = *(continuation 1*character)
% ~~~

'field-body'(Body) -->
  dcg_multi((continuation, dcg_multi(character, _N, CodeLists)), _M, Codes),
  {write(CodeLists)}, %DEB
  {write(Codes)}, %DEB
  {maplist(atom_codes, Body, CodeLists)}.

%! 'field-name'(-Tree:compound, ?Name:atom)//
%
% ~~~{.abnf}
% field-name = 1*character
% ~~~

'field-name'(Name) -->
  dcg_multi(character, between(1,_), Codes),
  {atom_codes(Name, Codes)}.

%! 'field-sep'//
%
% ~~~{.abnf}
% field-sep = *SP ":" *SP
% ~~~

'field-sep' -->
  dcg_multi('SP'),
  ":",
  dcg_multi('SP').

%! 'record-jar'(?Encoding:atom, ?Records:list(list(nvpair)))//
%
% ~~~{.abnf}
% record-jar = [encodingSig] [separator] *record
% ~~~

'record-jar'(Encoding, Records) -->
  (encodingSig(Encoding) ; ""),
  (separator(_Comments) ; ""),
  dcg_multi(record, _N, Records).

%! record(?Fields:list(nvpair))//
% ~~~{.abnf}
% record = 1*field separator
% ~~~

record(Fields) -->
  dcg_multi(field, between(1,_), Fields),
  separator(_Comments).

%! separator(?Comments:list(atom))//
%
% ~~~{.abnf}
% separator = [blank-line] *("%%" [comment] CRLF)
% ~~~

separator(Comments2) -->
  ('blank-line' ; ""),
  dcg_multi(separator_, _N, Comments1),
  {exclude(var, Comments1, Comments2)}.
separator_(Comment) -->
  "%%",
  (comment(Comment) ; ""),
  'CRLF'.

%! 'UNICHAR'(?Code:code)//
%
% ~~~{.abnf}
% UNICHAR = %x80-10FFFF
% ~~~

'UNICHAR'(C) -->
  [C],
  {between(128, 1114111, C)}.

