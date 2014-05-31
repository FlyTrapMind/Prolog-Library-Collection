:- module(
  record_jar,
  [
    'record-jar'//3 % -Tree:compound
                    % ?Encoding:atom
                    % ?Records:list(list(nvpair(atom,list(atom))))
  ]
).

/** <module> Record Jar

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
@version 2013/07, 2014/05-2014/06
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(parse_tree)).
:- use_module(flp(record_jar_char)).
:- use_module(flp(rfc4234_basic)).
:- use_module(math(radix)).



%! 'blank-line'(-Tree:compound)// .
% ~~~{.abnf}
% blank-line = WSP CRLF
% ~~~

'blank-line'('blank-line'('WSP','CRLF')) -->
  'WSP',
  'CRLF'.


%! comment(-Tree:compound, ?Comment:list(code))// .
% ~~~{.abnf}
% comment = SP *69(character)
% ~~~
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

comment(comment(T1), Comment) -->
  dcg_atom_codes_pt(comment1, T1, Comment).

comment1(['WSP'|Ts], Comment) -->
  'WSP',
  '*n'(69, character, Ts, Comment).


%! continuation(-Tree:compound)// .
% ~~~{.abnf}
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ~~~
%
% ### Suggestion
%
% I would allow the horizontal tab to occur in comments,
% for consistency with ???.

continuation(continuation) -->
  '?'(backslash),
  '?'(continuation1).

continuation1 -->
  '?'(continuation2),
  '+'('SP').

continuation2 -->
  '*'('SP'),
  'CRLF'.


%! encodingSig(-Tree:compound, ?Encoding:atom)// .
% ~~~{.abnf}
% encodingSig  = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ~~~

encodingSig(encodingSig('%%encoding',T1,T2,'CRLF'), Encoding) -->
  `%%encoding`,
  'field-sep'(T1),
  dcg_atom_codes_pt(encodingSig1, T2, Encoding),
  'CRLF'.

encodingSig1(Ts, Codes) -->
  '*'(encodingSig2, Ts, Codes).

encodingSig2('ALPHA'(Char), Code) -->
  'ALPHA'(Code),
  {char_code(Char, Code)}.
encodingSig2('DIGIT'(Char), Code) -->
  'DIGIT'(Code),
  {char_code(Char, Code)}.
encodingSig2('-', Code) -->
  hyphen(Code).
encodingSig2('_', Code) -->
  underscore(Code).


%! field(-Tree:compound, ?Field:nvpair(atom,list(atom)))// .
% ~~~{.abnf}
% field = ( field-name field-sep field-body CRLF )
% ~~~

field(T, Name=Body) -->
  'field-name'(T1, Name),
  'field-sep'(T2),
  'field-body'(T3, Body),
  'CRLF',
  {parse_tree(field, [T1,T2,T3], T)}.


%! 'field-body'(-Tree:compound, ?Body:list(atom))// .
% The field-body contains the data value. Logically, the field-body
% consists of a single line of text using any combination of characters
% from the Universal Character Set followed by a `CRLF` (newline).
%
% ### Escaping
%
% The carriage return, newline, and tab characters, when they occur in the
% data value stored in the field-body, are represented by their common
% backslash escapes (=\r=, =\n=, and =\t= respectively).
%
% ~~~{.abnf}
% field-body   = *(continuation 1*character)
% ~~~
%
% ### Folding
%
% To accommodate line limits (enforced by another standard or implementation),
% readability, and presentational purposes, the field-body portion of a field
% can be split into a multi-line representation; this is called *folding*.
%
% @tbd It is RECOMMENDED that folding not occur between characters inside a
%      Unicode grapheme cluster (since this will alter the display of
%      characters in the file and might result in unintentional alteration
%      of the file's semantics).
% @see Information on grapheme clusters, UAX29.

'field-body'('field-body'(Tss), Body) -->
  '*'('field-body1', Tss, Body).

'field-body1'([T1|Ts], Word) -->
  continuation(T1),
  dcg_atom_codes_pt('field-body2', Ts, Word).

'field-body2'(Ts, Codes) -->
  '+'(character, Ts, Codes).


%! 'field-name'(-Tree:compound, ?Name:atom)// .
% The field-name is an identifer. Field-names consist of a sequence of
% Unicode characters. Whitespace characters and colon (=:=, =%x3A=) are
% not permitted in a field-name.
%
% ### Case
%
% Field-names are case sensitive.  Upper and lowercase letters are
% often used to visually break up the name, for example using
% CamelCase.  It is a common convention that field names use an initial
% capital letter, although this is not enforced.
%
% ~~~{.abnf}
% field-name = 1*character
% ~~~
%
% ### Inconsistency
%
% The ABNF seems to be wrong. The working draft states the following:
% ~~~{.txt}
% Whitespace characters and colon (":", %x3A) are not permitted in a
% field-name.
% ~~~
% We therefore introduce the extra DCG rule 'field-name-character'//1.

'field-name'('field-name'(T), Name) -->
  dcg_atom_codes_pt('field-name1', T, Name).

'field-name1'(Ts, Name) -->
  '+'('field-name-character', Ts, Name).


%! 'field-sep'(-Tree:compound)// .
% The field separator is the colon character (=:=, =%x3A=).
% The separator MAY be surrounded on either side by any amount of
% horizontal whitespace (tab or space characters). The normal
% convention is one space on each side.
%
% ~~~{.abnf}
% field-sep = *SP ":" *SP
% ~~~
%
% ### Inconsistency
%
% The ABNF does not mention the (horizontal) tab, only the space character.
% We solve this by using 'WSP'// instead of 'SP'//.

'field-sep'('field-sep'('*WSP',':','*WSP')) -->
  '*'('WSP'),
  `:`,
  '*'('WSP').


%! 'record-jar'(
%!   -Tree:compound,
%!   ?Encoding:atom,
%!   ?Records:list(list(nvpair(atom,list(atom))))
%! )// .
% ~~~{.abnf}
% record-jar = [encodingSig] [separator] *record
% ~~~

'record-jar'(T, Encoding, Records) -->
  '?'(encodingSig(T1, Encoding)),
  % The disjunction with the empty string is not needed here,
  % since the production of the separator can process
  % the empty string as well.
  '?'(separator, T2, _),
  '*'(record, Ts, Records),
  {parse_tree('record-jar', [T1,T2|Ts], T)}.


%! record(-Tree:compound, ?Fields:list(nvpair(atom,list(atom))))// .
% ~~~{.abnf}
% record = 1*field separator
% ~~~

record(record(T), Fields) -->
  '+'(field, Ts, Fields),
  separator(T1, _),
  {append(Ts,T1,T)}.


%! separator(-Tree:compound, ?Comments:list(atom))// .
% ~~~{.abnf}
% separator = [blank-line] *("%%" [comment] CRLF)
% ~~~

separator(T, Comments) -->
  '?'('blank-line', T1),
  '*'(separator1, Ts, Comments),
  {parse_tree(separator, [T1|Ts], T)}.

separator1(['%%',T1,'CRLF'], Comment) -->
  `%%`,
  '?'(dcg_atom_codes_pt(comment), T1, Comment),
  'CRLF'.



%:- begin_tests(record_jar, []).
:- begin_tests(record_jar, [blocked('Takes too long to run each time.')]).

:- use_module(library(apply)).
:- use_module(library(pio)).

user:prolog_file_type(txt, text).

test(record_jar, []):-
  absolute_file_name(
    lang(rfc5646_iana_registry),
    File,
    [access(read),file_type(text)]
  ),
  setup_call_cleanup(
    open(File, read, Stream, [type(binary)]),
    once(phrase_from_stream('record-jar'(E, Rs), Stream)),
    close(Stream)
  ),
  maplist(formatnl, [E|Rs]).

:- end_tests(record_jar).

