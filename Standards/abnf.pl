:- module(
  abnf,
  [
    'ALPHA'//0,
    'ALPHA'//1,
    'BIT'//0,
    'BIT'//1, % ?Code:code
    'BIT'//2, % ?Code:code
              % ?DecimalDigit:between(0,1)
    'CHAR'//0,
    'CHAR'//1,
    'CR'//0,
    'CR'//1,
    'CRLF'//0,
    'CRLF'//1, % ?Codes:list(code)
    'CTL'//0,
    'CTL'//1,
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?Code:code
                % ?DecimalDigit:between(0,9)
    'DQUOTE'//0,
    'DQUOTE'//1,
    'HEXDIG'//0,
    'HEXDIG'//1, % ?Code:code
    'HEXDIG'//2, % ?Code:code
                 % ?DecimalDigit:between(0,15)
    'HTAB'//0,
    'HTAB'//1,
    'LF'//0,
    'LF'//1,
    'LWSP'//0,
    'LWSP'//1,
    'OCTET'//0,
    'OCTET'//1,
    'SP'//0,
    'SP'//1,
    'VCHAR'//0,
    'VCHAR'//1,
    'WSP'//0,
    'WSP'//1
  ]
).

/** <module> ABNF

Support for Augmented Backus-Naur Format (ABNF).

## ABNF & BNF

Differences between ABNF and standard BNF:
  * Alternatives
  * Angle brackets
    BNF requires angle brackets around rule names. ABNF allows this.
  * Naming rules
  * Order-independence
  * Repetition
  * Value ranges

## Rule names

Rule names are case insensitive.

Angle brackets around rule names may be used around a rule name whenever their
presence facilitates in discerning the use of a rule name.
This is typically restricted to rule name references in free-form prose, or
to distinguish partial rules that combine into a string not separated by
white space (see repetition).

## Rule form

~~~
name = elements crlf
~~~

For visual ease, rule definitions are left aligned.
When a rule requires multiple lines, the continuation lines are indented.
The left alignment and indentation are relative to the first lines of the
ABNF rules and need not match the left margin of the document.

# Terminal values

Rules resolve into a string of terminal values, sometimes called characters.
A character is a non-negative integer.
A specific mapping (encoding) of values into a character can be specified.

### Single terminal value

Bases:
  * =b=
    binary
  * =d=
    decimal
  * =x=
    hexadecimal

Examples:
~~~{.abnf}
CR = %d13
CR = %x0D
~~~

### Multiple terminal values

A concatenated string of such values is specified compactly, using a period
to separate the characters.

Example:
~~~{.abnf}
CRLF = %d13.10
~~~

Alternatively, a literal string that consists of US-ASCII characters only,
can be given directly. Such strings are case insensitive.

Example:
~~~{.abnf}
command = "command string"
~~~

### External encodings

External representations of terminal value characters will vary according to
constraints in the storage or transmission environment.

By separating external encoding from the syntax, it is intended that
alternate encoding environments can be used for the same syntax.

## Operations

### Concatenation

Space-separated elements.

### Alternatives

Forward-slash separated elements.

Example:
~~~{.abnf}
ruleset = alt1 / alt2 / alt3 / alt4 / alt5
~~~

### Incremental alternatives

It is sometimes convenient to specify a list of alternatives in fragments.
This is particularly useful for otherwise independent specifications that
derive from the same parent ruleset.

Format:
~~~
oldrule =/ additional-alternatives
~~~

Example:
~~~{.abnf}
ruleset =  alt1 / alt2
ruleset =/ alt3
ruleset =/ alt4 / alt5
~~~

### Value range alternatives

A range of alternative numeric values can be specified compactly,
using a dash to indicate the range of alternative values.

Example:
~~~{.abnf}
DIGIT = %x30-39
~~~

Concatenated numeric values and numeric value ranges cannot be specified in
the same string.

Example:
~~~{.abnf}
char-line = %x0D.0A %x20-7E %x0D.0A
~~~

### Sequence Group

Elements enclosed in parentheses are treated as a single element,
whose contents are strictly ordered.

The sequence group notation is also used within free text to set off
an element sequence from the prose.

### Variable Repetition:

The operator "=*=" preceding an element indicates repetition.
The full form is:
~~~
<a>*<b>element
~~~
where =|<a>|= and =|<b>|= are optional decimal values, indicating the least
and the most occurrences of the element.
Default values are =0= and =infinity=.

### Specific Repetition:

Rules of the form
~~~
<n>*<n>element
~~~
can be abbreviated with rules of the form:
~~~
<n>element
~~~

### Optional Sequence

Rules of the form:
~~~
*1(foo bar)
~~~
can be abbreviated by rules of the form:
~~~
[foo bar]
~~~

### Comment

A semicolon starts a comment that continues to the end of line.

### Precedence

From binding tightest to binding loosest:
  * Rule name, prose-val, Terminal value
  * Comment
  * Value range
  * Repetition
  * Grouping, Optional
  * Concatenation
  * Alternative

For example, alternatives are looser bound than concatenations.
Therefore [2] and [3] match the same strings, but [1] and [2] do not.

~~~{.abnf}
[1] elem (foo / bar) blat
[2] elem foo / bar blat
[3] (elem foo) / (bar blat)
~~~

Use of the alternative operator, freely mixed with concatenations, can be
confusing.
It is recommended that the grouping operator be used to make explicit
concatenation groups (as in [1] and [3]).

## ABNF ABNF

~~~{.abnf}
rulelist       =  1*( rule / (*c-wsp c-nl) )
rule           =  rulename defined-as elements c-nl
                      ; continues if next line starts
                      ;  with white space
rulename       =  ALPHA *(ALPHA / DIGIT / "-")
defined-as     =  *c-wsp ("=" / "=/") *c-wsp
                      ; basic rules definition and
                      ;  incremental alternatives
elements       =  alternation *c-wsp
c-wsp          =  WSP / (c-nl WSP)
c-nl           =  comment / CRLF
                      ; comment or newline
comment        =  ";" *(WSP / VCHAR) CRLF
alternation    =  concatenation
                 *(*c-wsp "/" *c-wsp concatenation)
concatenation  =  repetition *(1*c-wsp repetition)
repetition     =  [repeat] element
repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)
element        =  rulename / group / option /
                 char-val / num-val / prose-val
group          =  "(" *c-wsp alternation *c-wsp ")"
option         =  "[" *c-wsp alternation *c-wsp "]"
char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE
num-val        =  "%" (bin-val / dec-val / hex-val)
bin-val        =  "b" 1*BIT
                  [ 1*("." 1*BIT) / ("-" 1*BIT) ]
                       ; series of concatenated bit values
                       ;  or single ONEOF range
dec-val        =  "d" 1*DIGIT
                  [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
hex-val        =  "x" 1*HEXDIG
                  [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
                       ; bracketed string of SP and VCHAR
                       ;  without angles
                       ; prose description, to be used as
                       ;  last resort
~~~

--

@author Wouter Beek
@see Based on the obsoleted RFC 4234 standard,
     http://tools.ietf.org/html/rfc4234
@see Current version, http://tools.ietf.org/html/rfc5234
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).


/*
alternation -->
  concatenation,
  dcg(dcg('c-wsp'), "/", dcg('c-wsp'), concatenation).

%! 'bin-val'
% ~~~{.abnf}
% bin-val  =  "b" 1*BIT
%             [ 1*("." 1*BIT) / ("-" 1*BIT) ]
% ~~~

%! 'bin-val'(-Code:code)//
% Series of concatenated bit values or single ONEOF range.

% A single binary value.
'bin-val'([N]) -->
  "b",
  dcg('BIT', 1, Bits),
  {digits_to_decimal(Bits, 2 N)}.
% Multiple binary values.
'bin-val'(
  ( dcg((".", dcg('BIT', 1)), 1)
  ; "-", dcg('BIT', 1)
  ; "").

% Comment or newline.
'c-nl' -->
  comment.
'c-nl' -->
  'CRLF'.

'c-wsp' -->
  'WSP'.
'c-wsp' -->
  'c-nl',
  'WSP'.

% Quoted string of SP and VCHAR without DQUOTE.
'char-val'(ci_string(Cs)) -->
  'DQUOTE',
  % *(%x20-21 / %x23-7E)
  dcg(([C], {between(32, 33, C), between(35, 126, C)}), [Cs]),
  'DQUOTE'.

comment -->
  ";",
  dcg(('WSP' ; 'VCHAR')),
  'CRLF'.

concatenation -->
  repetition,
  dcg(dcg('c-wsp', 1), repetition).

'dec-val' -->
  "d",
  dcg('DIGIT', 1),
  ( dcg(".", dcg('DIGIT', 1) ; "-", dcg('DIGIT', 1))
  ; "" ).

% Basic rules definition and incremental alternatives.
'defined-as' -->
  dcg('c-wsp'),
  ("=" ; "=/"),
  dcg('c-wsp').

element(RuleName) -->
  rulename(RuleName).
element -->
  group.
element -->
  option.
element(DCG_Rule) -->
  'char-val'(DCG_Rule).
element -->
  'num-val'.
element -->
  'prose-val'.

elements -->
  alternation,
  dcg('c-wsp').

group -->
  "(",
  dcg('c-wsp'),
  alternation,
  dcg('c-wsp'),
  ")".

'hex-val' -->
  ci_string("x"),
  dcg('HEXDIG', 1),
  ( dcg((".", dcg('HEXDIG', 1) ; "-", dcg('HEXDIG', 1)), 1)
  ; "" ).

'num-val' -->
  "%",
  ('bin-val'
  ; 'dec-val'
  ; 'hex-val').

option -->
  "[",
  dcg('c-wsp'),
  alternation,
  dcg('c-wsp'),
  "]".

% Bracketed string of SP and VCHAR without angles.
% Prose description, to be used as last resort.
'prose-val' -->
  "<",
  % *(%x20-3D / %x3F-7E)
  dcg(([C], ({between(32, 61, C)} ; {between(63, 126, C)}))),
  ">".

%! repeat(-NumberOfRepeats:pair(nonneg,nonneg))//

repeat(Min-_) -->
  dcg('DIGIT', 1, [Ds]),
  {digits_to_decimal(Ds, Min)}.
repeat(Min-Max) -->
  dcg('DIGIT', 1, [D1s]),
  {digits_to_decimal(D1s, Min)}.
  "*",
  dcg('DIGIT', 1, [D2s]),
  {digits_to_decimal(D2s, Max)}.

repetition(dcg(DCG, Repeat)) -->
  (repeat(Repeat) ; ""),
  element(DCG).

%! rule(-DCG_Rule)//

rule(DCG_Rule) -->
  rulename(DCG_Head),
  'defined-as',
  elements(DCG_Body),
  % Continues if next line starts with white space
  'c-nl',
  {DCG_Rule =.. [-->,DCG_Head,DCG_Body]}.

%! rulelist(-DCG_Rules:list)//

rulelist(DCG_Rules) -->
  dcg((rule ; dcg('c-wsp', 'c-nl')), 1, DCG_Rules).

%! rulename(-RuleName:atom)//

rulename(RuleName) -->
  'ALPHA'(H),
  dcg(('ALPHA' ; 'DIGIT' ; "-"), _, T),
  {atom_codes(RuleName, [H|T])}.
*/


%! 'ALPHA'//
% @see 'ALPHA'//1

'ALPHA' -->
  ascii_letter.

%! 'ALPHA'(?Code:code)//
% The ASCII letters, i.e. =|A-Z / a-z|=.
%
% Hexadecimal character range: =|%x41-5A|= through =|%x61-7A|=.
%
% ~~~{.abnf}
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ~~~

'ALPHA'(C) -->
  ascii_letter(C).

%! 'BIT'//
% @see 'BIT'//1

'BIT' -->
  binary_digit.

'BIT'(C) -->
  binary_digit(C).

%! 'BIT'(?Code:code, ?DecimalDigit:between(0,1))//
% A binary digit, i.e. `0` or `1`.
%
% ~~~{.abnf}
% BIT = "0" / "1"
% ~~~

'BIT'(C, D) -->
  binary_digit(C, D).

%! 'CHAR'//
% @see 'CHAR'//1

'CHAR' -->
  [C],
  {between(1, 127, C)}.

%! 'CHAR'(?Code:code)//
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% ~~~{.abnf}
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ~~~

'CHAR'(C) -->
  [C],
  {between(1, 127, C)}.

%! 'CR'//
% @see 'CR'//1

'CR' -->
  carriage_return.

%! 'CR'(?Code:code)//
% The carriage return.
%
% ~~~{.abnf}
% CR = %x0D   ; carriage return
% ~~~

'CR'(C) -->
  carriage_return(C).

%! 'CRLF'//
% @see 'CRLF'//1

'CRLF' -->
  'CRLF'(_Cs).

%! 'CRLF'(?Codes:list(code))//
% Internet standard newline.
%
% ~~~{.abnf}
% CRLF = CR LF   ; Internet standard newline
% ~~~

'CRLF'([C1,C2]) -->
  'CR'(C1),
  'LF'(C2).

%! 'CTL'//
% @see 'CTL'//1

'CTL' -->
  control.

%! 'CTL'(?Code:code)//
% Control characters.
%
% ~~~{.abnf}
% CTL = %x00-1F / %x7F   ; controls
% ~~~

'CTL'(C) -->
  control(C).

%! 'DIGIT'//
% @see 'DIGIT'//2

'DIGIT' -->
  decimal_digit.

'DIGIT'(C) -->
  decimal_digit(C).

%! 'DIGIT'(?Code:code, ?DecimalDigit:integer)//
% Decimal digits.
%
% ~~~{.abnf}
% DIGIT = %x30-39   ; 0-9
% ~~~

'DIGIT'(C, D) -->
  decimal_digit(C, D).

%! 'DQUOTE'//
% @see 'DQUOTE'//1

'DQUOTE' -->
  double_quote.

%! 'DQUOTE'(?Code:code)//
%
% ~~~{.abnf}
% DQUOTE = %x22   ; " (Double Quote)
% ~~~

'DQUOTE'(C) -->
  double_quote(C).

%! 'HEXDIG'//
% @see 'HEXDIG'//1

'HEXDIG' -->
  hexadecimal_digit.

'HEXDIG'(C) -->
  hexadecimal_digit(C).

%! 'HEXDIG'(?Code:code, ?DecimalDigit:between(0,15))//
% Hexadecimal digits.
%
% ~~~{.abnf}
% HEXDIG =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ~~~

'HEXDIG'(C, D) -->
  hexadecimal_digit(C, D).

%! 'HTAB'//
% @see 'HTAB'//1

'HTAB' -->
  horizontal_tab.

%! 'HTAB'(?Code:code)//
%
% ~~~{.abnf}
% HTAB = %x09   ; horizontal tab
% ~~~

'HTAB'(C) -->
  horizontal_tab(C).

%! 'LF'//
% @see 'LF'//1

'LF' -->
  line_feed.

%! 'LF'(?Code:code)//
%
% ~~~{.abnf}
% LF = %x0A   ; linefeed
% ~~~

'LF'(C) -->
  line_feed(C).

%! 'LWSP'//
% @see 'LWSP'//

'LWSP' -->
  'LWSP'(_C).

%! 'LWSP'(?Code:code)//
%
% ~~~{.abnf}
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ~~~

'LWSP'([H|T]) -->
  'WSP'(H),
  'LWSP'(T).
'LWSP'([H|T]) -->
  'CRLF'(H),
  'WSP'(T).
'LWSP'([]) -->
  [].

%! 'OCTET'//
% @see 'OCTET'//1

'OCTET' -->
  'OCTET'(_C).

%! 'OCTET'(?Code:code)//
%
% ~~~{.abnf}
% OCTET = %x00-FF   ; 8 bits of data
% ~~~

'OCTET'(C) -->
  [C],
  {between(0, 255, C)}.

%! 'SP'//
% @see 'SP'//1

'SP' -->
  space.

%! 'SP'(?Code:code)//
%
% ~~~{.abnf}
% SP = %x20
% ~~~

'SP'(C) -->
  space(C).

%! 'VCHAR'//
% @see 'VCHAR'//1

'VCHAR' -->
  ascii_graphic.

%! 'VCHAR'(?Code:code)//
%
% ~~~{.abnf}
% VCHAR = %x21-7E   ; visible (printing) characters
% ~~~

'VCHAR'(C) -->
  ascii_graphic(C).

%! 'WSP'//
% @see 'WSP'//1

'WSP' -->
  'WSP'(_C).

%! 'WSP'(?Code:code)//
%
% ~~~{.abnf}
% WSP = SP / HTAB   ; white space
% ~~~

'WSP'(C) -->
  'SP'(C).
'WSP'(C) -->
  'HTAB'(C).

