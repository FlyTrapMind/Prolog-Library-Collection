:- module(
  rfc2616_generics,
  [
    charset//2, % -ParseTree:compound
                % ?Charset:atom
    comment//2, % -ParseTree:compound
                % ?Codes:list(code)
    parameter//2, % -ParseTree:compound
                  % ?Parameter:pair(atom,atom)
    'quoted-string'//1, % ?Codes:list(code)
    separator//0,
    token//1 % ?Token:atom
  ]
).

/** <module> HTTP: generics

Some basic DCG rules that are too specific to be reused outside of
  the HTTP specification, but that are too generic to be included in
  the main DCGs.

@author Wouter Beek
@see RFC 2616
@tbd Redundant `LWS`
@tbd Case sensitivity
@tbd Backwards compatibility
@version 2013/12, 2014/10
*/

:- use_module(generics(codes_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(parse_tree)).
:- use_module(plDcg_rfc(rfc2616_basic)).



%! attribute(-ParseTree:compound, ?Attribute:atom)//
% ~~~{.abnf}
% attribute = token
% ~~~

attribute(attribute(Attribute), Attribute) -->
  token(Attribute).



%! charset(-ParseTree:compound, ?Charset:atom)//
% HTTP character sets are identified by case-insensitive tokens.
%
% # Syntax
%
% HTTP character sets are identified by case-insensitive tokens.
%
% ~~~{.abnf}
% charset = token
% ~~~
%
% # Semantics
%
% The term **character set** is used to refer to a method used with
%  one or more tables to convert a sequence of octets into a sequence of
%  characters.
%
% Note that unconditional conversion in the other direction is not required,
%  in that not all characters may be available in a given character set and
%  a character set may provide more than one sequence of octets
%  to represent a particular character.
%
% # Pragmatics
%
% This definition is intended to allow various kinds of character encoding,
%  from simple single-table mappings such as US-ASCII to
%  complex table switching methods such as those that use ISO-2022's
%  techniques.
% However, the definition associated with a MIME character set name
%  MUST fully specify the mapping to be performed from octets to characters.
% In particular, use of external profiling information to determine
%  the exact mapping is not permitted.
%
% ## IANA
%
% The complete set of tokens is defined by the IANA Character Set registry.
%
% Although HTTP allows an arbitrary token to be used as a charset value,
%  any token that has a predefined value within the IANA Character Set
%  registry MUST represent the character set defined by that registry.
% Applications SHOULD limit their use of character sets to those defined
%  by the IANA registry.
%
% @see IANA Character Set registry
%
% ## Compatibility
%
% Some HTTP/1.0 software has interpreted a Content-Type header without
%  charset parameter incorrectly to mean "recipient should guess."
% Senders wishing to defeat this behavior MAY include a charset parameter
%  even when the charset is ISO-8859-1 and SHOULD do so when it is known
%  that it will not confuse the recipient.
%
% Unfortunately, some older HTTP/1.0 clients did not deal properly with
%  an explicit charset parameter. HTTP/1.1 recipients MUST respect the
%  charset label provided by the sender; and those user agents that have
%  a provision to "guess" a charset MUST use the charset from the
%  content-type field if they support that charset, rather than the
%  recipient's preference, when initially displaying a document.
%
% # Terminology
%
% Note: This use of the term "character set" is more commonly referred to
%  as a "character encoding."
% However, since HTTP and MIME share the same registry,
%  it is important that the terminology also be shared.
%
% --
%
% @see Implementors should be aware of IETF character set requirements.

charset(charset(Charset), Charset) -->
  token(Charset).



%! comment(-ParseTree:compound, ?Codes:list(code))//
% Comments can be included in some HTTP header fields by surrounding
%  the comment text with parentheses.
% Comments are only allowed in fields containing `"comment"` as part of
%  their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.abnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~

comment(comment, Comment) -->
  dcg_atom_codes(
    bracketed(
      '*'(comment0, Codes, [])
    ),
    Comment
  ).

comment0([H|T]) -->
  ctext(H),
  comment(T).
comment0([H|T]) -->
  'quoted-pair'(H),
  comment(T).
comment0(L) -->
  comment(_, L).



%! ctext(?Code:code)//
% Maybe this stands for 'comment text'.
%
% ~~~{.abnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~
%
% Notice that the round brackets are used to indicate
%  the begin and end of an HTTP comment.
%
% @see RFC 2616

ctext(Code) -->
  'TEXT'(Code),
  {\+ round_bracket(Code)}.



%! parameter(-ParseTree:compound, ?AttributeValuePair:kvpair(atom,atom))//
% Parameters are in the form of attribute/value pairs.
% ~~~{.abnf}
% parameter = attribute "=" value
% ~~~

parameter(paramter(T1,T2), Attribute-Value) -->
  attribute(T1, Attribute),
  "=",
  value(T2, Value).



%! qdtext(?Code:code)//
% ~~~{.abnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(Code) -->
  'TEXT'(Code),
  {\+ less_than_sign(Code)}.



%! 'quoted-pair'(?Code:code)//
% The backslash character MAY be used as a single-character quoting mechanism
%  only within `quoted-string` and `comment` constructs.
%
% ~~~{.abnf}
% quoted-pair = "\" CHAR
% ~~~
%
% @see RFC 2616

'quoted-pair'(Code) -->
  "\\",
  'CHAR'(Code).



%! 'quoted-string'(?QuotedString:atom)//
% A string of text is parsed as a single word if it is quoted using
%  double-quote marks.
%
% ~~~{.abnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~
%
% @see RFC 2616

'quoted-string'(QuotedString) -->
  quoted(dcg_atom_codes('quoted-string_codes', QuotedString)).

'quoted-string_codes'(Codes) -->
  qdtext(Codes).
'quoted-string_codes'(Codes) -->
  'quoted-pair'(Codes).



%! separator//
% Many HTTP/1.1 header field values consist of words separated by `LWS`
%  or special characters.
% These special characters MUST be in a `quoted-string`
%  to be used within a parameter value.
%
% ~~~{.abnf}
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ~~~
%
% @see RFC 2616

separator --> bracket. % 40,41,91,93,123,125
separator --> ">". % 62
separator --> "<". % 60
separator --> "@". % 64
separator --> ",". %44
separator --> ";". % 59
separator --> ":". % 58
separator --> "\\". % 92
separator --> '"'. %34
separator --> "/". % 47, 92
separator --> "?". % 63
separator --> "=". % 61
separator --> 'SP'. % 32
separator --> 'HT'. % 9



%! token(?Token:atom)//
% ~~~{.abnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~
%
% @see RFC 2616

token(Token) -->
  dcg_atom_codes(token_codes, Token).

token_codes(Codes) -->
  '+'(token_code, Codes, []).

token_code(C) -->
  'CHAR'(C),
  {
    \+ 'CTL'(C, _, _),
    \+ separator(C, _, _)
  }.



%! value(-ParseTree:compound, ?Value:atom)//
% ~~~{.abnf}
% value     = token | quoted-string
% ~~~

value(value(Value), Value) -->
  token(Value).
value(value(Value), Value) -->
  'quoted-string'(Value).

