:- module(
  rfc2616_message_header,
  [
    'message-header'//2 % -ParseTree:compound
                        % ?MessageHeader
  ]
).

/** <module> RFC 2616 message header

DCG for generic message headers in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic)).



%! 'field-content'(-ParseTree:compound)//
% # Syntax
%
% ~~~{.abnf}
% field-content = <the OCTETs making up the field-value and consisting of
%                 either *TEXT or combinations of token, separators,
%                 and quoted-string>
% ~~~
%
% ## White-space
%
% The `field-content` does not include any leading or trailing `LWS`:
%  linear white space occurring before the first non-whitespace character
%  of the `field-value` or after the last non-whitespace character
%  of the `field-value`.
% Such leading or trailing `LWS` MAY be removed without changing
%  the semantics of the field value.
% Any `LWS` that occurs between `field-content` MAY be replaced with
%  a single `SP` before interpreting the field value or forwarding
%  the message downstream.
%
% @tbd This BNF rules is **very** unclear!

'field-content'('field-content'(FieldContent), [FieldContent]) -->
  dcg_atom_codes('*'('TEXT'), FieldContent).
'field-content'(T0, FieldContent) -->
  '*'('field-content1', Ts, FieldContent),
  '*'(separator),
  {parse_tree('field-content', Ts, T0)}.

'field-content1'(FieldContent, FieldContent) -->
  '*'(separator),
  token(FieldContent).
'field-content1'(FieldContent, FieldContent) -->
  '*'(separator),
  'quoted-string'(FieldContent).


%! 'field-name'(-ParseTree:compound, ?FieldName:atom)//
% ~~~{.abnf}
% field-name = token
% ~~~

'field-name'('field-name'(FieldName), FieldName) -->
  token(FieldName).


%! 'field-value'(-ParseTree:compound, ?FieldValue:atom)//
% ~~~{.abnf}
% field-value = *( field-content | LWS )
% ~~~

'field-value'(T0, FieldContents) -->
  '*'('field-value1', Ts, FieldContents),
  '*'('LWS'),
  {parse_tree('field-value', Ts, T0)}.

'field-value1'(T1, FieldContents) -->
   '*'('LWS'),
  'field-content'(T1, FieldContents).


%! 'message-header'(-ParseTree:compound, ?MessageHeader)//
% HTTP header fields, which include `general-header`, `request-header`,
%  `response-header`, and `entity-header` fields,
%  follow the same generic format as that given in Section 3.1 of RFC 822.
%
% # Syntax
%
% Each header field consists of a name followed by a colon
%  and the field value.
%
% ~~~{.abnf}
% message-header = field-name ":" [ field-value ]
% ~~~
%
% ## Case-sensitivity
%
% Field names are case-insensitive.
%
% ## White-space
%
% The field value MAY be preceded by any amount of `LWS`,
%  though a single `SP` is preferred.
%
% ## Multi-line fields
%
% Header fields can be extended over multiple lines
%  by preceding each extra line with at least one `SP` or `HT`.
%
% # Pragmatics
%
% Applications ought to follow "common form", where one is known or indicated,
%  when generating HTTP constructs, since there might exist
%  some implementations that fail to accept anything beyond the common forms.
%
% ## Order
%
% The order in which header fields with differing field names are received
%  is not significant.
% However, it is "good practice" to send general-header fields first,
%  followed by `request-header` or `response-header` fields,
%  and ending with the `entity-header` fields.
%
% ## Multiple fields with the same name
%
% Multiple `message-header` fields with the same `field-name` MAY be present
%  in a message if and only if the entire `field-value` for that header field
%  is defined as a comma-separated list [i.e., `#(values)`].
% It MUST be possible to combine the multiple header fields into one
%  `"field-name: field-value"` pair, without changing the semantics
%  of the message, by appending each subsequent `field-value` to the first,
%  each separated by a comma.
% The order in which header fields with the same `field-name` are received
%  is therefore significant to the interpretation of the combined field value,
%  and thus a proxy MUST NOT change the order of these field values
%  when a message is forwarded.
%
% @see RFC 822

'message-header'(T0, Name-Value) -->
  'field-name'(T1, Name),
  ":",
  (
    'field-value'(T2, Value)
  ;
    ""
  ),
  {parse_tree('message-header', [T1,T2], T0)}.

