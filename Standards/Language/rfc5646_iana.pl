:- module(
  rfc5646_iana,
  [
    iana_find/1 % :Pattern:dcg
  ]
).

/** <module> RFC5646 IANA

Access the IANA registry for language tags, conforming with RFC 5646.

The IANA Language Subtag Registry contains a complehonsive list of all the
subtags that are valid in language tags.

The registry is a [Unicode] text file and consists of a series of
records in a format based on "record-jar".

## Inconsistency

The IANA file uses linefeeds to end fields. The `record-jar` working note
states that these should be sequences of a carriage return and a linefeed.
The IANA file was changed to be in accordance with the working note's
specification.

@author Wouter Beek
@version 2013//07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(library(pio)).
:- use_module(library(plunit)).



... --> [].
... --> [_], ... .

%! added(-Tree:compound, ?Date:atom)//
% @tbd Parse date subcomponents (existing standard for this?).

added(date(Date), Date) -->
  "Added", value(Date).

comment(comment(Comment), Comment) -->
  "Comments", {gtrace}, value(Comment).

deprecated(deprecated(Date), Date) -->
  "Deprecated", value(Date).

description(Description) -->
  "Description", value(Description).

descriptions(T0, L) -->
  descriptions_(L),
  {parse_tree(descriptions, L, T0)}.

descriptions_([H|T]) -->
  description(H),
  descriptions_(T).
descriptions_([H]) -->
  description(H).

%! iana_find(:Pattern)
% The following patterns are supported:
%   * added//2
%   * comment//2
%   * deprecated//2
%   * descriptions//2
%   * macrolanguage//2
%   * preferred_value//2
%   * prefixes//2
%   * registration//12
%   * scope//2
%   * subtag//2
%   * suppress_script//2
%   * type//2

iana_find(Pattern):-
  absolute_file_name(
    lang(rfc5646_iana_registry),
    File,
    [access(read), file_type(text)]
  ),
  phrase_from_file((..., Pattern, ...), File).

macrolanguage(macrolanguage(Macrolanguage), Macrolanguage) -->
  "Macrolanguage", value(Macrolanguage).

preferred_value(preferred_value(PreferredValue), PreferredValue) -->
  "Preferred-Value", value(PreferredValue).

prefix(Prefix) -->
  "Prefix", value(Prefix).

prefix(prefix(Prefix), Prefix) -->
  "Prefix", value(Prefix).

prefixes(T0, L) -->
  prefixes_(L),
  {parse_tree(prefixes, L, T0)}.

prefixes_([H|T]) -->
  prefix(H),
  prefixes_(T).
prefixes_([H]) -->
  prefix(H).

registration(
  T0,
  Type,
  Subtag,
  Descriptions,
  Added,
  SuppressScript,
  Scope,
  Prefixes,
  Macrolanguage,
  Comment,
  Deprecated,
  PreferredValue
) -->
  type(T1, Type),
  subtag(T2, Subtag),
  descriptions(T3, Descriptions),
  added(T4, Added),
  (suppress_script(T5, SuppressScript) ; ""),
  (scope(T6, Scope) ; ""),
  (prefixes(T7, Prefixes) ; ""),
  (macrolanguage(T8, Macrolanguage) ; ""),
  (comment(T9, Comment) ; ""),
  (deprecated(T10, Deprecated) ; ""),
  (preferred_value(T11, PreferredValue) ; ""),
  {parse_tree(registration, [T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], T0)}.

scope(scope(Scope), Scope) -->
  "Scope", value(Scope).

subtag(subtag(Subtag), Subtag) -->
  "Subtag", value(Subtag).

suppress_script(suppress_script(SuppressScript), SuppressScript) -->
  "Suppress-Script", value(SuppressScript).

%! type(-Tree:compound, ?Type:atom)//
% The type of an IANA language tag registration.
%
% The following types occur:
%   * `extlang`
%   * `grandfathered`
%   * `language`
%   * `redundant`
%   * `region`
%   * `script`
%   * `variant`

type(type(Type), Type) -->
  "Type", value(Type).

%! value(-Value:atom)//

value(Value) -->
  value_first(H),
  value_rest(T),
  {atomic_list_concat([H|T], ' ', Value)}.

value_(Value) -->
  dcg_until([end_mode(inclusive),output_format(atom)], end_of_line, Value).

value_first(Value) -->
  colon, space, value_(Value).

value_rest(Values) -->
  dcg_multi(value_rest_, _N, Values).

value_rest_(Value) -->
  space, space, value_(Value).



:- begin_tests(rfc5646_iana).

:- use_module(generics(print_ext)).
%:- use_module(library(apply)).

test(rfc5646_find, []):- %[true(Length == 8703)]):-
  % There should be 8703 IANA registrations.
  %findall(
  %  Tree,
  %  (
once((
      iana_find(
        registration(
          Tree,
          _Type,
          _Subtag,
          _Descriptions,
          _Added,
          _SuppressScript,
          _Scope,
          _Prefixes,
          _Macrolanguage,
          _Comment,
          _Deprecated,
          _PreferredValue
        )
      ),
      formatnl(Tree)
)).
%    ),
%    Trees
%  ),
%  length(Trees, Length),
%  formatnl(Length).

:- end_tests(rfc5646_iana).

