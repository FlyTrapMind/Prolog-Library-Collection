:- module(
  print_ext,
  [
    formatnl/1, % +Format
    formatnl/2, % +Format
                % :Arguments
    formatnl/3, % +Output
                % +Format
                % :Arguments
    indent/1, % +Indent:integer
    print_nvpair/2, % :Options:list(nvpair)
                    % +NVPair
    print_collection/2, % :Options:list(nvpair)
                        % +Collection:list
    print_pair/3, % :Options:list(nvpair)
                  % +X
                  % +Y
    print_proof/2, % :Options:list(nvpair)
                   % +Proof:tree
    print_list/2, % :Options:list(nvpair)
                  % +List:list
    print_set/2, % :Options:list(nvpair)
                 % +List:list
    print_tuple/2 % :Options:list(nvpair)
                  % +List:list
  ]
).

/** <module> Print extensions

Predicates for printing.

## Proof

A datatype of the following form:
~~~{.pl}
proof(Conclusion, Premises)
~~~

@author Wouter Beek
@tbd Remove all predicate variants that have an `Out` parameter.
     The calling context should use with_output_to/2 instead.
@version 2013/01-2013/02, 2013/04-2013/05, 2013/07-2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(codes_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- meta_predicate(print_collection(:,+)).
:- meta_predicate(print_list(:,+)).
:- meta_predicate(print_nvpair(:,+)).
:- meta_predicate(print_pair(:,+,+)).
:- meta_predicate(print_proof(:,+)).
:- meta_predicate(print_set(:,+)).
:- meta_predicate(print_tuple(:,+)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).



%! formatnl(+Format) is det.
% @see Variant of format/1 with a newline appended.

formatnl(Format1):-
  term_to_atom(Format1, Format2),
  format(Format2),
  nl.

%! formatnl(+Format, :Arguments) is det.
% @see Variant of format/2 with a newline appended.

formatnl(Format, Arguments):-
  format(Format, Arguments),
  nl.

%! formatnl(+Output, +Format, :Arguments) is det.
% @see Variant of format/3 with a newline appended.

formatnl(Out, Format, Arguments):-
  format(Out, Format, Arguments),
  nl(Out).

%! indent(+Indent:integer) is det.
% @see Like tab/1, but writes the given number of indents, where
%      a single indent can be multiple spaces.

indent(Indent):-
  setting(indent_size, IndentSize),
  NumberOfSpaces is IndentSize * Indent,
  tab(NumberOfSpaces).

is_meta(ordering).
is_meta(write_method).

%! print_collection(+Options:list(nvpair), +Collection:list) is det.
% The following options are supported:
%   1. =|begin(+Begin:atom)|=
%   2. =|end(+End:atom)|=
%   3. =|separator(+Separator:atom)|=
%   4. =|ordering(:Pred)|=
%      The binary predicate that is applied to the collection
%      to determine the order in which its elements occur.
%   5. =|write_method(:Pred)|=
%      The unary predicate that is used for writing the individual items
%      in the collection.

print_collection(O1, Collection1):-
  meta_options(is_meta, O1, O2),
  % E.g., list -> set.
  option(ordering(P), O2, =),
  once(call(P, Collection1, Collection2)),
  % Open a collection.
  option(begin(Begin), O2),
  write(Begin),
  print_collection_(O2, Collection2),
  % End a collection.
  option(end(End), O2),
  write(End).

% Done!
print_collection_(_O1, []):- !.
% Nested collection.
print_collection_(O1, [H1|T]):-
  is_list(H1), !,
  % Notice that set members that are sets may contain multiple occurrences,
  % since they will first be explicitly converted to ordset format.
  option(ordering(P), O1, =),
  once(call(P, H1, H2)),
  print_collection(O1, H2),
  print_collection_(O1, T).
% Next set member.
print_collection_(O1, [H|T]):-
  option(write_method(P), O1, write),
  call(P, H),
  % Do not add the separator after the last set member.
  option(separator(Separator), O1),
  unless(T == [], write(Separator)),
  print_collection_(O1, T).

%! print_list(+Options:list(nvpair), +List:list) is det.
% Prints the elements of the given list to the given output stream or handle.
%
% Lists are printed recursively, using indentation relative to the given
% indentation level.

print_list(O1, List):-
  meta_options(is_meta, O1, O2),
  merge_options(O2, [begin('['),end(']'),separator(',')], O3),
  print_collection(O3, List).

print_nvpair(O1, NVPair):-
  meta_options(is_meta, O1, O2),
  NVPair =.. [N,V],
  merge_options(O2, [begin(''),end(';'),separator(': ')], O3),
  print_collection(O3, [N,V]).

%! print_pair(+Options:list(nvpair), +X, +Y) is det.
% Prints the given pair.
%
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII characters `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

print_pair(O1, X, Y):-
  meta_options(is_meta, O1, O2),

  % The begin and end signs depend on the mode.
  select_option(brackets(Brackets), O2, O3, ascii),
  (
    Brackets == ascii
  ->
    Begin = '<',
    End = '>'
  ;
    Brackets == html
  ->
    Begin = '&lang;',
    End = '&rang;'
  ),

  merge_options(O3, [begin(Begin),end(End),separator(',')], O4),
  print_collection(O4, [X,Y]).

print_proof(O1, Proof):-
  meta_options(is_meta, O1, O2),
  default_option(O2, indent, 0, O3),
  once(phrase(print_proof(O3, Proof), Codes)),
  put_codes(current_output, Codes).

print_proof(O1, Proof) -->
  {Proof =.. [Rule,Premises,Conclusion]},

  % Indentation.
  {update_option(O1, indent, succ, I, O2)},
  indent(I),

  % The name of the rule that was used for deduction.
  "[", atom(Rule), "]",

  % Separator between rule name and conclusion.
  " ",

  % The conclusion.
  print_proposition(O1, Conclusion),
  newline,

  % Print premises / subproofs.
  dcg_multi(print_proof(O2), _, Premises, []).

print_proposition(O1, Proposition) -->
  {option(transformation(Predicate), O1, identity)},
  {call(Predicate, Proposition, Atom)},
  atom(Atom).

identity(X, X).

print_set(O1, List):-
  meta_options(is_meta, O1, O2),
  merge_options(
    O2,
    [begin('{'),end('}'),ordering(list_to_ord_set),separator(',')],
    O3
  ),
  print_collection(O3, List).

%! print_tuple(+Options:list(nvpair), +List:list) is det.
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII signs `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

print_tuple(O1, List):-
  meta_options(is_meta, O1, O2),

  % Set the left and right angles.
  option(brackets(Bracket), O2, ascii),
  (
    Bracket == ascii
  ->
    Begin = '<',
    End = '>'
  ;
    Bracket == html
  ->
    Begin = '&lang;',
    End = '&rang;'
  ),

  merge_options(O2, [begin(Begin),end(End),separator(',')], O3),
  print_collection(O3, List).

