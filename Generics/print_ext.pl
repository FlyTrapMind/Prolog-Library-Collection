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
    indent/2, % +Out
              % +Indent:integer
    print_nvpair/1, % +NVPair
    print_nvpair/2, % +Out
                    % +NVPair
    print_collection/2, % +Options:list(nvpair)
                        % +Collection:list
    print_pair/1, % Pair:nvpair
    print_proof/2, % ?Out
                   % +Proof:tree
    print_proof/3, % :Options:list(nvpair)
                   % ?Out
                   % +Proof:tree
    print_list/2, % +Out
                  % +List:list
    print_list/3, % +Options:list(nvpair)
                  % +Out
                  % +List:list
    print_set/2, % +Out
                 % +List:list
    print_set/3, % +Options:list(nvpair)
                 % +Out
                 % +List:list
    print_tuple/2, % +Out
                   % +List:list
    print_tuple/3 % +Options:list(nvpair)
                  % +Out
                  % +List:list
  ]
).

/** <module> PRINT

Predicates for printing.

# Proof

A datatype of the following form:
~~~{.pl}
proof(Conclusion, Premises)
~~~

@author Wouter Beek
@tbd Remove all predicate variants that have an `Out` parameter.
     The calling context should use with_output_to/2 instead.
@version 2013/01-2013/02, 2013/04-2013/05, 2013/07-2013/08
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(atom_ext)). % Meta-calls.
:- use_module(generics(codes_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- meta_predicate(print_proof(:,?,+)).

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

%! indent(+Stream, +Indent:integer) is det.
% @see Like tab/2, but writes the given number of indents, where
%      a single indent can be multiple spaces.

indent(Stream, Indent):-
  setting(indent_size, IndentSize),
  NumberOfSpaces is IndentSize * Indent,
  tab(Stream, NumberOfSpaces).

is_meta(transformation).
is_meta(write_method).

%! print_collection(+Options:list(nvpair), +Collection:list) is det.
% The following options are supported:
%   1. =|begin(+Begin:atom)|=
%   2. =|end(+End:atom)|=
%   3. =|separator(+Separator:atom)|=
%   4. =|transformation(:Pred)|=
%      The binary predicate that is applied to the collection.
%   5. =|write_method(:Pred)|=
%      The unary predicate that is used for writing the individual items
%      in the collection.

print_collection(O1, Collection1):-
  meta_options(is_meta, O1, O2),
  % E.g., list -> set.
  option(transformation(P), O2, =),
  once(call(P, Collection1, Collection2)),
  % Open a set.
  option(begin(Begin), O2),
  write(Begin),
  print_collection_(O2, Collection2).

% Done!
print_collection_(O, []):- !,
  option(end(End), O),
  write(End).
% Nested set.
print_collection_(O, [H1|T]):-
  is_list(H1), !,
  % Notice that set members that are sets may contain multiple occurrences,
  % since they will first be explicitly converted to ordset format.
  option(transformation(P), O, =),
  once(call(P, H1, H2)),
  print_collection(O, H2),
  print_collection_(O, T).
% Next set member.
print_collection_(O, [H|T]):-
  option(write_method(P), O, write),
  call(P, H),
  % Do not add the separator after the last set member.
  option(separator(Separator), O),
  unless(T == [], write(Separator)),
  print_collection_(O, T).

%! print_list(+Output, +List:list) is det.
% Prints the elements of the given list to the given output stream or handle.
%
% Lists are printed recursively, using indentation relative to the given
% indentation level.

print_list(Out, List):-
  print_list([], Out, List).

print_list(O1, Out, List):-
  merge_options(O1, [begin('['),end(']'),separator(',')], O2),
  with_output_to(Out, print_collection(O2, List)).

print_nvpair(NVPair):-
  NVPair =.. [Name, Value],
  write(Name), write(': '), write(Value), write(';').

print_nvpair(Out, NVPair):-
  with_output_to(Out, print_nvpair(NVPair)).

print_pair(Pair):-
  (
    Pair = Name-Value, !
  ;
    Pair =.. [Name,Value]
  ),
  write('<'),
  write(Name),
  write(','),
  write(Value),
  write('>').

print_proof(Out, Proof):-
  print_proof([], Out, Proof).

print_proof(O1, Out, Proof):-
  meta_options(is_meta, O1, O2),
  default_option(O2, indent, 0, O3),
  once(phrase(print_proof(O3, Proof), Codes)),
  put_codes(Out, Codes).

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

print_set(Out, List):-
  print_set([], Out, List).

print_set(O1, Out, List):-
  merge_options(
    O1,
    [begin('{'),end('}'),ordering(ordsets:list_to_ord_set),separator(',')],
    O2
  ),
  with_output_to(Out, print_collection(O2, List)).

print_tuple(Out, List):-
  print_tuple([], Out, List).

print_tuple(O1, Out, List):-
  merge_options(O1, [begin('<'),end('>'),separator(',')], O2),
  with_output_to(Out, print_collection(O2, List)).

