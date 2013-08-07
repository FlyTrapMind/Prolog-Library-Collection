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
    print_list/2, % +Out
                  % +List:list
    print_list/3, % +Options:list(nvpair)
                  % +Out
                  % +List:list
    print_set/2, % +Out
                 % +List:list
    print_set/3 % +Options:list(nvpair)
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
@version 2013/01-2013/02, 2013/04-2013/05, 2013/07
*/

:- use_module(generics(atom_ext)). % Meta-calls.
:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(apply)).
:- use_module(library(memfile)).
:- use_module(library(settings)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).

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

%! print_collection(+Options:list(nvpair), +Collection:list) is det.
% @param Options The following options are supported:
%      1. `begin(+Begin:atom)`
%      2. `end(+End:atom)`
%      3. `separator(+Separator:atom)`
%      4. `transformation(:Pred)`
%         The binary predicate that is applied to the collection.

print_collection(O, Collection1):-
  % E.g., list -> set.
  option(transformation(P), O, =),
  once(call(P, Collection1, Collection2)),
  % Open a set.
  option(begin(Begin), O),
  write(Begin),
  print_collection_(O, Collection2).

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
  write(H),
  % Do not add the separator after the last set member.
  option(separator(Separator), O),
  unless(T == [], write(Separator)),
  print_collection_(O, T).

print_nvpair(NVPair):-
  NVPair =.. [Name, Value],
  write(Name), write(': '), write(Value), write(';').

print_nvpair(Out, NVPair):-
  with_output_to(Out, print_nvpair(NVPair)).

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

print_set(Out, List):-
  print_set([], Out, List).

print_set(O1, Out, List):-
  merge_options(
    O1,
    [
      begin('{'),
      end('}'),
      separator(','),
      transformation(ordsets:list_to_ord_set)
    ],
    O2
  ),
  with_output_to(Out, print_collection(O2, List)).



% @tbd The predicates that appear below should be unified with some RDF module
%      used for exporting triples and with some TMS module used for exporting
%      justification chains.

print_proposition(Stream, Options, rdf(S, P, O)):-
  maplist(rdf_term_name(Options), [S, P, O], [S0, P0, O0]),
  option(indent(Indent), Options, 0),
  option(index(Index), Options, 'c'),
  indent(Stream, Indent),
  format(Stream, '[~w] ~w ~w ~w\n', [Index, S0, P0, O0]).

print_proposition0(Stream, Options, Proposition):-
  print_proposition(Stream, Options, Proposition), !.
print_proposition0(Stream, Options, Proposition):-
  option(indent(Indent), Options, 0),
  option(index(Index), Options, c),
  indent(Stream, Indent),
  format(Stream, '[~w]:\t~w', [Index, Proposition]).

print_proof(_Stream, _Options, []).
print_proof(Stream, Options, [proof(Conclusion, Premises) | Proofs]):-
  print_proposition0(Stream, Options, Conclusion),
  select_option(indent(Indent), Options, Options0),
  succ(Indent, NewIndent),
  select_option(index(Index), Options0, Options1, 1),
  succ(Index, NewIndex),
  print_proof(Stream, [indent(NewIndent), index(1) | Options1], Premises),
  print_proof(Stream, [indent(Indent), index(NewIndex) | Options1], Proofs).
