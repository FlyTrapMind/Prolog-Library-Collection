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
    print_list/2, % +Out
                  % +List:list
    print_list/3 % +Out
                 % +Indent:integer
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
:- use_module(library(memfile)).
:- use_module(library(settings)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).



%! formatnl(+Format) is det.
% @see Variant of format/1 with a newline appended.

formatnl(Format):-
  format(Format),
  current_output(Stream), nl(Stream).

%! formatnl(+Format, :Arguments) is det.
% @see Variant of format/2 with a newline appended.

formatnl(Format, Arguments):-
  format(Format, Arguments),
  current_output(Stream), nl(Stream).

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

%! print_list(+Output, +List:list) is det.
% @see Wrapper predicate for print_list/3, using no indentation.

print_list(Out, List):-
  print_list(Out, 0, List).

%! print_list(+Output, +Indent:integer, +List:list) is det.
% Prints the elements of the given list to the given output stream or handle.
%
% Lists are printed recursively, using indentation relative to the given
% indentation level.
%
% @arg Indent The number of tabs prefixed to each written line.
%
% @tbd Do not explicitly distinguish between codes and atoms output.
%      with_output_to/2 writes characters to both.

print_list(Out, Indent, List):-
  with_output_to(Out, print_list_(Indent, List)).

% Done!
print_list_(_Indent, []):- !.
% Nested lists.
print_list_(Indent, [H | T]):-
  is_list(H), !, NewIndent is Indent + 1,
  print_list_(NewIndent, H),
  print_list_(Indent, T).
% Next list entry.
print_list_(Indent, [H | T]):-
  indent(Indent), write(H), format('\n'),
  print_list_(Indent, T).

% @tbd The predicates that appear below should be unified with some RDF module
%      used for exporting triples and with some TMS module used for exporting
%      justification chains.

print_proposition(Stream, Options, rdf(S, P, O)):-
  maplist(vertex_naming(Options), [S, P, O], [S0, P0, O0]),
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
