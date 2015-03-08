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
    report_on_process/2, % +Message:atom
                         % :Goal
    tab/0,
    writeln/2 % +Stream:stream
              % +Term
  ]
).

/** <module> Print extensions

Predicates for printing.

@author Wouter Beek
@version 2013/01-2013/02, 2013/04-2013/05, 2013/07-2013/09, 2013/11, 2014/11,
         2015/03
*/

:- use_module(library(settings)).

:- meta_predicate(report_on_process(+,0)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).

:- setting(
  screen_width,
  integer,
  80,
  'The default width of the screen in number of characters.'
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
%      See setting `indent_size`.

indent(Indent):-
  setting(indent_size, IndentSize),
  NumberOfSpaces is IndentSize * Indent,
  tab(NumberOfSpaces).



%! report_on_process(+Message:atom, :Goal) is det.

report_on_process(Msg, Goal):-
  setup_call_catcher_cleanup(
    print_message(informational, start_process(Msg)),
    Goal,
    Exception,
    (   Exception == true
    ->  print_message(informational, end_process)
    ;   print_message(warning, end_process(Exception))
    )
  ).



%! tab.

tab:-
  write('\t').



%! writeln(+Stream:stream, +Term:term) is det.

writeln(Stream, Term):-
  write(Stream, Term),
  nl(Stream).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(end_process) -->
  ['done.'].

prolog:message(end_process(Exception)) -->
  [Exception].

prolog:message(start_process(Msg)) -->
  [Msg].
