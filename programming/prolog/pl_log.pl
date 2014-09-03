:- module(
  pl_log,
  [
    canonical_blobs_atom/2, % @Term
                            % -Atom:atom
    run_collect_messages/1, % :Goal
    run_collect_messages/2, % :Goal
                            % +File:atom
    run_collect_messages/3, % :Goal
                            % -Status:or([oneof([false,true]),compound])
                            % -Messages:list(compound)
    run_print_messages/1, % :Goal
    store_term_to_log/2, % +File:atom
                         % @Term
    write_canonical_blobs/1, % @Term
    write_canonical_blobs/2 % +Stream:stream
                            % @Term
  ]
).

/** <module> Prolog logging

Logging the performance and results of Prolog predicates.

@author Wouter Beek
@version 2014/04, 2014/06, 2014/08
*/

:- use_module(library(check_installation)). % Private predicates.

:- meta_predicate(run_collect_messages(0)).
:- meta_predicate(run_collect_messages(0,+)).
:- meta_predicate(run_collect_messages(0,-,-)).
:- meta_predicate(run_print_messages(0)).



%! canonical_blobs_atom(@Term, -Atom:atom) is det.

canonical_blobs_atom(Term, Atom):-
  with_output_to(atom(Atom), write_canonical_blobs(Term)).


%! replace_blobs(Term0, Term) is det.
% Copy Term0 to Term, replacing non-text blobs.
% This is required for error messages that may hold streams
% and other handles to non-readable objects.

replace_blobs(NIL, NIL):-
  NIL == [], !.
replace_blobs(Blob, Atom):-
  blob(Blob, Type), Type \== text, !,
  format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term):-
  compound(Term0), !,
  compound_name_arguments(Term0, Name, Args0),
  maplist(replace_blobs, Args0, Args),
  compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).


%! run_collect_messages(:Goal) is det.
% Write an abbreviated version of status and messages to the console.

run_collect_messages(Goal):-
  run_collect_messages(Goal, Status, Messages),
  length(Messages, NumberOfMessages),
  format(
    current_output,
    'Status: ~a; #messages: ~D~n',
    [Status,NumberOfMessages]
  ).


%! run_collect_messages(:Goal, +File:atom) is det.
% Write status and messages to file.

run_collect_messages(Goal, File):-
  run_collect_messages(Goal, Status, Messages),
  maplist(store_term_to_log(File), [Status|Messages]).


%! run_collect_messages(
%!   :Goal,
%!   -Status:or([oneof([false,true]),compound]),
%!   -Messages:list(compound)
%! ) is det.
% Return status and messages to the calling context for further processing.

run_collect_messages(Goal, Status, Messages):-
  check_installation:run_collect_messages(Goal, Status, Messages).


%! run_print_messages(:Goal) is det.

run_print_messages(Goal):-
  run_collect_messages(Goal, Status, Warnings),
  print_message(informational, run_print_messages(Status,Warnings)).


%! store_term_to_log(+File:atom, @Term) is det.

store_term_to_log(File, exception(Error)):- !,
  store_term_to_log(File, Error).
store_term_to_log(_, false):- !.
store_term_to_log(_, true):- !.
store_term_to_log(File, Term):-
  setup_call_cleanup(
    open(File, append, Stream),
    with_output_to(Stream, write_canonical_blobs(Term)),
    close(Stream)
  ).


%! write_canonical_blobs(@Term) is det.
% Alteration of write_canonical/[1,2] that lives up to the promise that
% "terms written with this predicate can always be read back".

write_canonical_blobs(Term):-
  write_canonical_blobs(current_output, Term).

%! write_canonical_blobs(+Stream:stream, @Term) is det.

write_canonical_blobs(Stream, Term):-
  replace_blobs(Term, AtomBlobs),
  write_term(Stream, AtomBlobs, [quoted(true)]).



% Messages

:- multifile(prolog:message/1).

prolog:message(run_print_messages(Status,Warnings)) -->
  status(Status),
  warnings(Warnings).

lines([]) --> [].
lines([H|T]) -->
  [H],
  lines(T).

% @tbd Send an email whenever an MD5 fails.
status(false) --> !,
  ['    [STATUS] FALSE',nl].
status(true) --> !.
status(Status) -->
  ['    [STATUS] ~w'-[Status],nl].

warning(message(_,Kind,Lines)) -->
  ['    [MESSAGE(~w)] '-[Kind]],
  lines(Lines),
  [nl].

warnings([]) --> !, [].
warnings([H|T]) -->
  warning(H),
  warnings(T).

