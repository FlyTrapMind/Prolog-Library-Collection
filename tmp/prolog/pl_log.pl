:- module(
  pl_log,
  [
    canonical_blobs_atom/2, % @Term
                            % -Atom:atom
    run_collect_messages/1, % :Goal
    run_collect_messages/3, % :Goal
                            % -Status:or([oneof([false,true]),compound])
                            % -Messages:list(compound)
    write_canonical_blobs/1, % @Term
    write_canonical_blobs/2 % +Out:stream
                            % @Term
  ]
).

/** <module> Prolog logging

Logging the performance and results of Prolog predicates.

@author Wouter Beek
@version 2014/04, 2014/06, 2014/08-2014/09, 2014/12, 2015/03
*/

:- use_module(library(check_installation)). % Private predicates.
:- use_module(library(debug)).

:- meta_predicate(run_collect_messages(0)).
:- meta_predicate(run_collect_messages(0,-,-)).





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



%! run_collect_messages(:Goal) .
% Write an abbreviated version of status and messages to the console.

run_collect_messages(Goal):-
  run_collect_messages(Goal, Status, Warnings),
  process_warnings(Warnings),
  process_status(Status).



%! run_collect_messages(
%!   :Goal,
%!   -Status:or([oneof([false,true]),compound]),
%!   -Messages:list(compound)
%! ) is det.
% Return status and messages to the calling context for further processing.

run_collect_messages(Goal, Status, Messages):-
  check_installation:run_collect_messages(Goal, Status, Messages).



%! write_canonical_blobs(@Term) is det.
% Alteration of write_canonical/[1,2] that lives up to the promise that
% "terms written with this predicate can always be read back".

write_canonical_blobs(Term):-
  write_canonical_blobs(current_output, Term).



%! write_canonical_blobs(+Stream:stream, @Term) is det.

write_canonical_blobs(Stream, Term):-
  replace_blobs(Term, AtomBlobs),
  write_term(Stream, AtomBlobs, [quoted(true)]).





% HELPERS %

process_status(true):- !.
process_status(fail):- !,
  fail.
process_status(Exception):-
  print_message(warning, Exception).



process_warnings(Warnings):-
  debugging(pl_log), !,
  forall(
    member(Warning, Warnings),
    debug(pl_log, '[WARNING] ~a', [Warning])
  ).
process_warnings(Warnings):-
  length(Warnings, NumberOfWarnings),
  (   NumberOfWarnings =:= 0
  ->  true
  ;   print_message(warnings, number_of_warnings(NumberOfWarnings))
  ).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(number_of_warnings(NumberOfWarnings)) -->
  ['~D warnings'-[NumberOfWarnings]].
