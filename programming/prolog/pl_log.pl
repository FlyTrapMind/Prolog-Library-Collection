:- module(
  pl_log,
  [
    run_collect_messages/3, % :Goal
                            % -Status:or([oneof([false,true]),compound])
                            % -Messages:list(compound)
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
@version 2014/04
*/

:- use_module(library(check_installation)).

:- meta_predicate(run_collect_messages(0,-,-)).



%! run_collect_messages(
%!   :Goal,
%!   -Status:or([oneof([false,true]),compound]),
%!   -Messages:list(compound)
%! ) is det.

run_collect_messages(Goal, Status, Messages):-
  check_installation:run_collect_messages(Goal, Status, Messages).


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

%! replace_blobs(Term0, Term) is det.
% Copy Term0 to Term, replacing non-text blobs.
% This is required for error messages that may hold streams
% and other handles to non-readable objects.

replace_blobs(Blob, Atom):-
  blob(Blob, Type), Type \== text, !,
  format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term):-
  compound(Term0), !,
  compound_name_arguments(Term0, Name, Args0),
  maplist(replace_blobs, Args0, Args),
  compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).

