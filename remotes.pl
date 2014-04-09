% Overview of Prolog remotes.
% This intended to be called from within `use_remote_module`.

:- use_module(library(option)).

:- multifile(user:remote/3).

% PLC
user:remote(
  github,
  plc,
  [default(true),repository('Prolog-Library-Collection'),user(wouterbeek)]
).

% PraSem
user:remote(
  github,
  prasem,
  [repository('Pragmatic-Semantics'),user(wouterbeek)]
).



register_remotes(Dir, O1):-
  forall(
    user:remote(Type, RemoteId, O2),
    (
      merge_options(O1, O2, O3),
      register_remote(Type, RemoteId, Dir, O3)
    )
  ).

