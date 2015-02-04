:- module(
  list_script,
  [
    list_script/3 % :Goal_1
                  % +Todo:list
                  % +Options:list(nvpair)
  ]
).

/** <module> List script

List scripting is the practice of running some arbitrary goal on items
that are read in from a list that may be stored in a file.

There are two lists:
  - `Todo.txt`
    Contains all items the goal has to be run on.
  - `DONE.txt`
    Contains only those items for which goal was run at some point
    in the past.

The main method reads both of these files, applies a given goal to
the members of (Todo minus DONE), and adds the processed items to DONE.

The list stored in Todo changes accordingly,
i.e. denoting the remaining items.

To process the same items again one should have copied the orginal Todo list.

@author Wouter Beek
@version 2013/06, 2014/01, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(readutil)).

:- use_module(generics(atom_ext)).

:- predicate_options(list_script/3, 3, [
      message(+atom),
      notdone(-list),
      skip(+nonneg)
   ]).

:- meta_predicate(list_script(1,+,+)).
:- meta_predicate(list_script(1,+,+,+,-,-)).





%! list_script(:Goal_1, +Todo:list(term), +Options:list(nvpair)) is det.
% Processes the items in `Todo` using the given goal
% and places the items either in the `Done` or in the `NotDone` list.
% The `Done` list can be pre-instantiated.
%
% The following options are supported:
%   - message(+atom)
%   - notdone(-list)
%   - skip(+nonneg)

list_script(Goal_1, Todo0, Options):-
  length(Todo0, L),

  % Process option `message`.
  (   option(message(Msg0), Options)
  ->  Msg = Msg0
  ;   Msg = 'Processed'
  ),

  % Process option `skip`.
  (   option(skip(I), Options)
  ->  length(Skip, I),
      append(Skip, Todo, Todo0)
  ;   I = 1,
      Todo = Todo0
  ),

  % Process option `notdone`.
  list_script(Goal_1, Msg, I-L, Todo, Done, NotDone),
  (   option(notdone(NotDone0), Options)
  ->  NotDone0 = NotDone
  ;   true
  ),

  % DEB
  (   debugging(list_script)
  ->  length(Done, L1),
      progress_bar(L1, L, Bar),
      debug(list_script, '[EVAL] ~w: ~w', [Msg,Bar]),
      forall(
        member(X, NotDone),
        debug(list_script, '[NOT-DONE] ~w\n', [X])
      )
  ;   true
  ).

%! list_script(
%!   :Goal_1,
%!   +Message:atom,
%!   +Counter:pair(nonneg),
%!   +Todo:list(term),
%!   -Done:list(term),
%!   -NotDone:list(term)
%! ) is det.

% Nothing `Todo`.
list_script(_, _, L-L, [], [], []):- !.
% Could process a `Todo` item; pushed to `Done`.
list_script(Goal_1, Msg, I1-L, [X|Todo], [X|Done], NotDone):-
  call(Goal_1, X), !,
  % Retrieve the current index, based on the previous index.
  I2 is I1 + 1,
  debug(list_script, '[TODO] ~a ~:D/~:D', [Msg,I2,L]),
  list_script(Goal_1, Msg, I2-L, Todo, Done, NotDone).
% Could not process a `Todo` item; pushed to `NotDone`.
list_script(Goal_1, Msg, I1-L, [X|Todo], Done, [X|NotDone]):-
  I2 is I1 + 1,
  debug(list_script, '[NOT-DONE] ~a ~:D/~:D', [Msg,I2,L]),
  list_script(Goal_1, Msg, I2-L, Todo, Done, NotDone).







