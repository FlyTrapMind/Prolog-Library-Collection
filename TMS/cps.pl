:- module(
  cps,
  [
  ]
).

/** <module> CPS

Classical Problem Solving

@author Wouter Beek
@see Kenneth D. Forbus and Johan de Kleer, 1993, Building Problem Solvers
@version 2013/09
*/

:- use_module(livrary(debug)).
:- use_module(livrary(option)).

:- meta_predicate(bsolve(:,+,-)).
:- meta_predicate(bsolve(:,:,:,+,-)).

:- debug(cps).



bsolve(O1, Initial, Solution):-
  meta_options(is_meta, O1, O2),
  option(goal_recognizer(GoalRecognizer), O2),
  option(print_new_paths(PrintNewPaths), O2),
  option(state_printer(StatePrinter), O2),
  flag('number-examined', _OldId, 0),
  bsolve(GoalRecognizer, PrintNewPaths, StatePrinter, [[Initial]], Solution).

bsolve(GoalRecognizer, _PrintNewPaths, _StatePrinter, [], _NoSolution):- !,
  fail.
bsolve(
  GoalRecognizer,
  _PrintNewPaths,
  StatePrinter,
  [[State|Path]|Paths],
  NumberExamined-[State|Path]
):-
  call(GoalRecognizer, State), !,
  call(StatePrinter, State, StateMsg),
  debug(cps, 'Found goal state: ~w', [StateMsg]),
  flag('number-examined', NumberExamined, NumberExamined).
bsolve(
  GoalRecognizer,
  PrintNewPaths,
  StatePrinter,
  [[State|Path]|Paths],
  Solution
):-
  % DEB: Print the currently considered path
  %      (we only show the path's current state).
  call(StatePrinter, State, StateMsg),
  debug(cps, 'State explored: ~w', [StateMsg]),
  
  % Extend the currently considered path.
  extend_path([State|Path], ExtendedPaths),
  
  % DEB: Print the extended paths.
  debug(cps, 'New operator instances: ', []),
  forall(
    member(ExtendedPath, ExtendedPaths),
    (
      call(PrintNewPath, ExtendedPath, ExtendedPathMsg),
      debug(cps, '\t~w', [ExtendedPathMsg])
    )
  ),
  
  % This is the breadth-first search.
  append(Paths, ExtendedPaths, NewPaths),
  
  % Update statistics.
  flag('number-examined', Id, Id+1),
  
  bsolve(GoalRecognizer, PrintNewPaths, StatePrinter, NewPaths, Solution).

extend_path(Path, ExtendedPaths):-
  

is_meta(goal_recognizer).
is_meta(print_new_paths).
is_meta(state_printer).

