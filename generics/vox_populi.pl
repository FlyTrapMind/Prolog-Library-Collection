:- module(
  vox_populi,
  [
    ask/3 % +ThreadPrefix:atom
          % +Question:compound
          % -Answers:list
  ]
).

/** <module> Vox Populi

@author Wouter Beek
@version 2014/06
*/



%! ask(+ThreadPrefix:atom, +Question:compound, -Answers:list) is det.

ask(ThreadPrefix, Question, Answers):-
  thread_create(ask0(ThreadPrefix, Question, Answers), ThreadId, []),
  thread_join(ThreadId, exited(Answers)).

ask0(ThreadPrefix, Question, Answers):-
  thread_self(Me),
  findall(
    Thread,
    (
      matching_thread(ThreadPrefix, Thread),
      thread_send_message(Thread, question(Me,Question))
    ),
    Threads
  ),
  collect_answers(Threads, Answers),
  thread_exit(Answers).


%! collect_answers(+Threads:list(atom), -Answers:list) is det.

collect_answers(Threads, Answers):-
  collect_answers(Threads, [], Answers).

collect_answers([], Solution, Solution):- !.
collect_answers(Threads1, Answers, Solution):-
  thread_get_message(answer(Thread,Answer)),
  selectchk(Thread, Threads1, Threads2), !,
  collect_answers(Threads2, [Answer|Answers], Solution).
collect_answers(Threads, Answers, Solution):-
  sleep(1),
  collect_answers(Threads, Answers, Solution).


%! matching_thread(+ThreadPrefix:atom, -Thread:atom) is nondet.

matching_thread(ThreadPrefix, Thread):-
  thread_property(Thread, status(_)),
  atom_concat(ThreadPrefix, _, Thread).

